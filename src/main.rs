use chrono::prelude::DateTime;
use chrono::Local;
use chrono::{Datelike, Timelike, Utc};
use clap::Parser;
use indicatif::{HumanBytes, ProgressBar, ProgressStyle};
use regex::Regex;
use reqwest::Url;
use serde::Deserialize;
use serde_json::Value;
use which::which;

use inquire::formatter::MultiOptionFormatter;
use inquire::{Confirm, MultiSelect};
use std::collections::HashMap;
use std::io::Write;
use std::os::unix::fs::MetadataExt;
use std::process::Command;
use std::thread::sleep;
use std::time::{Duration, UNIX_EPOCH};
use std::{fs, io};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    // Query (SMWCentral ID or URL)
    #[arg(short, long)]
    query: String,

    #[arg(short, long)]
    album: String,

    #[arg(short="ca", long)]
    coverart: String
}

#[derive(Deserialize, Debug)]
struct SMWCUser {
    id: u16,
    name: String
}

#[derive(Deserialize, Debug)]
struct SMWCAudioFields {
    size: String,
    #[serde(rename = "type")]
    variant: String,
    samples: String,
    source: String,
    duration: String,
    featured: bool,
    description: String
}

#[derive(Debug, Deserialize)]
struct SMWCFile {
    id: u16,
    section: String,
    name: String,
    time: u64,
    authors: Vec<SMWCUser>,
    submitter: Option<SMWCUser>,
    tags: Vec<String>,
    images: Option<Vec<String>>,
    rating: Option<u8>,
    size: u32,
    downloads: u16,
    download_url: String,
    obsoleted_by: Option<u16>,
    // fields: SMWCAudioFields, <- unnecessary
    raw_fields: SMWCAudioFields
}

const TINY_CAPS_MAPPING: [char; 26] = ['ᴀ', 'ʙ', 'ᴄ', 'ᴅ', 'ᴇ', 'ғ', 'ɢ', 'ʜ', 'ɪ', 'ᴊ', 'ᴋ', 'ʟ', 'ᴍ', 'ɴ', 'ᴏ', 'ᴘ', 'ꞯ', 'ʀ', 's', 'ᴛ', 'ᴜ', 'ᴠ', 'ᴡ', 'x', 'ʏ', 'ᴢ'];


/// Serde JSON deserialiser to capture anything as a string (* -> String)
fn de_unistr<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: serde::Deserializer<'de>,
{
    // Deserialize into blanket serde_json::Value then case-match
    let value = Value::deserialize(deserializer)?;
    Ok(match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => s,
        Value::Array(arr) => format!("{:?}", arr),
        Value::Object(o) => format!("{:?}", o),
    })
}


/// Parses Unix time to human-readable time object + timezone difference.
fn unix_to_hrtime(unix_off: u64) -> DateTime<Utc> {
    let tz_off = Local::now().offset().local_minus_utc();
    let unix_time = UNIX_EPOCH + Duration::from_secs((unix_off as i64 + tz_off as i64) as u64);
    DateTime::<Utc>::from(unix_time)
}

/// Produces "quantity" string via filling with provided active/inactive chars.
fn q_str(active_c: &char, inactive_c: &char, active_q: u8, total_q: u8) -> String {
    let mut res = String::with_capacity(total_q as usize);
    res.push_str(&*active_c.to_string().repeat(active_q as usize));
    res.push_str(&*inactive_c.to_string().repeat((total_q - active_q) as usize));
    res
}

/// Strips "HTML" (html string) to its string equivalent.
/// Tentatively <br>\r\n → \n and condenses consecutive \n into one; unless SMWc's "safe HTML" has other HTML-specific symbols.
fn strip_html(h_str: &str) -> String {
    let res = h_str.replace("<br>\\r\\n", "\n");
    let rgx = Regex::new("r(\\n)+").unwrap();
    rgx.replace_all(&*res, "\n").to_string()
}

/// Converts alphabetical vector of mappings to map.
fn alphavec_to_map(alphavec: [char; 26]) -> HashMap<char, char> {
    ('a'..='z').zip(alphavec.iter().copied()).collect()
}

/// Clamps string to specified max length.
fn strclamp(str: &str, max_len: usize) -> String {
    let clamped_lines = str.len() / max_len;
    let mut res = String::with_capacity(str.len() + clamped_lines);

    for i in 0..=clamped_lines {
        res.push_str(&str[i * max_len..std::cmp::min(str.len(), (i + 1) * max_len)]);
        res.push('\n');
    }

    res
}

/// Applies the provided character mapper, leaving as-is non-defined mappings.
fn alphamap(str: &str, mapper: &HashMap<char, char>) -> String {
    str
        .chars()
        .map(|c| mapper.get(&c).copied().unwrap_or(c))
        .collect()
}

/// Downloads file at specified URL and updates provided indicatif bar. Specific to this project (s2w).
fn s2w_download(url: &str, dest: &str, client: &reqwest::blocking::Client, size: u64) {
    let resp = client.get(url).send().unwrap();
    let mut reader = resp.bytes().unwrap();
    let mut file = fs::File::create(dest).unwrap();

    let bar = ProgressBar::new(size);
    bar.set_style(ProgressStyle::with_template("{bar:83} {percent:0}% ({bytes}/{total_bytes})")
        .unwrap()
        .progress_chars("█▒░"));


    let mut dl_bytes = 0;
    for chunk in reader.chunks(1024) {
        file.write_all(&chunk).unwrap();
        dl_bytes += chunk.len() as u64;
        bar.set_position(dl_bytes);
        sleep(Duration::from_millis(4)); // TODO: Most of these files are <50kb, so add a *very* tiny delay for user gratification! This theoretically shouldn't cause any problematic (artificial) waiting, but if making this function generic, stay wary of arbitrary dl size.
    }

    bar.finish_and_clear();
}

/// Extracts files at provided location and updates indicatif bar. Specific to this project (s2w; only keeps .spc).
fn s2w_extract(loc: &str) {
    // Modified from "zip" crate example https://github.com/zip-rs/zip2/blob/7c20fa30016301909bf2ade203cb4841b7776154/examples/extract.rs

    let archive_file = fs::File::open(loc).unwrap();
    let mut archive = zip::ZipArchive::new(archive_file).unwrap();

    let bar = ProgressBar::new(archive.len() as u64);
    bar.set_style(ProgressStyle::with_template("{bar:83} {percent:0}% ({pos}/{len})")
        .unwrap()
        .progress_chars("█▒░"));

    for i in 0..archive.len() {
        bar.inc(1);

        let mut file = archive.by_index(i).unwrap();

        // Validate path (skip pass if invalid)
        let fpath = match file.enclosed_name() {
            Some(path) => path,
            None => continue
        };

        // Check file extension (skip pass if not .spc — this implicitly removes directories!)
        if fpath.extension().and_then(|e| e.to_str()).is_some_and(|e| e == "spc") {
            // Yank file to base directory and write file
            let bpath = fpath.file_name().unwrap();
            let mut outfile = fs::File::create(&bpath).unwrap();
            io::copy(&mut file, &mut outfile).unwrap();
        } else {
            sleep(Duration::from_millis(20));
        }
    }

    // Delete zip file
    fs::remove_file(loc).unwrap();

    bar.finish_and_clear();
}

/// Converts specified .spc file to .wav using spc2wav utility and updates indicatif bar. Specific to this project (s2w).
fn s2w_conv(loc: &str) {

    let bar = ProgressBar::new(1);
    bar.set_style(ProgressStyle::with_template("{bar:83} {percent:0}% ({pos}/{len})")
        .unwrap()
        .progress_chars("█▒░"));

    Command::new("spc2wav")
        .arg(loc)
        .output()
        .expect("Violation of: spc2wav could not be run (is it installed?)");

    bar.inc(1);
    sleep(Duration::from_millis(20));
    bar.finish_and_clear();
}

/// Overwrites previous printed line (assuming println) with text and flushes stdout. Use format macro for stringf.
fn ow_print(str: &str) {
    println!("\x1B[A\x1B[2K{}", str);
    io::stdout().flush().unwrap();
}

fn main() {
    // Arguments...
    // smwc2wav — CLI browser for SMWCentral "Music" section + searching.
    // smwc2wav -f [query file] — pass in a file with queries desired (plaintext, delimited by CRLF).
    // smwc2wav -q [URL or ID] — shorthand catch-all -i, -u, -f

    // smwc2wav -i [ID]
    // smwc2wav -u [URL] — handy for quick C+P from browser

    let client = reqwest::blocking::Client::new();
    let args = Cli::parse();

    let smwc_api = Url::parse(&*format!("https://www.smwcentral.net/ajax.php?a=getfile&v=2&id={}", args.query)).expect("Violation of: invalid SMWc API URL!");;
    let api_resp = client.get(smwc_api).send().unwrap();
    let file: SMWCFile = api_resp.json().unwrap();

    let is_obsolete = file.obsoleted_by.is_some();
    let is_featured = file.raw_fields.featured;

    let hrtime = unix_to_hrtime(file.time);
    let alphamapper = alphavec_to_map(TINY_CAPS_MAPPING);

    println!("\x1B[38;2;131;125;246m\n▓▓▓▓▓▓▓▓▓▒▓▓▓▓▒▒▒▒▒▒▓▓▒▒▓▒▒▒▒▒░▒▒▒▒▒▒▒▒░▒▒░░▒▒▒▒▒░░░▒▒░▒░▒▒░░▒▒▒▒▒░▒▒░░░░▒▒▒░░░░░▒░▒░░░░░▒░░░░░▒░░░░░░░░░░░░░\n");

    println!(" ╔{}╗", "═".repeat(file.name.len() + 2 + is_featured.then_some(2).unwrap_or(0)));
    print!(" ║ {}", file.name);
    if is_featured { println!(" * ║") } else { println!("  ║") }
    println!(" ╚{}╝", "═".repeat(file.name.len() + 2 + is_featured.then_some(2).unwrap_or(0)));

    print!("  └── {} ——— {} ——— {}", file.raw_fields.duration, HumanBytes(file.size as u64), file.authors.iter().map(|a| a.name.clone()).collect::<Vec<String>>().join(", "));
    match file.submitter {
        Some(s) => println!("[{}]", s.name),
        None => println!()
    }

    println!("     │\n     └── @ {}-{}-{} {}:{}:{}", hrtime.month(), hrtime.day(), hrtime.year(), hrtime.hour(), hrtime.minute(), hrtime.second());

    match file.rating {
        Some(r) => print!("        │\n        └── {}", q_str(&'★', &'☆', r, 5)),
        None => print!("        │\n        └── (no rating)")
    }
    println!(" {} downloads\n\n", file.downloads);

    println!("tags ▶  {}", file.tags.join(", "));
    println!("source ▶  {}", file.raw_fields.source);
    println!("samples ▶  {}\n\n\n\n", file.raw_fields.samples);
    println!("⏷  info  ⏷\n\n{}", strclamp(&strip_html(&file.raw_fields.description), 100));
    println!("{}", "\n▓▓▓▓▓▓▓▓▓▒▓▓▓▓▒▒▒▒▒▒▓▓▒▒▓▒▒▒▒▒░▒▒▒▒▒▒▒▒░▒▒░░▒▒▒▒▒░░░▒▒░▒░▒▒░░▒▒▒▒▒░▒▒░░░░▒▒▒░░░░░▒░▒░░░░░▒░░░░░▒░░░░░░░░░░░░░\x1B[0m\n");

    print!("Confirm download...");
    io::stdout().flush().unwrap();
    io::stdin().read_line(&mut String::new()).unwrap();
    ow_print("Downloading zip (1/3)");
    let zip_fname = file.id.to_string() + ".zip";
    s2w_download(&*file.download_url, &*zip_fname, &client, file.size as u64);

    ow_print("Extracting zip (2/3)");
    io::stdout().flush().unwrap();
    s2w_extract(&zip_fname);

    ow_print("Converting spc → wav (3/3)");
    let all_files = fs::read_dir(".").unwrap();
    let spc_files: Vec<_> = all_files
        .filter_map(Result::ok)
        .filter(|e| {
            e.path().extension().map_or(false, |ext| ext == "spc")
        })
        .collect();

    let spc_fname = spc_files[0].file_name();
    let spc = spc_fname.to_str().unwrap();
    s2w_conv(spc);

    let wav = fs::metadata(&spc.replace("spc", "wav"));
    ow_print(&format!("\x1B[38;2;41;255;188m{} of 16-bit goodness saved ✔\x1B[0m", HumanBytes(wav.unwrap().size())));

    let has_sox: bool = which("sox").unwrap().exists();
    let mut inq_opts: Vec<&str> = vec!["Purge"];

    if has_sox {
        inq_opts.push("Convert (lossless)");
        inq_opts.push( "Convert (lossy)");
    }

    inq_opts.push("Stats");

    let inq_fmt: MultiOptionFormatter<'_, &str> = &|a| format!("{} queued", a.len());
    let inq = MultiSelect::new("Select additional operations...", inq_opts)
        .with_formatter(inq_fmt)
        .prompt()
        .unwrap();

    inq.iter().for_each(|s| match s {
        &"Purge" => {
            let is_purge = Confirm::new("Purge song?")
                .with_default(false)
                .with_help_message("This permanently removes this session's files.")
                .prompt();
        },

        &"Convert (lossless)" | &"Convert (lossy)" => {
            let id3_title = file.name;
            let id3_artist = file.authors.join(", ");
            let id3_album = args.album;
            let id3_comment = "Processed by smwc2wav";
            let id3_date = format!("{}-{}-{}", hrtime.year(), hrtime.month(), hrtime.day());
            let id3_coverart = args.coverart;

        },

        &"Stats" => {

        }
        _ => {}
    });
}