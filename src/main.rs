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
use audiotags::{Album, MimeType, Picture, Tag};
use inquire::{Confirm, Select};
use strum::IntoEnumIterator;
use strum_macros::{AsRefStr, EnumIter};

use std::collections::HashMap;
use std::io::{ErrorKind, Write};
use std::os::unix::fs::MetadataExt;
use std::process::Command;
use std::thread::sleep;
use std::time::{Duration, UNIX_EPOCH};
use std::{fs, io};
use std::io::Error;

const TINY_CAPS_MAPPING: [char; 26] = ['ᴀ', 'ʙ', 'ᴄ', 'ᴅ', 'ᴇ', 'ғ', 'ɢ', 'ʜ', 'ɪ', 'ᴊ', 'ᴋ', 'ʟ', 'ᴍ', 'ɴ', 'ᴏ', 'ᴘ', 'ꞯ', 'ʀ', 's', 'ᴛ', 'ᴜ', 'ᴠ', 'ᴡ', 'x', 'ʏ', 'ᴢ'];

// Based on https://en.wikipedia.org/wiki/Magic_number_(programming)
#[derive(AsRefStr, EnumIter, PartialEq)]
enum FileType {
    JPEG,
    GIF,
    PNG,
    VTF,
    MIDI,
    UnixScript,
    ELF,
    PDF,
    MBR,
    TIFF,
    WAD,
    ZIP,
    TAR,
    XML,
    TXT,
    HEIC,
    WEBP,
    NES,
    BMP,
    SPC,
    WAV,
    AVI,
    AIFF,
    MP3,
    MP4,
    OGG,
    FLAC,
    M4A,
    AAC
}

impl FileType {
    // Returned as a regex pattern with byte values delimited with ; for the sake of generic sizing.
    // e.g. [4A BC] or [A3 BB 9F] -> (4A;BC|A3;BB;9F)
    fn magic(&self) -> &str {
        match *self {
            FileType::JPEG => "(FF;D8;FF;DB|FF;D8;FF;E0;00;10;4A;46;49;46;00;01|FF;D8;FF;EE|FF;D8;FF;E1;([0-9A-F]{2};){2}45;78;69;66;00;00|FF;D8;FF;E0)", // TODO: this excludes JPEG2000, needed?
            FileType::GIF => "(47;49;46;38;39;61|47;49;46;38;37;61).*", // GIF89a or GIF87a
            FileType::PNG => "(89;50;4E;47;0D;0A;1A;0A).*", // \211PNG\r\n\032\n
            FileType::VTF => "(00;46;54;56).*", // VTF\0 (https://developer.valvesoftware.com/wiki/VTF_(Valve_Texture_Format))
            FileType::MIDI => "(4D;54;68;64).*", // MThd
            FileType::UnixScript => "(23;21).*", // #!
            FileType::ELF => "(7F;45;4C;46).*", // 0x7F + ELF
            FileType::PDF => "(25;50;44;46;2D).*", // %PDF-
            FileType::MBR => ".*(55;AA)", // 0x55AA
            FileType::TIFF => "(49;49;2A;00|4D;4D;00;2A|49;49;2B;00|4D;4D;00;2B).*", // II (le) or MM (be) + 0x42
            FileType::WAD => "(49;57;41;44|50;57;41;44|57;41;44;32|57;41;44;33).*", // IWAD/PWAD (Doom), WAD2 (Quake), WAD3 (Half-Life)
            FileType::ZIP => "(50;4B;03:04).*", // PK♥♦
            FileType::TAR => "(75;73;74;61;72;00;30;30|75;73;74;61;72;20;20;00).*", // ustar␀00 or ustar␠␠␀
            FileType::XML => "(3C;3F;78;6D;6C;20|3C;00;3F;00;78;00;6D;00;6C;00;20|00;3C;00;3F;00;78;00;6D;00;6C;00;20|3C;00;00;00;3F;00;00;00;78;00;00;00;6D;00;00;00;6C;00;00;00;20;00;00;00|00;00;00;3C;00;00;00;3F;00;00;00;78;00;00;00;6D;00;00;00;6C;00;00;00;20).*",
            FileType::TXT => "(EF;BB;BF|FF;FE|FE;FF|FF;FE;00;00|00;00;FE;FF).*", // ï»¿, ÿþ, þÿ, ÿþ␀␀, or ␀␀þÿ
            FileType::HEIC => "(66;74;79;70;68;65;69;63;66;74;79;70;6D", // ftypheic
            FileType::WEBP => "(52;49;46;46;([0-9A-F]{2};){4}57;45;42;50).*", // RIFF????WEBP
            FileType::NES => "(4E;45;53;1A).*", // NES␚
            FileType::BMP => "(42;4D).*", // BM
            FileType::SPC => "(53;4E;45;53;2D;53;50;43;37;30;30;20;53;6F;75;6E;64;20;46;69;6C;65;20;44;61;74;61;20;76;30;2E;33;30;1A;1A).*", // SNES-SPC700 Sound File Data v0.30 + 2x 0x26
            FileType::WAV => "(52;49;46;46;([0-9A-F]{2};){4}57;41;56;45).*", // RIFF????WAVE
            FileType::AVI => "(52;49;46;46;([0-9A-F]{2};){4}41;56;49;20).*", // RIFF????AVI␠
            FileType::AIFF => "(46;4F;52;4D;([0-9A-F]{2};){4}41;49;46;46).*", // FORM????AIFF
            FileType::MP3 => "(FF;FB|FF;F3|FF;F2|49;44;33).*", // ÿû, ÿó, or ÿò (or ID3)
            FileType::MP4 => "(66;74;79;70;4D;53;4E;56).*", // ftypMSNV
            FileType::OGG => "(4F;67;67;53).*", // OggS
            FileType::FLAC => "(66;4C;61;43).*", // fLaC
            FileType::M4A => "(00;00;00;(1C|20);66;74;79;70;4D;34;41;20).*", // 0x000000 ftypM4A. There was a single byte difference for some reason? Also could also be M4A_? (https://docs.fileformat.com/audio/m4a/)
            FileType::AAC => "(FF;F1|FF;F9).*", // ÿñ or ÿù
        }
    }

    /// Utility method for converting to audiotags::MimeType
    fn mime(&self) -> Result<MimeType, Error> {
        match *self {
            FileType::JPEG => Ok(MimeType::Jpeg),
            FileType::PNG => Ok(MimeType::Png),
            FileType::TIFF => Ok(MimeType::Tiff),
            FileType::BMP => Ok(MimeType::Bmp),
            FileType::GIF => Ok(MimeType::Gif),
            _ => Err(Error::from(ErrorKind::Unsupported)) // TODO: wrong way to indicate unsupported datatype?
        }
    }
}

macro_rules! is_regex {
    ($str:expr, $pat:expr) => {{
        use regex::Regex;
        let re = Regex::new($pat).unwrap();
        re.is_match($str)
    }}
}

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    // Query (SMWCentral ID or URL)
    #[arg(short, long)]
    query: String,

    #[arg(short, long, default_missing_value = None)]
    album: Option<String>,

    #[arg(short, long, default_missing_value = None)]
    coverart: Option<String>
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
    rgx.replace_all(&*res, "\n").into()
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

    bar.tick();

    Command::new("spc2wav")
        .arg(loc)
        .output()
        .expect("Violation of: spc2wav could not be run (is it installed?)");

    fs::remove_file(loc).expect("Could not delete .spc file");

    bar.inc(1);
    sleep(Duration::from_millis(20));
    bar.finish_and_clear();
}

/// Overwrites previous printed line (assuming println) with text and flushes stdout. Use format macro for stringf.
fn ow_print(str: &str) {
    println!("\x1B[A\x1B[2K{}", str);
    io::stdout().flush().unwrap();
}

/// Get filetype by magic number.
/// Note standards may change, # not present, etc.
fn magictype(data: &Vec<u8>) -> Option<FileType> {
    let data_str = data.iter()
        .map(|&b| b.to_string())
        .collect::<Vec<String>>()
        .join(";")
        .to_uppercase();

    FileType::iter().find(|f| is_regex!(f.magic(), &data_str))
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


    // Validate arguments first for the sake of not hitting the user with a panic 3 minutes into operation

    let ca_data: Option<(Vec<u8>, MimeType)> = if let Some(ca) = args.coverart {
        let ca_file = fs::read(ca).expect("Cover art image could not be read");
        let ca_meta = magictype(&ca_file).expect("Cover art file could not be identified");
        let ca_mime = ca_meta.mime();

        if ca_mime.is_err() {
            panic!("Improper MIME type!");
        }

        Some((ca_file, ca_mime.unwrap()))
    } else {
        None
    };

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
    let spc_name = spc_fname.to_str().unwrap();
    s2w_conv(spc_name);

    let wav_name = &spc_name.replace(".spc", ".wav");
    let wav_meta = fs::metadata(wav_name).unwrap();
    ow_print(&format!("\x1B[38;2;41;255;188m{} of 16-bit goodness saved ✔\x1B[0m", HumanBytes(wav_meta.size())));

    let has_sox: bool = which("sox").unwrap().exists();
    let has_ffmpeg: bool = which("ffmpeg").unwrap().exists();
    let is_conv = Confirm::new("SoX detected. Convert audio format?").prompt();

    if has_sox && is_conv.expect("No choice!") {
        let conv_opts: Vec<&str> = vec!["flac", "mp3", "aiff", "ogg"];
        let conv_format = Select::new("Select format:", conv_opts).prompt().unwrap();
        let conv_name = &wav_name.replace(".wav", &format!(".{}", conv_format));
        
        Command::new("sox")
            .arg(wav_name)
            .arg(conv_name)
            .output()
            .expect("SoX failed to convert file.");

        fs::remove_file(wav_name).expect("Could not remove .wav file");

        let mut tag = Tag::default().read_from_path(conv_name).unwrap();
        tag.set_title(&*file.name);
        tag.set_artist(&*file.authors.iter()
            .map(|a| a.name.clone())
            .collect::<Vec<String>>()
            .join(", "));

        if let Some(album) = args.album {
            tag.set_album(Album::with_title(&album));
        }

        if let Some((ca_file, ca_mime)) = ca_data {
            tag.set_album_cover(Picture::new(&*ca_file, ca_mime));
        }

        tag.set_year(hrtime.year());
        tag.set_comment("Processed by smwc2wav".into());
        tag.set_genre("Game");

        tag.write_to_path(conv_name).expect("Failed to save ID3 tags");
    }
}