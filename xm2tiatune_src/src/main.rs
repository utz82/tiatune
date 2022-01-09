// xm2tiatune - convert XM files to TIAtune music data
// by utz 11'2017

extern crate xmkit;

use std::env;
use std::str;
use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

//grab version number from Cargo.toml
const PRGM_VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    println!("xm2tiatune converter {}", PRGM_VERSION);

    let filename = match env::args().nth(1) {
        Some(arg) => arg,
        None => "music.xm".to_string(),
    };

    let xm = match xmkit::XModule::parse_file(&Path::new(&filename)) {
        Err(e) => panic!("{}", e.to_string()),
        Ok(xm) => xm,
    };

    if xm.channel_count() < 2 {
        panic!("Module must have at least 2 channels.");
    }

    let mut outfile = File::create("music.asm").unwrap();
    let mut total_byte_count: usize = 1;
    
    outfile.write_all("\nsequence_hi\n".as_bytes()).unwrap();
    for it in xm.sequence() {
        outfile.write_all(format!("\t!byte ptn{:x}>>8\n", it).as_bytes()).unwrap();
    }
    outfile.write_all("\t!byte 0".as_bytes()).unwrap();

    outfile.write_all("\nsequence_lo\n".as_bytes()).unwrap();
    for it in xm.sequence() {
        outfile.write_all(format!("\t!byte ptn{:x}&$ff\n", it).as_bytes()).unwrap();
        total_byte_count += 2;
    }


    for (ptn_num, ptn) in xm.patterns.iter().enumerate() {
        if xm.pattern_used(ptn_num as u8) {
            outfile.write_all(format!("\nptn{:x}\n", ptn_num).as_bytes()).unwrap();

            let mut triggers_ch1 = vec![ptn.tracks[0].note_trigger(0).unwrap()];
            let mut notes_ch1 = vec![ptn.tracks[0].note(0).unwrap()];
            let mut volumes_ch1 = vec![ptn.tracks[0].volume(0).unwrap()];
            let mut instruments_ch1 = vec![ptn.tracks[0].instrument(0).unwrap()];
            let mut triggers_ch2 = vec![ptn.tracks[1].note_trigger(0).unwrap()];
            let mut notes_ch2 = vec![ptn.tracks[1].note(0).unwrap()];
            let mut volumes_ch2 = vec![ptn.tracks[1].volume(0).unwrap()];
            let mut instruments_ch2 = vec![ptn.tracks[1].instrument(0).unwrap()];
            let mut speeds = vec![ptn.tempo(&xm, 0).unwrap()];

            for row in 1..ptn.len() as u8 {
                if speeds.last().unwrap() + ptn.tempo(&xm, row).unwrap() <= 0x3f 
                    && ptn.tracks[0].note(row).unwrap() == ptn.tracks[0].note(row - 1).unwrap()
                    && ptn.tracks[1].note(row).unwrap() == ptn.tracks[1].note(row - 1).unwrap()
                    && ptn.tracks[0].volume(row).unwrap() == ptn.tracks[0].volume(row - 1).unwrap()
                    && ptn.tracks[1].volume(row).unwrap() == ptn.tracks[1].volume(row - 1).unwrap()
                    && ptn.tracks[0].instrument(row).unwrap() == ptn.tracks[0].instrument(row - 1).unwrap()
                    && ptn.tracks[1].instrument(row).unwrap() == ptn.tracks[1].instrument(row - 1).unwrap() {
                    *speeds.last_mut().unwrap() += ptn.tempo(&xm, row).unwrap();
                }
                else {
                    triggers_ch1.push(ptn.tracks[0].note_trigger(row).unwrap());
                    notes_ch1.push(ptn.tracks[0].note(row).unwrap());
                    volumes_ch1.push(ptn.tracks[0].volume(row).unwrap());
                    instruments_ch1.push(ptn.tracks[0].instrument(row).unwrap());
                    triggers_ch2.push(ptn.tracks[1].note_trigger(row).unwrap());
                    notes_ch2.push(ptn.tracks[1].note(row).unwrap());
                    volumes_ch2.push(ptn.tracks[1].volume(row).unwrap());
                    instruments_ch2.push(ptn.tracks[1].instrument(row).unwrap());
                    speeds.push(ptn.tempo(&xm, row).unwrap());
                }
            }

            let mut bytecount = 0;

            for row in 0..triggers_ch1.len() {
                bytecount += 1;
                
                let mut ctrlbyte = speeds[row];
                if !triggers_ch1[row] && (row > 0 && notes_ch1[row] == notes_ch1[row - 1]
                    && volumes_ch1[row] == volumes_ch1[row - 1]
                    && instruments_ch1[row] == instruments_ch1[row - 1]) {
                    ctrlbyte += 0x80;
                }
                if !triggers_ch2[row] && (row > 0 && notes_ch2[row] == notes_ch2[row - 1]
                    && volumes_ch2[row] == volumes_ch2[row - 1]
                    && instruments_ch2[row] == instruments_ch2[row - 1]) {
                    ctrlbyte += 0x40;
                }
                outfile.write_all(format!("\t!byte ${:x}", ctrlbyte).as_bytes()).unwrap();

                if (ctrlbyte & 0x80) == 0 {
                    let mut notebyte = notes_ch1[row];
                    if notebyte == 97 { notebyte = 0; }
                    if instruments_ch1[row] == 2 && notebyte != 0 { notebyte += 12; }
                    if (instruments_ch1[row] == 3 && notebyte > 53) || (instruments_ch1[row] > 3 && notebyte > 41) {
                        println!("Replaced out-of-range note in pattern {:#x}, channel 1, row {:#x} with a rest.", ptn_num, row);
                        notebyte = 0;
                    }

                    let vibyte = if notebyte > 0 { (((volumes_ch1[row] - 1) * 2) & 0xf8) + (instruments_ch1[row] - 1) }
                        else { 0 };
                    
                    outfile.write_all(format!(", ${:x}, ${:x}", vibyte, notebyte).as_bytes()).unwrap();
                    bytecount += 2;
                }

                if (ctrlbyte & 0x40) == 0 {
                    let mut notebyte = notes_ch2[row];
                    if notebyte == 97 { notebyte = 0; }
                    if instruments_ch2[row] == 2 && notebyte != 0 { notebyte += 12; }
                    if (instruments_ch2[row] == 3 && notebyte > 53) || (instruments_ch2[row] > 3 && notebyte > 41) {
                        println!("Replaced out-of-range note in pattern {:#x}, channel 1, row {:#x} with a rest.", ptn_num, row);
                        notebyte = 0;
                    }

                    let vibyte = if notebyte > 0 { (((volumes_ch2[row] - 1) * 2) & 0xf8) + (instruments_ch2[row] - 1) }
                        else { 0 };
                    
                    outfile.write_all(format!(", ${:x}, ${:x}", vibyte, notebyte).as_bytes()).unwrap();
                    bytecount += 2;
                }

                outfile.write_all("\n".as_bytes()).unwrap();
            }

            if bytecount > 255 {
                panic!("\n Pattern {:#x}: Maximum length exceeded by {:#x} bytes.", ptn_num, bytecount - 255);
            }

            total_byte_count += bytecount + 1;
            outfile.write_all("\t!byte 0".as_bytes()).unwrap();
        }
    }

    if total_byte_count + 0xf47e > 0xfffc {
        panic!("Error: Maximum data size exceeded by {} bytes.", total_byte_count + 0xf47e - 0xfffc);
    }

    println!("Success, player+data written from 0xf000 to {:#x}, {:#x} bytes free", total_byte_count + 0xf47e, 0xb7e - total_byte_count);
}
