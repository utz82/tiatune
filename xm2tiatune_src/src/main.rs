// xm2tiatune - convert XM files to TIAtune music data
// by utz 11'2017

extern crate xmkit;

use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::str;

const DEFAULT_ORIGIN: usize = 0xf000;
const PLAYER_SIZE: usize = 290;
const MAX_BIN_SIZE: usize = 0xffc;
const INSTRUMENT_NAMES: [&str; 7] = [
    "SQUARE",
    "POLY9",
    "POLY4",
    "R1813",
    "POLY5",
    "POLY5_4",
    "R1813_POLY4",
];
const INSTRUMENT_SIZES: [usize; 7] = [4, 67, 5, 7, 7, 62, 62];
const NTSC: bool = true;
const PAL: bool = false;

fn instrument2name(instrument: u8) -> &'static str {
    INSTRUMENT_NAMES[instrument as usize]
}

fn name2instrument(name: &str) -> u8 {
    INSTRUMENT_NAMES.iter().position(|&n| n == name).unwrap() as u8
}

// returns divider*2, to avoid dealing with floats at this point
fn instrument2div(instrument: u8) -> u16 {
    match instrument {
        0 | 1 => 4,
        2 => 15,
        3 | 4 => 31,
        5 | 6 => 465,
        _ => panic!("Invalid instrument used"),
    }
}

fn note2freq(note: u8) -> f64 {
    const FREQUENCY_B7: f64 = 7902.13 / 2.0;
    const TWELTH_ROOT_OF_2: f64 = 1.059463094359;

    if note == 0 || note == 97 {
        0.0
    } else {
        FREQUENCY_B7 * TWELTH_ROOT_OF_2.powf((-96 + note as i8).into())
    }
}

fn note2freq_div(note_idx: u8, instr_div: u16, ntsc: bool) -> u16 {
    let freq = note2freq(note_idx);
    let base_freq = if ntsc { 1193181.67 } else { 1182298.0 };
    (65536.0
        - freq * 256.0 * 256.0 / (base_freq / (88.0 + 14.0 / 256.0)) * ((instr_div as f64) / 2.0))
        .round() as u16
}

fn write_note_table(unique_note_div_combos: &HashMap<(u8, u16), u8>, ntsc: bool) {
    let filename = if ntsc {
        "note_table_ntsc.h"
    } else {
        "note_table_pal.h"
    };

    let mut note_table = File::create(filename).unwrap();

    let mut note_indices: Vec<_> = unique_note_div_combos.iter().collect();
    note_indices.sort_by(|a, b| a.1.cmp(b.1));

    let note_names = [
        "c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-",
    ];

    let mut fvals = Vec::<(u16, String, u16)>::new();

    for entry in note_indices {
        if entry.0 .0 == 0 {
            fvals.push((0xffff, "rest".to_string(), 0));
        } else {
            let fval = note2freq_div(entry.0 .0, entry.0 .1, ntsc);
            let note_name = note_names[((entry.0 .0 - 1) % 12) as usize].to_owned()
                + &(((entry.0 .0 - 1) / 12) as usize).to_string();
            fvals.push((fval, note_name, entry.0 .1));
        }
    }

    note_table.write_all("note_table_hi\n".as_bytes()).unwrap();
    for entry in &fvals {
        note_table
            .write_all(
                format!(
                    "\t!byte >${:x}\t; {}, div{}\n",
                    entry.0,
                    entry.1,
                    (entry.2 as f64 / 2.0)
                )
                .as_bytes(),
            )
            .unwrap();
    }

    note_table.write_all("note_table_lo\n".as_bytes()).unwrap();
    for entry in &fvals {
        note_table
            .write_all(
                format!(
                    "\t!byte <${:x}\t; {}, div{}\n",
                    entry.0,
                    entry.1,
                    (entry.2 as f64 / 2.0)
                )
                .as_bytes(),
            )
            .unwrap();
    }
}

fn get_unique_instruments(xm: &xmkit::XModule) -> HashSet<u8> {
    let mut unique_instruments = HashSet::new();
    unique_instruments.insert(0);

    for (ptn_num, ptn) in xm.patterns.iter().enumerate() {
        if xm.pattern_used(ptn_num as u8) {
            for row in 0..ptn.len() as u8 {
                for channel in 0..2 {
                    let instr = ptn.tracks[channel].instrument(row).unwrap();
                    if instr != 0 && !unique_instruments.contains(&(instr - 1)) {
                        unique_instruments.insert(instr - 1);
                    }
                }
            }
        }
    }

    unique_instruments
}

fn write_unique_instruments(unique_instruments: &HashSet<u8>, mut outfile: &File) {
    let unique_instruments_v = INSTRUMENT_NAMES
        .iter()
        .filter(|name| unique_instruments.contains(&name2instrument(name)))
        .collect::<Vec<_>>();

    for (idx, name) in unique_instruments_v.iter().enumerate() {
        outfile
            .write_all(format!("\t{} = {}\n", name, idx).as_bytes())
            .unwrap();
    }
}

fn instrument_data_size(unique_instruments: &HashSet<u8>) -> usize {
    unique_instruments
        .iter()
        .map(|idx| INSTRUMENT_SIZES[*idx as usize])
        .sum::<usize>()
}

fn get_used_pattern_ids(xm: &xmkit::XModule) -> Vec<usize> {
    xm.patterns
        .iter()
        .enumerate()
        .filter(|&(idx, _)| xm.pattern_used(idx as u8))
        .map(|(idx, _)| idx)
        .collect::<Vec<_>>()
}

fn sequence_data_size(xm: &xmkit::XModule) -> usize {
    let used_ptns = get_used_pattern_ids(&xm);

    if used_ptns.len() > 254 {
        panic!("More than 254 patterns used.");
    }

    xm.sequence().len() + 2 + used_ptns.len() + used_ptns.len() / 2 + used_ptns.len() % 2
}

// writes sequence + pattern lookup table
fn write_sequence(xm: &xmkit::XModule, mut outfile: &File) {
    let used_ptns = get_used_pattern_ids(&xm);

    let new_ptn_indices = used_ptns
        .iter()
        .enumerate()
        .map(|(idx, ptn)| (ptn, idx))
        .collect::<HashMap<&usize, usize>>();

    outfile.write_all("\nsequence\n".as_bytes()).unwrap();

    for it in xm.sequence() {
        outfile
            .write_all(
                format!(
                    "\t!byte ${:x}\n",
                    new_ptn_indices.get(&(it as usize)).unwrap() + 1
                )
                .as_bytes(),
            )
            .unwrap();
    }

    outfile.write_all("\t!byte 0\n".as_bytes()).unwrap();

    outfile
        .write_all("\npattern_lookup_lo\n".as_bytes())
        .unwrap();

    for p in &used_ptns {
        outfile
            .write_all(format!("\t!byte <ptn{:x}\n", p + 1).as_bytes())
            .unwrap()
    }

    outfile.write_all("\t!byte <ptnEnd\n".as_bytes()).unwrap();

    outfile
        .write_all("\npattern_lookup_hi\n".as_bytes())
        .unwrap();

    outfile
        .write_all(format!("\t!byte (ptn{:x}>>4)&$f0\n", used_ptns[0] + 1).as_bytes())
        .unwrap();

    let mut used_ptns_hi = used_ptns;
    const PADDING: usize = 0xff;
    used_ptns_hi.remove(0);
    used_ptns_hi.resize(
        used_ptns_hi.len() + (2 - used_ptns_hi.len() % 2) % 2,
        PADDING,
    );

    for pos in used_ptns_hi.chunks(2) {
        outfile.write_all("\t!byte ".as_bytes()).unwrap();
        if pos[1] == PADDING {
            outfile
                .write_all(format!("(ptn{:x}>>8)&$f\n", pos[0] + 1).as_bytes())
                .unwrap();
        } else {
            outfile
                .write_all(
                    format!(
                        "((ptn{:x}>>8)&$f)|((ptn{:x}>>4)&$f0)\n",
                        pos[0] + 1,
                        pos[1] + 1
                    )
                    .as_bytes(),
                )
                .unwrap();
        }
    }
}

fn write_definitions(xm: &xmkit::XModule) {
    let mut outfile = File::create("def.h").unwrap();

    outfile
        .write_all(format!("\tBPM = {}\n", xm.bpm()).as_bytes())
        .unwrap();

    write_unique_instruments(&get_unique_instruments(xm), &outfile);
}

fn main() {
    println!("xm2tiatune converter {}", env!("CARGO_PKG_VERSION"));

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
    let mut total_byte_count: usize = 0;

    write_definitions(&xm);

    write_sequence(&xm, &outfile);

    let mut unique_note_div_combos = HashMap::<(u8, u16), u8>::new();
    unique_note_div_combos.insert((0, 0), 0);

    for (ptn_num, ptn) in xm.patterns.iter().enumerate() {
        if xm.pattern_used(ptn_num as u8) {
            outfile
                .write_all(format!("\nptn{:x}\n", ptn_num + 1).as_bytes())
                .unwrap();

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
                    && ptn.tracks[0].instrument(row).unwrap()
                        == ptn.tracks[0].instrument(row - 1).unwrap()
                    && ptn.tracks[1].instrument(row).unwrap()
                        == ptn.tracks[1].instrument(row - 1).unwrap()
                {
                    *speeds.last_mut().unwrap() += ptn.tempo(&xm, row).unwrap();
                } else {
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

                let mut ctrlbyte = (speeds[row] - 1) * 4;
                if !triggers_ch1[row]
                    && (row > 0
                        && notes_ch1[row] == notes_ch1[row - 1]
                        && volumes_ch1[row] == volumes_ch1[row - 1]
                        && instruments_ch1[row] == instruments_ch1[row - 1])
                {
                    ctrlbyte += 1;
                }
                if !triggers_ch2[row]
                    && (row > 0
                        && notes_ch2[row] == notes_ch2[row - 1]
                        && volumes_ch2[row] == volumes_ch2[row - 1]
                        && instruments_ch2[row] == instruments_ch2[row - 1])
                {
                    ctrlbyte += 2;
                }
                outfile
                    .write_all(format!("\t!byte ${:x}", ctrlbyte).as_bytes())
                    .unwrap();

                if (ctrlbyte & 1) == 0 {
                    let mut notebyte = notes_ch1[row];
                    if notebyte == 97 {
                        notebyte = 0;
                    }
                    if instruments_ch1[row] == 2 && notebyte != 0 {
                        notebyte += 12;
                    }
                    if instruments_ch1[row] != 0
                        && note2freq_div(notebyte, instrument2div(instruments_ch1[row] - 1), PAL)
                            == 0
                    {
                        println!(
                            "Replaced out-of-range note in pattern {:#x}, \
				  channel 1, row {:#x} with a rest.",
                            ptn_num, row
                        );
                        notebyte = 0;
                    }

                    let vibyte = if notebyte > 0 {
                        ((volumes_ch1[row] - 1) * 2) & 0xf8
                    } else {
                        0
                    };

                    let instr = if instruments_ch1[row] == 0 {
                        0
                    } else {
                        instruments_ch1[row] - 1
                    };

                    let instr_str = if vibyte == 0 {
                        "SQUARE"
                    } else {
                        instrument2name(instr)
                    };

                    if notebyte != 0
                        && !unique_note_div_combos.contains_key(&(notebyte, instrument2div(instr)))
                    {
                        unique_note_div_combos.insert(
                            (notebyte, instrument2div(instr)),
                            unique_note_div_combos.len() as u8,
                        );
                    }

                    let note_index = if notebyte == 0 {
                        &0
                    } else {
                        unique_note_div_combos
                            .get(&(notebyte, instrument2div(instr)))
                            .unwrap()
                    };

                    outfile
                        .write_all(
                            format!(", ${:x} + {}, ${:x}", vibyte, instr_str, note_index)
                                .as_bytes(),
                        )
                        .unwrap();
                    bytecount += 2;
                }

                if (ctrlbyte & 0x2) == 0 {
                    let mut notebyte = notes_ch2[row];
                    if notebyte == 97 {
                        notebyte = 0;
                    }
                    if instruments_ch2[row] == 2 && notebyte != 0 {
                        notebyte += 12;
                    }
                    if instruments_ch2[row] != 0
                        && note2freq_div(notebyte, instrument2div(instruments_ch2[row] - 1), PAL)
                            == 0
                    {
                        println!(
                            "Replaced out-of-range note in pattern {:#x}, \
				  channel 1, row {:#x} with a rest.",
                            ptn_num, row
                        );
                        notebyte = 0;
                    }

                    let vibyte = if notebyte > 0 {
                        ((volumes_ch2[row] - 1) * 2) & 0xf8
                    } else {
                        0
                    };

                    let instr = if instruments_ch2[row] == 0 {
                        0
                    } else {
                        instruments_ch2[row] - 1
                    };

                    let instr_str = if vibyte == 0 {
                        "SQUARE"
                    } else {
                        instrument2name(instr)
                    };

                    if notebyte != 0
                        && !unique_note_div_combos.contains_key(&(notebyte, instrument2div(instr)))
                    {
                        unique_note_div_combos.insert(
                            (notebyte, instrument2div(instr)),
                            unique_note_div_combos.len() as u8,
                        );
                    }

                    let note_index = if notebyte == 0 {
                        &0
                    } else {
                        unique_note_div_combos
                            .get(&(notebyte, instrument2div(instr)))
                            .unwrap()
                    };

                    outfile
                        .write_all(
                            format!(", ${:x} + {}, ${:x}", vibyte, instr_str, note_index)
                                .as_bytes(),
                        )
                        .unwrap();
                    bytecount += 2;
                }

                outfile.write_all("\n".as_bytes()).unwrap();
            }

            if bytecount > 255 {
                panic!(
                    "\n Pattern {:#x}: Maximum length exceeded by {:#x} bytes.",
                    ptn_num,
                    bytecount - 255
                );
            }

            total_byte_count += bytecount;
        }
    }

    outfile.write_all("\nptnEnd\n".as_bytes()).unwrap();

    write_note_table(&unique_note_div_combos, PAL);
    write_note_table(&unique_note_div_combos, NTSC);

    println!("player:      {:>4}", PLAYER_SIZE);
    println!("patterns:    {:>4}", total_byte_count);
    println!("sequence:    {:>4}", sequence_data_size(&xm));
    println!(
        "instruments: {:>4}",
        instrument_data_size(&get_unique_instruments(&xm))
    );
    println!("note_div:    {:>4}", unique_note_div_combos.len() * 2);

    total_byte_count += PLAYER_SIZE
        + sequence_data_size(&xm)
        + instrument_data_size(&get_unique_instruments(&xm))
        + unique_note_div_combos.len() * 2;

    println!("total:       {:>4}", total_byte_count);

    if total_byte_count > MAX_BIN_SIZE {
        println!(
            "ERROR: Maximum data size exceeded by {} bytes.",
            total_byte_count - MAX_BIN_SIZE
        );
        std::process::exit(1);
    } else {
        println!(
            "Success, player+data written from {:#x} to {:#x}, {} bytes free.",
            DEFAULT_ORIGIN,
            DEFAULT_ORIGIN + total_byte_count,
            MAX_BIN_SIZE - (total_byte_count)
        );
    }
}
