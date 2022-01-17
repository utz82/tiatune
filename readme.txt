********************************************************************************
TIAtune v0.3.0
by utz 11'2017 * irrlichtproject.de
updated by Thomas Jentzsch and utz 01'2022
********************************************************************************

Updates
================================================================================
- Optimized waveform generation code. This increased the sample rate from 10.4
  to 13.5 kHz.
- All TIA waveforms are supported.
- Optimized music data format to save space.
- Song tempo can be controlled now
- About 700 more bytes are free now for music.
- Optional simple visuals


ABOUT
================================================================================

TIAtune is a music driver for the Atari 2600 (VCS) console. Unlike other TIA
music drivers, it does not rely on the on the TIA's built-in frequency
dividers. That means that the detune usually associated with TIA music is absent
in TIAtune.


Features
========

- 16-bit frequency dividers (accurate pitch within <0.15% margin)
- sample rate: 13.5 KHz
- per-step tempo, 6-bit tempo resolution
- BPM (~2.5 BMP granularity).


Limitations
===========

- 100% CPU time used, cannot render graphics at the same time as playing music
- fairly large player (528 bytes with demo music, was 1147 bytes)


The TIAtune source is designed to be assembled with the ACME cross-assembler.


XM CONVERTER
================================================================================

You can use the provided music.xm template to compose music for TIAtune. To use
the converter, you must first install the ACME assembler, which is available at
https://sourceforge.net/projects/acme-crossass/. You should also have the Stella
emulator installed. Then simply run compile.cmd (Windows) or compile.sh
(*nix, Mac OS X) to convert, assemble, and run the result in Stella.

The following limitations apply:

- Any changes to instruments/samples are ignored.
- FX commands are ignored, except for Fxx (change tempo, xx < 0x20)
- The range of instruments is limited:
  - instruments 1 and 2: C0..G#8
  - instrument 3: C0..A5
  - instruments 4, 5 and 6: C0..G#4
  - instrument 7: C0..G#2
  - instrument 8: C0..A1

Pattern length is also limited. The actual limit depends on the converted data
size. In the worst case, you will run out of bytes after 51 rows, but normally
you can get away with 64 and more rows. Generally it is a good idea to keep
patterns short, though. The converter optimizes pattern data, but not the
overall pattern structure. That means you can often save some bytes by breaking
down your patterns into smaller parts and removing redundancies.

Linux and Mac users will have to build the converter from source. Provided you
have Rust and Cargo installed, building the converter should be a simple matter
of running

  cargo build --release

in the xm2tiatune directory and then moving the build from target/release to the
main directory.


MUSIC DATA FORMAT
================================================================================

Music data for TIAtune consists of a sequence, followed by one or more patterns.
The music data must be provided in a file named "music.asm", which is included
by the main file.


Sequence
========

The sequence contains a list of ids of pointers to patterns, in the order in
which they are to be played. The sequence list must be terminated with a 0-byte.
The sequence may at most contain 255 entries.

The pattern pointer are split into a hi-byte and a lo-byte part, labelled
"pattern_lookup_hi" and "pattern_lookup_lo", respectively. The pattern pointer
must be ordered like the pattern definitions. The lo-list must be terminated
with the lo-pointer to the byte after the last pattern. For the hi-pointer, two
pointers are encoded into one byte (see example below).

The most simple sequence would thus be:

sequence
    !byte 1 ;index to pattern1
    !byte 0 ;terminator

pattern_lookup_lo
    !byte <pattern1
    !byte <pattern2
    !byte <pattern3
    !byte <patternEnd ; terminator
pattern_lookup_hi
    !byte (>pattern1>>4)&$f0
    !byte ((pattern2>>8)&$f)|((>pattern3>>4)&$f0)

Patterns
========

Patterns contain the actual music data. They consist of one or more rows
(steps), which in turn consist of 1-5 data bytes. The function of the data bytes
is as follows:

byte  bits   function
1     0      if set, skip updating channel 1
      1      if set, skip updating channel 2
      0..5   tempo (step length)
2     0..2   waveform channel 1 (0..4)
      3..6   volume channel 1
3     0..7   note channel 1
4     0..2   waveform channel 2 (0..4)
      3..6   volume channel 2
5     0..7   note channel 2

If bit 0 of byte 1 is set, byte 2 and 3 are omitted. Likewise, if bit 1 of byte
1 is set, byte 4 and 5 are omitted. On the first step of the first pattern in
the sequence, no data bytes may be omitted.

AUDCx equivalents of the waveform parameter, and their note ranges are as
follows:

wave  AUDCx      range
0     4,5,C,D    C0..G#8
1     8          C0..G#8
2     1          C0..A5
3     6,A        C0..G#4
4     7,9        C0..G#4
5     3          C0..G#2
6     2          C0..A1

Each pattern may contain up to 255 data bytes. Thus, each pattern may contain at
least 51 steps. In most cases however, it is advisable to use shorter patterns,
to optimize overall data usage.

For more information, check the provided example music.asm file.
