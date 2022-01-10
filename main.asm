;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;improved by Thomas Jentzsch 01'2021

; Ideas:
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; o precalulate waveforms
;   o poly4_5
;   - R1813 -> poly4
; + rowLen via TIMxxT
; x variable tempo (using fractional math here too)
; + use wait cycles

; TODOs:
; + adapt FreqDiv tables to faster loop
; + adapt tone lengths to faster loop (see TEMPO)
; + sounds rough, maybe preserve X/Y between notes
; + test T1024T (too short for single loop, use two, nested loops)
; + test divs and resulting frequencies

; Legend: + done, o partially done, - todo, x canceled

;wave  AUDCx      range         type
;0     4,5        c-0..gis-8    square  div2
;1     8          c-0..gis-8    poly9   div2
;2     1          c-0..a-5      poly4   r1813 (TODO: fix)
;3     6,A        c-0..gis-4    r1813   div31 (div2?)
;4     7,9        c-0..gis-4    poly5   div31
;TODO:
;5     3                        poly5_4 div2
;6     2                        poly4   r1813

;Assembler switches
NTSC    = 0 ; else PAL

TIMER   = 1     ; use T1024T and variable speed
!ifdef TIMER {
BPM     = 155   ; should be defined in music.asm
TPB     = 16    ; should be defined in music.asm
!if NTSC {
HZ      = 1193182
} else {
HZ      = 1182298
}
TDIV    = BPM * TPB * 1024
TEMPO   = (HZ * 60 + TDIV / 2) / TDIV ; * 1024 = shortest note length
VISUALS = 1 ; add some visuals (both channels) (+40 bytes)
} else {
; only allowed without timer!
VISUALS = 1 ; add some visuals (from channel 0 only) (+75 bytes)
}

;Define which waveforms should be excluded
;NO_SQUARE
;NO_POLY9
;NO_POLY4
;NO_R1813
;NO_POLY5
NO_POLY5_4

    !to "tiatune.bin", plain
    !sl "tiatune.sym"
    !cpu 6510

    !source "vcs.h"
    !source "notes.h"

    * = $f000, invisible
    !pseudopc $80 {
seqOffs     !byte 0     ;seqence offset
ptnOffs     !byte 0
ptnPtrL     !byte 0     ;temporary
ptnPtrH     !byte 0     ;temporary
!ifndef TIMER {
rowLenL     !byte 0
}
rowLenH     !byte 0
saveX       = ptnOffs   ;temporary
saveY       !byte 0     ;temporary
VAR_END
    }

    !macro NextPoly ~.val, .bits, .tap1, .tap2  {
        !if (.val & .tap1) != 0 XOR (.val & .tap2) != 0 {
            !set .or = 1 << (.bits - 1)
        } else {
            !set .or = 0
        }
        !set .val = (.val >> 1) | .or
    }

    !macro CreatePoly1 .val, .bits, .tap1, .tap2 {
        !set .addr = * + (1 << (.bits - 3))
        !set .pat = 0
        !for .i, 2, 1 << .bits {
            !set .pat = (.pat >> 1) | ((.val & 1) * $80)
            !if (.i & 7) = 0 {
                * = .addr - (.i >> 3), invisible ; store in reverse order
                !byte <.pat
                !set .pat = 0
            }
            +NextPoly ~.val, .bits, .tap1, .tap2
        }
        * = .addr
    }

    !macro CreatePoly5_4 .val5, .val4 {
        !set .length = ((1 << 5) - 1) * ((1 << 4) - 1)
        !set .addr = * + ((.length + 7) >> 3)
        !set .pat = 0
        !for .i, 1+6, .length+6 {
;            !warn .val4 & 1, .i
            !set .pat = (.pat >> 1) | ((.val4 & 1) * $80)
            !if (.i & 7) = 0 {
                * = .addr - (.i >> 3), invisible ; store in reverse order
;                !warn .i, *, <.pat
                !byte <.pat
                !set .pat = 0
            }
            +NextPoly ~.val5, 5, 1, 4
            !if (.val5 & 1) = 1 {
                +NextPoly ~.val4, 4, 1, 2
            }
        }
        * = .addr - ((.i + 7) >> 3), invisible ; store in reverse order
;        !warn .i, *, <.pat
        !byte <.pat
        * = .addr
    }

    !macro PrevPoly ~.val, .tap1, .tap2  {
        !if (.val & .tap1) != 0 XOR (.val & .tap2) != 0 {
            !set .or = 1
        } else {
            !set .or = 0
        }
        !set .val = (.val << 1) | .or
    }

    !macro CreatePoly .init, .count, .tap1, .tap2 {
        !set .val = .init
        !set .idx = 0
        !for .x, 0, .count {
            +PrevPoly ~.val, .tap1, .tap2
            !set .pat = .pat << 1
            !if (.val & 1) = 1 {
                !set .pat = .pat | 1
            }
            !set .idx = .idx + 1
            !if .idx = 8 {
              !set .idx = 0
              !byte <.pat
              !set .pat = 0
            }
        }
        !set .pat = .pat << 1
        !byte <.pat
    }

    *       = $f000
    !zone main

PolyX
;    +CreatePoly1    1, 4, 1, 2
;    +CreatePoly1    1, 5, 1, 4
;    +CreatePoly5_4  %10011, %1111

PatternTbl
;Note: 1st bit of a pattern MUST always be set!
!ifndef NO_SQUARE {
SquarePtn
    !byte   %01010101 }
!ifndef NO_POLY9 {
Poly9Ptn
    +CreatePoly 1, 510, 8, 256 }
!ifndef NO_POLY4 {
Poly4Ptn
    +CreatePoly 1, 14, 1, 8 }
!ifndef NO_R1813 {
R1813Ptn
    !byte   %00000000, %00000111, %11111111, %11111110 }
!ifndef NO_POLY5 {
Poly5Ptn
    +CreatePoly 1, 30, 2, 16 }
!ifndef NO_POLY5_4 {
Poly5_4Ptn
    +CreatePoly5_4 %00001, %0001 }

PatternPtr
!ifndef NO_SQUARE   { !byte   <SquarePtn }
!ifndef NO_POLY9    { !byte   <Poly9Ptn }
!ifndef NO_POLY4    { !byte   <Poly4Ptn }
!ifndef NO_R1813    { !byte   <R1813Ptn }
!ifndef NO_POLY5    { !byte   <Poly5Ptn }
!ifndef NO_POLY5_4  { !byte   <Poly5_4Ptn }

InitVal
!ifndef NO_SQUARE   { !byte   $01 }
!ifndef NO_POLY9    { !byte   $02 }
!ifndef NO_POLY4    { !byte   $02 }
!ifndef NO_R1813    { !byte   $02 }
!ifndef NO_POLY5    { !byte   $02 }
!ifndef NO_POLY5_4  { !byte   $40 }

ResetVal
!ifndef NO_SQUARE   { !byte    1-1 }
!ifndef NO_POLY9    { !byte   64-1 }
!ifndef NO_POLY4    { !byte    2-1 }
!ifndef NO_R1813    { !byte    4-1 }
!ifndef NO_POLY5    { !byte    4-1 }
!ifndef NO_POLY5_4  { !byte   59-1 }

Reset
    cld
    ldx     #0                  ;clear TIA regs, RAM, set SP to $00ff
    txa                         ;alternatively just use CLEAN_START macro
-
    dex
    txs
    pha
    bne     -

;relocate player to zeropage
    ldx     #PlayerLength - 1
-
    lda     PlayerCode,x
    sta     VAR_END,x
    dex
    bpl     -

!ifdef TIMER {
 !ifdef VISUALS {
    ldx     #6
.wait
    dex
    bpl     .wait
    sta     RESP0
    stx     NUSIZ0
    stx     NUSIZ1
    lda     #$28
    sta     RESP1
    stx     CTRLPF
    sta     REFP0
    sta     HMP1
    lsr
    sta     HMP0
    sta     WSYNC
    sta     HMOVE
 }
}

.readSeq                        ;read next entry in sequence
    ldy     seqOffs
    lda     sequence_hi,y
    beq     Reset               ;if hi-byte = 0, loop
    sta     ptnPtrH
    lda     sequence_lo,y
    sta     ptnPtrL
    inc     seqOffs
    ldy     #0
    beq     .enterPtn

ReadPtn
    sty     saveY
    ldy     ptnOffs             ;saveX and ptnOffs share the same RAM byte!
.enterPtn
    stx     saveX
    lda     (ptnPtrL),y         ;ctrl byte
    beq     .readSeq            ;0-end marker

    sta     rowLenH
    bmi     .noCh0Reload

    iny
    lax     (ptnPtrL),y         ;wave0/vol0
    lsr
    lsr
    lsr
    sta+1   Vol0
    txa
    and     #$7
    tax
    lda     PatternPtr,x
    sta+1   Pattern0
    lda     InitVal,x
    sta+1   Init0
    lda     ResetVal,x
    sta+1   Reset0
    cmp     saveX
    bcs     .xOK
    sta     saveX
.xOK

    iny
    cpx     #2                  ;look up freq divider depending on waveform used
    bcs     +                   ;0,1 -> square/poly9
    lax     (ptnPtrL),y         ;note
    lda     FreqDiv2Lsb,x
    sta+1   Freq0L
    lda     FreqDiv2Msb,x
    bcc     .contCh0

+
    bne     +                   ;2 -> poly4
    lax     (ptnPtrL),y
    lda     FreqDiv15Lsb,x
    sta+1   Freq0L
    lda     FreqDiv15Msb,x
    bcs     .contCh0

+
    lax     (ptnPtrL),y         ;3,4 -> 1813/poly5
    lda     FreqDiv31Lsb,x
    sta+1   Freq0L
    lda     FreqDiv31Msb,x
.contCh0
    sta+1   Freq0H
.noCh0Reload

    bit     rowLenH
    bvs     .noCh1Reload

    iny
    lax     (ptnPtrL),y         ;wave1/vol1
    lsr
    lsr
    lsr
    sta+1   Vol1
    txa
    and     #$7
    tax
    lda     PatternPtr,x
    sta+1   Pattern1
    lda     InitVal,x
    sta+1   Init1
    lda     ResetVal,x
    sta+1   Reset1
    cmp     saveY
    bcs     .yOK
    sta     saveY
.yOK

    iny
    cpx     #2                  ;look up freq divider depending on waveform used
    bcs     +                   ;0,1 -> square/poly9
    lax     (ptnPtrL),y         ;note
    lda     FreqDiv2Lsb,x
    sta+1   Freq1L
    lda     FreqDiv2Msb,x
    bcc     .contCh1

+
    bne     +                   ;2 -> poly4
    lax     (ptnPtrL),y
    lda     FreqDiv15Lsb,x
    sta+1   Freq1L
    lda     FreqDiv15Msb,x
    bcs     .contCh1

+
    lax     (ptnPtrL),y         ;3,4 -> 1813/poly5
    lda     FreqDiv2Lsb,x
    sta+1   Freq1L
    lda     FreqDiv2Msb,x
.contCh1
    sta+1   Freq1H
.noCh1Reload

    lda     rowLenH
    and     #$3f
    sta     rowLenH

    ldx     saveX               ;saveX and ptnOffs share the same RAM byte!
    iny
    sty     ptnOffs
    ldy     saveY
;    clc
    jmp     PlayNote

PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage
.loopH                          ;8
!ifdef TIMER {
    lda     #TEMPO              ;2          define next tick length,
    sta     T1024T              ;4          constant, even if loop is exited
}
    dec     rowLenH             ;5
    bne     PlayNote            ;3/2 =  8/7 (+6)
    jmp     ReadPtn             ;3
;---------------------------------------
.waitCh0                        ;3
!ifndef TIMER {
 !ifdef VISUALS {
    txs                         ;2
    lda+1   Sum0H               ;3
    lsr                         ;2
    lsr                         ;2
    tax                         ;2
    lda     WaveGfx,x           ;4
    sta+2   PF2                 ;4
    tsx                         ;2
;Note: carry is random
    bpl     ContinueCh0         ;3   = 27
 } else { ;{
    jmp     WaitCh0             ;24  = 27
 } ;}
} else {
    jmp     WaitCh0             ;24  = 27
}

.resetIdx1                      ;11          assumes 1st bit set
Reset1  = *+1
    ldy     #3                  ;2   = 13    0,1,3,63(,58)
Init1   = *+1
    lda     #$02                ;2           or $01 (square,poly4->5)
    sta+1   Mask1               ;3
    nop                         ;2
.loadV1
Vol1    = *+1
    lda     #0                  ;2
.zeroV1
    sta     AUDV1               ;3   = 12
ContinueCh1

!ifdef TIMER {
    lda     TIMINT              ;4
    bmi     .loopH              ;2/3 =  6/7
                                ;           avg 88 cycles (was 114)
} else {
    dec     rowLenL             ;5          always loops 256 times here
    beq     .loopH              ;2/3 =  7/8
                                ;           avg 89 cycles (was 114)
}
;---------------------------------------
PlayNote
.sum0L  = *+1
    lda     #0                  ;2
Freq0L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum0L              ;3
Sum0H  = *+1
    lda     #0                  ;2
Freq0H  = *+1
    adc     #0                  ;2
    sta+1   Sum0H               ;3   = 14

    bcs     .waitCh0            ;2/3 = 2/3
;create waveform from table
;assumes 1st bit of waveform always set
Mask0   = *+1
    lda     #1                  ;2
    bpl     .contMask0          ;2/3
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx0          ;2/3
.cont0
    sta+1   Mask0               ;3   = 13
Pattern0 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.contMask0                      ;5
    asl                         ;2
    bcc     .cont0              ;3

.resetIdx0                      ;11          assumes 1st bit set
Reset0  = *+1
    ldx     #3                  ;2   = 13    0,1,3,63(,58)
Init0   = *+1
    lda     #$02                ;2           or $01 (square,poly4->5)
    sta+1   Mask0               ;3
    nop                         ;2
.loadV0
Vol0    = *+1
    lda     #0                  ;2
.zeroV0
    sta     AUDV0               ;3   = 12
; 31 bytes
ContinueCh0
;---------------------------------------
.sum1L  = *+1
    lda     #0                  ;2
Freq1L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum1L              ;3
Sum1H  = *+1
    lda     #0                  ;2
Freq1H  = *+1
    adc     #0                  ;2
    sta+1   Sum1H               ;3   = 14

    bcs     .waitCh1            ;2/3 =  2/3
Mask1   = *+1
    lda     #1                  ;2
    bpl     .contMask1          ;2/3
    lda     #$01                ;2
    dey                         ;2
    bmi     .resetIdx1          ;2/3
.cont1
    sta+1   Mask1               ;3   = 13
Pattern1 = *+1
    and     PatternTbl,y        ;4
    bne     .loadV1             ;2/3
    beq     .zeroV1             ;3

.contMask1                      ;5
    asl                         ;2
    bcc     .cont1              ;3

.waitCh1                        ;3
    jmp     WaitCh1             ;24  = 27
}
PlayerLength = * - PlayerCode


!ifdef VISUALS {
 !ifndef TIMER {
WaitCh1                         ;3
    lda+1   Freq0H              ;3
    sta     COLUPF              ;3
    lda     #1                  ;2
    sta+2   CTRLPF              ;4
    nop                         ;2
    nop                         ;2
    clc                         ;2
    jmp     ContinueCh1         ;3   = 24
 } else { ; {
WaitCh0                         ;3
    lda+1   Mask0               ;3
    sta     GRP0                ;3
    lda+1   Mask1               ;3
    sta     GRP1                ;3
    nop                         ;2
    nop                         ;2
    clc                         ;2
    jmp     ContinueCh0         ;3   = 24

WaitCh1                         ;3
    lda+1   Freq0H              ;3
    sta     COLUP0              ;3
    lda+1   Freq1H              ;3
    sta     COLUP1              ;3
    nop                         ;2
    nop                         ;2
    clc                         ;2
    jmp     ContinueCh1         ;3   = 24
 }
} else { ;{
WaitCh0                         ;3
    jsr     Wait18              ;18
    jmp     ContinueCh0         ;3   = 24

WaitCh1                         ;3
    jsr     Wait18              ;18
    jmp     ContinueCh1         ;3   = 24

Wait18                          ;6
    nop                         ;2
    nop                         ;2
    clc                         ;2
    rts                         ;6   = 18
} ;}

!if NTSC { ;{
;$10000 - Frequency * 256 * 256 / (1193181.67 / (89 + 8/256)) * div (div = 2, 15, 31)
FreqDiv2Msb = * - 1
    !h  FF FF FF FF FF FF FF FF FF FE FE FE ;c-0..b-0
    !h  FE FE FE FE FE FE FE FE FE FD FD FD ;c-1..b-1
    !h  FD FD FD FD FC FC FC FC FC FB FB FB ;c-2..b-2
    !h  FB FA FA FA F9 F9 F8 F8 F8 F7 F7 F6 ;c-3..b-3
    !h  F6 F5 F4 F4 F3 F2 F1 F1 F0 EF EE ED ;c-4..b-4
    !h  EC EA E9 E8 E6 E5 E3 E2 E0 DE DC DA ;c-5..b-5
    !h  D8 D5 D3 D0 CD CA C7 C4 C0 BC B8 B4 ;c-6..b-6
    !h  B0 AB A6 A0 9B 95 8E 88 81 79 71 69 ;c-7..b-7
    !h  60 56 4C 41 36 2A 1D 10             ;c-8..g-8
FreqDiv2Lsb = * - 1
    !h  60 56 4C 41 36 2A 1D 10 02 F3 E3 D2 ;c-0..b-0
    !h  C0 AD 98 83 6D 55 3B 20 04 E6 C6 A4 ;c-1..b-1
    !h  80 5A 31 07 DA AA 77 41 08 CC 8C 48 ;c-2..b-2
    !h  00 B4 63 0E B4 54 EE 83 11 98 18 90 ;c-3..b-3
    !h  01 69 C7 1D 68 A8 DD 06 22 30 30 21 ;c-4..b-4
    !h  02 D2 8F 3A D0 50 BA 0C 44 61 61 43 ;c-5..b-5
    !h  05 A4 1F 74 A0 A2 75 18 89 C2 C3 87 ;c-6..b-6
    !h  0A 48 3F E8 41 43 EB 31 12 85 86 0D ;c-7..b-7
    !h  14 91 7E D2 83 87 D6 63             ;c-8..g-8

FreqDiv15Msb
    !h  FB FB FA FA FA F9 F9 F8 F8 F8 F7 F7 ;c-0..b-0
    !h  F6 F6 F5 F4 F4 F3 F2 F1 F1 F0 EF EE ;c-1..b-1
    !h  ED EC EA E9 E8 E6 E5 E3 E2 E0 DE DC ;c-2..b-2
    !h  DA D8 D5 D3 D0 CD CA C7 C4 C0 BD B9 ;c-3..b-3
    !h  B5 B0 AB A6 A1 9B 95 8F 89 81 7A 72 ;c-4..b-4
    !h  6A 61 57 4D 43 37 2B 1F 12 03       ;c-5..a-5
FreqDiv15Lsb = * - 1
    !h  50 09 BE 6D 18 BE 60 FA 8F 1E A6 27 ;c-0..b-0
    !h  A1 12 7B DB 31 7E BF F5 20 3D 4D 4F ;c-1..b-1
    !h  42 24 F6 B6 63 FB 7F EB 3F 7B 9B 9F ;c-2..b-2
    !h  84 4A ED 6D C6 F8 FE D7 80 F6 37 3E ;c-3..b-3
    !h  09 94 DB DA 8D EF FC AE 01 ED 6E 7D ;c-4..b-4
    !h  12 28 B6 B5 1B DF F8 5D 01 DA       ;c-5..a-5

FreqDiv31Msb
    !h  F6 F5 F5 F4 F3 F3 F2 F1 F0 EF EE ED ;c-0..b-0
    !h  EC EB EA E8 E7 E6 E4 E2 E1 DF DD DB ;c-1..b-1
    !h  D9 D6 D4 D1 CF CC C9 C5 C2 BE BA B6 ;c-2..b-2
    !h  B2 AD A9 A3 9E 98 92 8B 85 7D 75 6D ;c-3..b-3
    !h  65 5B 52 47 3C 31 24 17 0A          ;c-4..gis-4
FreqDiv31Lsb
    !h  51 BE 22 7B CD 12 4F 7D A0 B7 BE B8 ;c-0..b-0
    !h  A2 7B 43 F8 9A 26 9C FB 42 6E 7E 70 ;c-1..b-1
    !h  44 F6 86 F1 33 4C 39 F7 84 DC FD E2 ;c-2..b-2
    !h  8A EE 0D E2 68 9A 73 EF 09 B9 FA C5 ;c-3..b-3
    !h  12 DD 1B C3 CE 33 E8 DF 13          ;c-4..gis-4
    ;} /NTSC
} else {
    ; { PAL
;$10000 - Frequency * 256 * 256 / (1182298 / (89 + 8/256)) * div (div = 2, 15, 31)
FreqDiv2Msb = * - 1
    !h  FF FF FF FF FF FF FF FF FE FE FE FE ;c-0..b-0
    !h  FE FE FE FE FE FE FE FE FD FD FD FD ;c-1..b-1
    !h  FD FD FD FD FC FC FC FC FB FB FB FB ;c-2..b-2
    !h  FA FA FA FA F9 F9 F8 F8 F7 F7 F7 F6 ;c-3..b-3
    !h  F5 F5 F4 F4 F3 F2 F1 F0 EF EF EE EC ;c-4..b-4
    !h  EB EA E9 E8 E6 E5 E3 E1 DF DE DC D9 ;c-5..b-5
    !h  D7 D5 D2 D0 CD CA C6 C3 BF BC B8 B3 ;c-6..b-6
    !h  AF AA A5 A0 9A 94 8D 87 7F 78 70 67 ;c-7..b-7
    !h  5E 55 4A 40 34 28 1B 0E             ;c-8..g-8
FreqDiv2Lsb = * - 1
    !h  5E 55 4A 40 34 28 1B 0E FF F0 E0 CF ;c-0..b-0
    !h  BD A9 95 80 69 51 37 1C FF E1 C0 9E ;c-1..b-1
    !h  7A 53 2B 00 D2 A2 6F 38 FF C2 81 3D ;c-2..b-2
    !h  F4 A8 56 00 A5 44 DE 71 FE 84 03 7A ;c-3..b-3
    !h  E9 50 AD 01 4A 89 BC E2 FC 09 06 F5 ;c-4..b-4
    !h  D3 A0 5A 02 95 12 78 C5 F9 12 0D EA ;c-5..b-5
    !h  A6 40 B5 04 2A 24 F0 8B F3 24 1B D5 ;c-6..b-6
    !h  4D 81 6B 08 54 48 E0 17 E6 48 37 AA ;c-7..b-7
    !h  9B 02 D7 11 A8 90 C1 2E             ;c-8..g-8

FreqDiv15Msb = * - 1
    !h  FB FA FA FA FA F9 F9 F8 F8 F8 F7 F7 ;c-0..b-0
    !h  F6 F5 F5 F4 F4 F3 F2 F1 F0 F0 EF EE ;c-1..b-1
    !h  ED EB EA E9 E8 E6 E5 E3 E1 E0 DE DC ;c-2..b-2
    !h  DA D7 D5 D3 D0 CD CA C7 C3 C0 BC B8 ;c-3..b-3
    !h  B4 AF AB A6 A0 9B 95 8E 87 80 79 71 ;c-4..b-4
    !h  68 5F 56 4C 41 36 2A 1D 0F 01       ;c-5..a-5
FreqDiv15Lsb = * - 1
    !h  45 FD B1 60 0B B0 50 EA 7E 0C 92 12 ;c-0..b-0
    !h  8B FA 62 C1 16 60 A0 D4 FD 18 26 25 ;c-1..b-1
    !h  15 F5 C4 82 2B C0 40 A9 F9 31 4C 4B ;c-2..b-2
    !h  2C EC 8A 04 57 82 81 52 F4 62 99 97 ;c-3..b-3
    !h  58 D9 15 08 AE 03 02 A5 E8 C4 33 2F ;c-4..b-4
    !h  B1 B1 2A 11 5E 07 05 4C D0 88       ;c-5..a-5

FreqDiv31Msb
    !h  F6 F5 F5 F4 F3 F2 F2 F1 F0 EF EE ED ;c-0..b-0
    !h  EC EB EA E8 E7 E5 E4 E2 E0 DF DD DB ;c-1..b-1
    !h  D8 D6 D4 D1 CE CB C8 C5 C1 BE BA B6 ;c-2..b-2
    !h  B1 AD A8 A3 9D 97 91 8A 83 7C 74 6C ;c-3..b-3
    !h  63 5A 50 46 3B 2F 22 15 07          ;c-4..gis-4
FreqDiv31Lsb
    !h  3A A6 08 60 B0 F4 2E 5B 7C 90 95 8D ;c-0..b-0
    !h  75 4A 0F C2 60 EA 5C B7 FA 21 2D 1A ;c-1..b-1
    !h  E9 95 1F 84 C0 D2 B8 6F F3 43 5A 36 ;c-2..b-2
    !h  D3 2D 40 09 82 A6 71 DE E8 86 B5 6D ;c-3..b-3
    !h  A5 5A 81 10 02 4C E3 BC D0          ;c-4..gis-4
} ;} /PAL

!ifdef VISUALS {
 !ifndef TIMER {
WaveGfx
    !fill   7, %00000000
    !fill   7, %10000000
    !fill   7, %11000000
    !fill   7, %01100000
    !fill   8, %00110000
    !fill   7, %00011000
    !fill   7, %00001100
    !fill   7, %00000110
    !fill   7, %00000011
 }
}
    !zone debug
!ifndef USED {
USED = 1
    !warn * - $f000, " bytes used"
}

    !zone musicdata
musicData
!if 0 { ;{
SQUARE  = 0
POLY9   = 1
POLY4   = 2
R1813   = 3
POLY5   = 4

sequence_hi
    !byte >ptn0
    !byte 0
sequence_lo
    !byte <ptn0
ptn0
    !byte $5f, (%1111<<3)|SQUARE, a3
    !byte 0
} else { ;}
    !source "music.asm"
}

    * = $fffc

    !word   Reset          ; RESET
    !word   $0000          ; IRQ

;       END
