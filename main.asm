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
NTSC    = 0     ; else use PAL frequencies
VISUALS = 1     ; add some visuals (both channels) (+38 bytes)
USE_SUB = 1     ; saves 30 bytes, costs ~16 cycles/channel loaded

BPM     = 104   ; should be defined in music.asm
TPB     = 24    ; should be defined in music.asm

; calculate TEMPO (do not change!)
!if NTSC {
HZ      = 1193182
} else {
HZ      = 1182298
}
TDIV    = BPM * TPB * 1024
TEMPO   = (HZ * 60 + TDIV / 2) / TDIV ; * 1024 = shortest note length

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
rowLenH     !byte 0
saveX       = ptnOffs   ;temporary
saveY       !byte 0     ;temporary
!if USE_SUB { ; {
tmpDivL     !byte 0     ;temporary
}
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

!if VISUALS {
; position and size players
    lda     (0,x)
    dex
    stx     CTRLPF
    sta     RESP0
    stx     NUSIZ0
    stx     NUSIZ1
    stx     REFP0               ; use P1 to reverse direction
    sta     RESP1
}

;relocate player to zeropage
    ldx     #PlayerLength - 1
-
    lda     PlayerCode,x
    sta     VAR_END,x
    dex
    bpl     -

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
!if USE_SUB = 0 { ; {
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
} else { ;{
    jsr     LoadPattern
    stx+1   Freq0L
} ;}
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
!if USE_SUB = 0 { ; {
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
} else { ;{
    jsr     LoadPattern
    stx+1   Freq1L
};}
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

!if USE_SUB { ;{
    !zone pattern
LoadPattern
    iny
    cpx     #2                  ;look up freq divider depending on waveform used
    bcs     +                   ;0,1 -> square/poly9
    lax     (ptnPtrL),y         ;note
    lda     FreqDiv2Lsb,x
    sta     tmpDivL
    lda     FreqDiv2Msb,x
    rts

+
    bne     +                   ;2 -> poly4
    lax     (ptnPtrL),y
    lda     FreqDiv15Lsb,x
    sta     tmpDivL
    lda     FreqDiv15Msb,x
    rts

+
    lax     (ptnPtrL),y         ;3,4 -> 1813/poly5
    lda     FreqDiv2Lsb,x
    sta     tmpDivL
    lda     FreqDiv2Msb,x
    ldx     tmpDivL
    rts
} ;}

PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage
.loopH                          ;7
    lda     #TEMPO              ;2          define tick length,
    sta     T1024T              ;4          constant, even if loop is exited
    dec     rowLenH             ;5
    bne     PlayNote            ;3/2= 14/13
    jmp     ReadPtn             ;3          (RTS would work here too)
;---------------------------------------
.resetIdx1                      ;11         assumes 1st bit set
Reset1  = *+1
    ldy     #3                  ;2  = 13    0,1,3,63(,58)
Init1   = *+1
    lda     #$02                ;2          or $01/$40 (square/poly4->5)
    sta+1   Mask1               ;3
    nop                         ;2
.loadV1
Vol1    = *+1
    lda     #0                  ;2
.zeroV1
    sta     AUDV1               ;3  = 12
ContinueCh1

    lda     TIMINT              ;4
    bmi     .loopH              ;2/3=  6/7
                                ;           avg 88 cycles (was 114)
;---------------------------------------
PlayNote
.sum0L  = *+1
    lda     #0                  ;2
Freq0L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum0L              ;3
.sum0H  = *+1
    lda     #0                  ;2
Freq0H  = *+1
    adc     #0                  ;2
    sta+1   .sum0H              ;3  = 14

    bcs     .waitCh0            ;2/3= 2/3
;create waveform from table
;assumes 1st bit of waveform always set
Mask0   = *+1
    lda     #1                  ;2
    bpl     .contMask0          ;2/3
    lda     #$01                ;2
    dex                         ;2
    bmi     .resetIdx0          ;2/3
.cont0
    sta+1   Mask0               ;3  = 13
Pattern0 = *+1
    and     PatternTbl,x        ;4
    bne     .loadV0             ;2/3
    beq     .zeroV0             ;3

.contMask0                      ;5
    asl                         ;2
    bcc     .cont0              ;3

.resetIdx0                      ;11         assumes 1st bit set
Reset0  = *+1
    ldx     #3                  ;2  = 13    0,1,3,63(,58)
Init0   = *+1
    lda     #$02                ;2          or $01/$40 (square/poly4->5)
    sta+1   Mask0               ;3
    nop                         ;2
.loadV0
Vol0    = *+1
    lda     #0                  ;2
.zeroV0
    sta     AUDV0               ;3  = 12
; 31 bytes
ContinueCh0
;---------------------------------------
.sum1L  = *+1
    lda     #0                  ;2
Freq1L  = *+1
    adc     #0                  ;2           CF==0!
    sta     .sum1L              ;3
.sum1H  = *+1
    lda     #0                  ;2
Freq1H  = *+1
    adc     #0                  ;2
    sta+1   .sum1H              ;3  = 14

    bcs     .waitCh1            ;2/3=  2/3
Mask1   = *+1
    lda     #1                  ;2
    bpl     .contMask1          ;2/3
    lda     #$01                ;2
    dey                         ;2
    bmi     .resetIdx1          ;2/3
.cont1
    sta+1   Mask1               ;3  = 13
Pattern1 = *+1
    and     PatternTbl,y        ;4
    bne     .loadV1             ;2/3
    beq     .zeroV1             ;3

.contMask1                      ;5
    asl                         ;2
    bcc     .cont1              ;3
;---------------------------------------
!if VISUALS {
.waitCh0                        ;3
    jmp     WaitCh0             ;24 = 27

.waitCh1                        ;3
    jmp     WaitCh1             ;24 = 27
} else { ;{
.waitCh0                        ;3
    brk                         ;21
    nop                         ;           2 bytes skipped
.waitCh1                        ;3
    brk                         ;21
    bcc     ContinueCh0         ;3  = 27
    bcc     ContinueCh1         ;3  = 27
} ;}
; 7 bytes RAM free (2 used by stack if VISUALS disabled)
}
PlayerLength = * - PlayerCode

!if VISUALS {
WaitCh0                         ;3
    lda+1   Mask0               ;3
    sta     GRP0                ;3
    lda+1   Freq0H              ;3
    asl                         ;2          spread colors better
    asl                         ;2
    sta     COLUP0              ;3
    clc                         ;2
    jmp     ContinueCh0         ;3  = 24

WaitCh1                         ;3
    lda+1   Mask1               ;3
    sta     GRP1                ;3
    lda+1   Freq1H              ;3
    asl                         ;2          spread colors better
    asl                         ;2
    sta     COLUP1              ;3
    clc                         ;2
    jmp     ContinueCh1         ;3  = 24
} else { ;{
Wait                            ;7
    pla                         ;4
    nop                         ;2
    clc                         ;2
    rts                         ;6  = 21
} ;}

!ifndef PASS1 {
    !warn * - $f000, " player byte"
}

FrequencyStart
!if NTSC { ;{
;$10000 - Frequency * 256 * 256 / (1193181.67 / (88 + 14/256)) * div (div = 2, 15, 31)
FreqDiv2Msb = * - 1
    !h  FF FF FF FF FF FF FF FF FF FE FE FE ;c-0..b-0
    !h  FE FE FE FE FE FE FE FE FE FD FD FD ;c-1..b-1
    !h  FD FD FD FD FC FC FC FC FC FB FB FB ;c-2..b-2
    !h  FB FA FA FA F9 F9 F9 F8 F8 F7 F7 F6 ;c-3..b-3
    !h  F6 F5 F4 F4 F3 F2 F2 F1 F0 EF EE ED ;c-4..b-4
    !h  EC EB E9 E8 E7 E5 E4 E2 E0 DE DC DA ;c-5..b-5
    !h  D8 D6 D3 D0 CE CB C8 C4 C1 BD B9 B5 ;c-6..b-6
    !h  B0 AC A7 A1 9C 96 90 89 82 7A 73 6A ;c-7..b-7
    !h  61 58 4E 43 38 2C 20 13 04          ;c-8..gis-8
FreqDiv2Lsb = * - 1
    !h  61 58 4E 43 38 2C 20 13 04 F5 E6 D5 ;c-0..b-0
    !h  C3 B0 9C 87 71 59 40 26 09 EB CC AA ;c-1..b-1
    !h  87 61 39 0F E2 B3 81 4C 13 D7 98 55 ;c-2..b-2
    !h  0E C3 73 1F C5 67 02 98 27 AF 31 AB ;c-3..b-3
    !h  1D 86 E7 3E 8B CD 05 30 4E 5F 62 56 ;c-4..b-4
    !h  3A 0D CE 7D 17 9B 0A 60 9D BF C5 AD ;c-5..b-5
    !h  75 1B 9D FA 2E 37 14 C1 3B 7F 8B 5A ;c-6..b-6
    !h  EA 36 3B F4 5C 6F 28 82 76 FF 16 B5 ;c-7..b-7
    !h  D5 6D 76 E8 B8 DF 51 04 EC          ;c-8..gis-8

FreqDiv15Msb
    !h  FB FB FA FA FA F9 F9 F9 F8 F8 F7 F7 ;c-0..b-0
    !h  F6 F6 F5 F4 F4 F3 F2 F2 F1 F0 EF EE ;c-1..b-1
    !h  ED EC EB E9 E8 E7 E5 E4 E2 E0 DE DD ;c-2..b-2
    !h  DA D8 D6 D3 D1 CE CB C8 C5 C1 BD BA ;c-3..b-3
    !h  B5 B1 AC A7 A2 9D 97 90 8A 83 7B 74 ;c-4..b-4
    !h  6B 62 59 4F 45 3A 2E 21 14 06       ;c-5..a-5
FreqDiv15Lsb = * - 1
    !h  5D 17 CC 7C 29 D0 72 0E A4 34 BD 40 ;c-0..b-0
    !h  BB 2E 98 FA 53 A1 E4 1D 4A 69 7C 80 ;c-1..b-1
    !h  76 5C 31 F5 A5 41 C9 3A 93 D3 F9 02 ;c-2..b-2
    !h  EE B9 63 EA 4B 84 92 74 27 A7 F2 05 ;c-3..b-3
    !h  DB 73 C7 D4 96 08 26 E9 4F 4F E5 0A ;c-4..b-4
    !h  B7 E6 8F A9 2D 11 4C D4 9E 9E       ;c-5..a-5

FreqDiv31Msb
    !h  F6 F5 F5 F4 F3 F3 F2 F1 F0 EF EE ED ;c-0..b-0
    !h  EC EB EA E9 E7 E6 E4 E3 E1 DF DD DB ;c-1..b-1
    !h  D9 D7 D5 D2 CF CC C9 C6 C3 BF BB B7 ;c-2..b-2
    !h  B3 AE AA A4 9F 99 93 8D 86 7F 77 6F ;c-3..b-3
    !h  66 5D 54 49 3E 33 27 1A 0C          ;c-4..gis-4
FreqDiv31Lsb
    !h  6C DB 40 9B EF 37 75 A6 CB E4 EF EB ;c-0..b-0
    !h  D9 B4 80 39 DE 6F E9 4D 99 C9 DF D7 ;c-1..b-1
    !h  B1 69 00 72 BC DD D3 9A 30 93 BF B0 ;c-2..b-2
    !h  63 D5 01 E4 7A BC A7 35 63 27 7E 60 ;c-3..b-3
    !h  C5 AA 03 C8 F2 78 4F 6B C6          ;c-4..gis-4
    ;} /NTSC
} else {
    ; { PAL
;$10000 - Frequency * 256 * 256 / (1182298 / (88 + 14/256)) * div (div = 2, 15, 31)
FreqDiv2Msb = * - 1
    !h  FF FF FF FF FF FF FF FF FF FE FE FE ;c-0..b-0
    !h  FE FE FE FE FE FE FE FE FE FD FD FD ;c-1..b-1
    !h  FD FD FD FD FC FC FC FC FC FB FB FB ;c-2..b-2
    !h  FB FA FA FA F9 F9 F8 F8 F8 F7 F7 F6 ;c-3..b-3
    !h  F6 F5 F4 F4 F3 F2 F1 F1 F0 EF EE ED ;c-4..b-4
    !h  EC EA E9 E8 E6 E5 E3 E2 E0 DE DC DA ;c-5..b-5
    !h  D8 D5 D3 D0 CD CA C7 C4 C0 BC B8 B4 ;c-6..b-6
    !h  B0 AB A6 A1 9B 95 8F 88 81 79 71 69 ;c-7..b-7
    !h  60 56 4C 42 36 2A 1E 10 02          ;c-8..gis-8
FreqDiv2Lsb = * - 1
    !h  60 56 4C 42 36 2A 1E 10 02 F3 E3 D2 ;c-0..b-0
    !h  C0 AD 99 84 6D 55 3C 21 05 E7 C7 A5 ;c-1..b-1
    !h  81 5B 33 08 DB AB 79 43 0A CE 8E 4A ;c-2..b-2
    !h  03 B7 66 11 B7 57 F2 86 14 9C 1C 95 ;c-3..b-3
    !h  05 6E CD 22 6E AE E4 0D 29 38 39 2A ;c-4..b-4
    !h  0C DC 9A 45 DC 5D C8 1A 53 71 72 55 ;c-5..b-5
    !h  18 B8 35 8B B8 BB 90 35 A7 E3 E5 AB ;c-6..b-6
    !h  30 71 6A 16 71 76 21 6A 4E C6 CA 55 ;c-7..b-7
    !h  60 E2 D4 2C E3 ED 42 D5 9D          ;c-8..gis-8

FreqDiv15Msb = * - 1
    !h  FB FB FA FA FA F9 F9 F8 F8 F8 F7 F7 ;c-0..b-0
    !h  F6 F6 F5 F4 F4 F3 F2 F1 F1 F0 EF EE ;c-1..b-1
    !h  ED EC EB E9 E8 E7 E5 E3 E2 E0 DE DC ;c-2..b-2
    !h  DA D8 D6 D3 D0 CE CB C7 C4 C1 BD B9 ;c-3..b-3
    !h  B5 B0 AC A7 A1 9C 96 8F 89 82 7A 72 ;c-4..b-4
    !h  6A 61 58 4E 43 38 2C 1F 12 04       ;c-5..a-5
FreqDiv15Lsb = * - 1
    !h  52 0B C0 6F 1B C1 63 FE 93 22 AA 2B ;c-0..b-0
    !h  A5 17 80 E0 37 84 C5 FC 27 45 55 57 ;c-1..b-1
    !h  4B 2E 00 C1 6E 07 8B F8 4E 8A AB B0 ;c-2..b-2
    !h  96 5D 01 82 DD 10 17 F1 9D 14 57 60 ;c-3..b-3
    !h  2C BA 03 04 BA 1F 2F E3 3A 29 AE C0 ;c-4..b-4
    !h  5A 74 06 0A 75 3E 5E C8 73 53       ;c-5..a-5

FreqDiv31Msb
    !h  F6 F5 F5 F4 F3 F3 F2 F1 F0 EF EE ED ;c-0..b-0
    !h  EC EB EA E9 E7 E6 E4 E3 E1 DF DD DB ;c-1..b-1
    !h  D9 D7 D4 D2 CF CC C9 C6 C2 BE BB B7 ;c-2..b-2
    !h  B2 AE A9 A4 9E 98 92 8C 85 7D 76 6E ;c-3..b-3
    !h  65 5C 52 48 3D 31 25 18 0A          ;c-4..gis-4
FreqDiv31Lsb
    !h  56 C3 27 81 D3 18 55 84 A7 BE C6 C1 ;c-0..b-0
    !h  AC 85 4D 03 A6 33 A9 09 51 7D 8F 82 ;c-1..b-1
    !h  56 0A 9A 07 4A 65 53 13 A1 FB 1E 05 ;c-2..b-2
    !h  AF 15 37 0E 96 CB A7 27 44 F7 3C 0B ;c-3..b-3
    !h  5C 2B 6E 1A 2B 96 50 4E 89          ;c-4..gis-4
} ;} /PAL

!ifndef PASS1 {
    !warn * - FrequencyStart, " frequency bytes"
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
;    !source "music.asm"
;    !source "music_2.asm"
    !source "music_std.asm"
}
!ifndef PASS1 {
    !warn * - musicData, " music bytes"
    !warn $fffc - *, " bytes free"
PASS1
}


    * = $fffc

    !word   Reset          ; RESET
!if VISUALS {
    !word   $0000          ; IRQ
} else {
    !word   Wait
}
