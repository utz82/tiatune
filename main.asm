;TIAtune
;Atari 2600 music player
;by utz 10'2017 * irrlichtproject.de
;improved by Thomas Jentzsch & utz 01'2021

; Ideas:
; + get rid of initial CLC
; + keep variables in X/Y
; + inline waveform code into RAM
; o precalulate waveforms
;   + poly4_5
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
;2     1          c-0..a-5      poly4   r1813
;3     6,A        c-0..gis-4    r1813   div31
;4     7,9        c-0..gis-4    poly5   div31
;5     3                        poly5_4 div31
;6     2                        poly4   r1813

;Assembler switches
NTSC    = 0     ; else use PAL frequencies
VISUALS = 0     ; add some visuals (both channels) (+38 bytes)
DEBUG   = 1     ; enable debug output
MUSIC   = 0     ; chose track (0..2)

; calculate TEMPO (do not change!)
!if NTSC {
HZ      = 1193182
} else {
HZ      = 1182298
}
;Calculation based on tick duration [ms] = 2500/BPM
TDIV    = BPM * 1024 * 10
TEMPO   = (HZ * 25 + TDIV / 2) / TDIV ; * 1024 = shortest note length

    !to "tiatune.bin", plain
    !sl "tiatune.sym"

    !cpu 6510
    !source "vcs.h"
    !source "def.h"
;POLY5_4 = 5
;R1813_POLY4 = 6

    * = $f000, invisible
    !pseudopc $80 {
seqOffs     !byte 0     ;sequence offset in bytes
ptnOffs     !byte 0     ;offset in bytes
ptrOffsEnd  !byte 0     ;end of pattern offset
ptnPtrL     !byte 0     ;temporary
ptnPtrH     !byte 0     ;temporary
rowLenH     !byte 0     ;low value in timer
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

;!macro CreatePoly1 .val, .bits, .tap1, .tap2 {
;    !set .addr = * + (1 << (.bits - 3))
;    !set .pat = 0
;    !for .i, 2, 1 << .bits {
;        !set .pat = (.pat >> 1) | ((.val & 1) * $80)
;        !if (.i & 7) = 0 {
;            * = .addr - (.i >> 3), invisible ; store in reverse order
;            !byte <.pat
;            !set .pat = 0
;        }
;        +NextPoly ~.val, .bits, .tap1, .tap2
;    }
;    * = .addr
;}

!macro CreatePoly5_4 .val5, .val4 {
    !set .length = ((1 << 5) - 1) * ((1 << 4) - 1)
    !set .addr = * + ((.length + 7) >> 3)
    !set .pat = 0
    !for .i, 1+6, .length+6 {
;        !warn .val4 & 1, .i
        !set .pat = (.pat >> 1) | ((.val4 & 1) * $80)
        !if (.i & 7) = 0 {
            * = .addr - (.i >> 3), invisible ; store in reverse order
;            !warn .i, *, <.pat
            !byte <.pat
            !set .pat = 0
        }
        +NextPoly ~.val5, 5, 1, 4
        !if (.val5 & 1) = 1 {
            +NextPoly ~.val4, 4, 1, 2
        }
    }
    * = .addr - ((.i + 7) >> 3), invisible ; store in reverse order
;    !warn .i, *, <.pat
    !byte <.pat
    * = .addr
}

!macro CreateR1813_Poly4 .val4 {
    !set .length = ((1 << 4) - 1) * 31
    !set .addr = * + ((.length + 7) >> 3)
;    !warn *, ", addr ", .addr, ", length ", .length
    !set .pat = 0
    !set .c = 7
    !for .i, 1, 15 {
        !for .j, 1, 13 {
;            !warn .val4 & 1, .i
            !set .pat = (.pat >> 1) | ((.val4 & 1) * $80)
            !set .c = .c + 1
            !if (.c & 7) = 0 {
                * = .addr - (.c >> 3), invisible ; store in reverse order
;                !warn .c, *, <.pat
                !byte <.pat
                !set .pat = 0
            }
        }
        +NextPoly ~.val4, 4, 1, 2
        !for .j, 1, 18 {
;            !warn .val4 & 1, .i
            !set .pat = (.pat >> 1) | ((.val4 & 1) * $80)
            !set .c = .c + 1
            !if (.c & 7) = 0 {
                * = .addr - (.c >> 3), invisible ; store in reverse order
;                !warn .c, *, <.pat
                !byte <.pat
                !set .pat = 0
            }
        }
        +NextPoly ~.val4, 4, 1, 2
    }
;    !set .pat = (.pat >> 1) | ((.val4 & 1) * $80)
    * = .addr - ((.c + 7) >> 3), invisible ; store in reverse order
;    !warn .c, *, <.pat
;    !byte <.pat
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

PatternTbl
;Note: 1st bit of a pattern MUST always be set!
!ifdef SQUARE {
SquarePtn
    !byte   %01010101 }
!ifdef POLY9 {
Poly9Ptn
    +CreatePoly 1, 510, 8, 256 }
!ifdef POLY4 {
Poly4Ptn
    +CreatePoly 1, 14, 1, 8 }
!ifdef R1813 {
R1813Ptn
    !byte   %00000000, %00000111, %11111111, %11111110 }
!ifdef POLY5 {
Poly5Ptn
    +CreatePoly 1, 30, 2, 16 }
!ifdef POLY5_4 {
Poly5_4Ptn
    +CreatePoly5_4 %00001, %0001 }
!ifdef R1813_POLY4 {
R1813_Poly4Ptn
    +CreateR1813_Poly4 %0001 }

PatternPtr
!ifdef SQUARE       { !byte   <SquarePtn }
!ifdef POLY9        { !byte   <Poly9Ptn }
!ifdef POLY4        { !byte   <Poly4Ptn }
!ifdef R1813        { !byte   <R1813Ptn }
!ifdef POLY5        { !byte   <Poly5Ptn }
!ifdef POLY5_4      { !byte   <Poly5_4Ptn }
!ifdef R1813_POLY4  { !byte   <R1813_Poly4Ptn }

InitVal
!ifdef SQUARE       { !byte   $01 }
!ifdef POLY9        { !byte   $02 }
!ifdef POLY4        { !byte   $02 }
!ifdef R1813        { !byte   $02 }
!ifdef POLY5        { !byte   $02 }
!ifdef POLY5_4      { !byte   $40 }
!ifdef R1813_POLY4  { !byte   $40 }

ResetVal
!ifdef SQUARE       { !byte    1-1 }
!ifdef POLY9        { !byte   64-1 }
!ifdef POLY4        { !byte    2-1 }
!ifdef R1813        { !byte    4-1 }
!ifdef POLY5        { !byte    4-1 }
!ifdef POLY5_4      { !byte   59-1 }
!ifdef R1813_POLY4  { !byte   59-1 }

CodeStart
Reset
;reset code adapted from Hard2632
;-                               ;clear TIA regs, most RAM, set SP to $00ff
;    lsr
;    tsx
;    pha
;    bne      -
;    cld

; fixed for 0.3.1
    lda     #0
    tax
    cld                     ; clear BCD math bit
-
    dex
    txs
    pha
    bne     -

!if VISUALS {
;position and size players
    nop
    nop
    dex
    stx     CTRLPF
    sta     RESP0
    stx     NUSIZ0
    stx     NUSIZ1
    stx     REFP0               ;use P1 to reverse direction
    sta     RESP1
}

;relocate player to zeropage
    ldx     #PlayerLength - 1
-
    lda     PlayerCode,x
    sta     VAR_END,x
    dex
    bpl     -

ReadPtn
    sty     saveY
    ldy     ptnOffs             ;saveX and ptnOffs share the same RAM byte!
    stx     saveX
    cpy     ptrOffsEnd          ;if offset at next pattern, play next in sequence
    bne     .skipReadSeq
;read next entry in sequence
    ldy     seqOffs
    ldx     sequence,y
    beq     Reset               ;if 0, loop
    lda     pattern_lookup_lo-1,x
    sta     ptnPtrL
    eor     #$ff                ;CF==1!
    adc     pattern_lookup_lo,x
    sta     ptrOffsEnd          ;begin of next pattern - begin of current pattern
    txa
    lsr
    tax
    lda     pattern_lookup_hi,x
    bcc     .even
    lsr
    lsr
    lsr
    lsr
.even
    ora     #$10                ;bit4 must be set!
    sta     ptnPtrH
    inc     seqOffs
    ldy     #0
.skipReadSeq
    lda     (ptnPtrL),y         ;ctrl byte
    lsr
    sta     rowLenH
    bcs     .noCh0Reload

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
    lax     (ptnPtrL),y         ;note0
    lda     note_table_lo,x
    sta+1   Freq0L
    lda     note_table_hi,x
    sta+1   Freq0H
.noCh0Reload

    lsr     rowLenH
    bcs     .noCh1Reload

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
    lax     (ptnPtrL),y         ;note1
    lda     note_table_lo,x
    sta+1   Freq1L
    lda     note_table_hi,x
    sta+1   Freq1H
.noCh1Reload

    ldx     saveX               ;saveX and ptnOffs share the same RAM byte!
    iny
    sty     ptnOffs
    ldy     saveY
;    clc
    jmp     PlayNote

PlayerCode = *

    !pseudopc VAR_END {         ;actual player runs on zeropage
.loopH                          ;7
    lda     #TEMPO              ;2          define tick length, which is
    sta     T1024T              ;4          constant, even if loop is exited
    dec     rowLenH             ;5
    bpl     PlayNote            ;3/2= 14/13
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

FrequencyStart
!if NTSC {
;$10000 - Frequency * 256 * 256 / (1193181.67 / (88 + 14/256)) * div (div = 2, 15, 31)
    !if MUSIC = 0 { !source "note_table_ntsc.h" }
    !if MUSIC = 1 { !source "note_table_ntsc_2.h" }
    !if MUSIC = 2 { !source "note_table_ntsc_std.h" }
} else {
;$10000 - Frequency * 256 * 256 / (1182298 / (88 + 14/256)) * div (div = 2, 15, 31)
    !if MUSIC = 0 { !source "note_table_pal.h" }
    !if MUSIC = 1 { !source "note_table_pal_2.h" }
    !if MUSIC = 2 { !source "note_table_pal_std.h" }
}
FrequencyEnd

    !zone musicdata
musicData
!if 0 { ;{
sequence
    !byte 1
    !byte 0
pattern_lookup_lo
    !byte <ptn0
    !byte <ptnEnd
pattern_lookup_hi
    !byte >ptn0
ptn0
    !byte $1d, (%1111<<3)|POLY5_4, 3
    !byte $1d, (%1111<<3)|POLY5_4, 2
    !byte $1d, (%1111<<3)|POLY5_4, 1
ptnEnd
} else { ;}
  !if MUSIC = 0 { !source "music.asm" }
  !if MUSIC = 1 { !source "music_2.asm" }
  !if MUSIC = 2 { !source "music_std.asm" }
}

!if DEBUG {
  !ifndef PASS1 {
PASS1
  } else {
    !ifndef PASS2 {
PASS2
  } else {
    !ifndef PASS3 {
PASS3
  }}}

    !zone debug
  !ifdef PASS3 {
    !warn TEMPO, " = TEMPO"
    !warn CodeStart - PatternTbl, "     wave form bytes"
    !warn FrequencyStart - CodeStart, " + code bytes"
    !warn FrequencyEnd - FrequencyStart, "  + frequency bytes"
    !warn FrequencyEnd - $f000, " = player bytes"
    !warn * - musicData, " music bytes"
    !warn $fffc - *, " bytes free"
  }
}

    * = $fffc

    !word   Reset          ; RESET
!if VISUALS {
    !word   $0000          ; IRQ
} else {
    !word   Wait
}
