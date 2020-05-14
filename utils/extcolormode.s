; Extended background color text mode test
;
; Switch to extended background color text mode and
; write the available 64 characters with 4 background
; colors to the screen.

; BASIC/KERNAL variables and pointers
pnt             = $d1           ; $d1/$d2 points to the address of the beginning of the current screen line

; VIC registers
scroly          = $d011         ; VIC vertical fine scrolling & control register
bgcol0          = $d021         ; background color 0 (default 6 blue)
bgcol1          = $d022         ; background color 1 (default 1 white)
bgcol2          = $d023         ; background color 2 (default 2 red)
bgcol3          = $d024         ; background color 3 (default 3 cyan)

; KERNAL routines
plot            = $e50a         ; set cursor position if carry clear / get position if carry set
clrscr          = $e544         ; initialize the screen link table and clear the screen
scradr          = $e9f0         ; set the address of the screen line in x to $d1/$d2

; *** main ***

                *=$033e         ; sys830 (TBUFFER $033c-$03fb)

                ; set the background colors
                lda #$00
                sta bgcol1
                lda #$02
                sta bgcol2
                lda #$0b
                sta bgcol3

                ; switch to extended background color text mode
                lda scroly
                ora #%01000000
                sta scroly

                ; clear the screen
                jsr clrscr

                ; set character color for the first 8 lines to white in color RAM
                lda #$00
                sta $fb
                lda #$d8
                sta $fc

                ldy #$00
                lda #$01
a00             sta ($fb),y
                iny
                bne a00

                inc $fc
a01             sta ($fb),y
                iny
                cpy #$40
                bne a01

                ; first 64 chars with bgcolor 0
                ldx #$00        ; get the address of line 0 in pnt
                jsr scradr
                lda #$00        ; store the screen code offset in $fc
                sta $fc
                jsr putchar

                ; next 64 chars with bgcolor 1
                ldx #$02
                jsr scradr
                lda #$40
                sta $fc
                jsr putchar

                ; next 64 chars with bgcolor 2
                ldx #$04
                jsr scradr
                lda #$80
                sta $fc
                jsr putchar

                ; next 64 chars with bgcolor 3
                ldx #$06
                jsr scradr
                lda #$c0
                sta $fc
                jsr putchar

                ; set the cursor position to the beginning of line 8
                clc
                ldx #$08
                ldy #$00
                jsr plot

                rts

; *** subroutines ***

putchar
.block
                ldy #$00
a00             sty $fb
                lda $fc         ; get the offset
                clc
                adc $fb         ; add y to the offset
                sta (pnt),y     ; put the character to the screen memory
                iny
                cpy #$40        ; y == 64
                bne a00         ; no -> loop
                rts
.bend          