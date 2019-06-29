; Switch to VIC bank #3, copy character generator ROM to RAM
; and print the first 128 characters.
;

; *** labels ***
hibase          = $288          ; top page for screen memory
ci1icr          = $dc0d         ; CIA1 interrupt control register
ci1cra          = $dc0e         ; CIA1 control register A
ci2pra          = $dd00         ; CIA2 data port register A
vmcsb           = $d018         ; VIC memory control register
chrout          = $ffd2         ; output a byte in A
plot            = $fff0         ; set cursor position if carry clear / get position if carry set

; *** main ***

                *=$c000         ; sys49152
                sei
                lda #%01111111  ; $7f - turn off the system timer interrupt
                sta ci1icr
                lda ci1cra
                and #%11111110  ; $fe - bit #0=0 stops CIA1 timer A (system timer)
                sta ci1cra
                lda $01
                and #%11111011  ; select character generator ROM at $d000
                sta $01

                jsr copy
                
                lda $01
                ora #%00000100  ; select I/O instead of chargen ROM
                sta $01
                lda ci1cra
                ora #%00000001  ; enable system timer
                sta ci1cra
                lda #%10000001  ; enable system timer interrupt
                sta ci1icr

                lda ci2pra
                and #%11111100  ; bit 0-1 select VIC bank - select bank #3
                sta ci2pra

                lda #%00111100  ; bit 7-4: 0011 -> screen ram offset = 3*1k = $c00
                sta vmcsb       ; bit 3-1: 110 -> 4*2k = $3000 chargen offset
                                ; as we are in bank #3: screen=$cc00; chargen= $f000

                lda #$cc        ; tell the OS that the screen RAM is at page 204 ($cc)
                sta hibase
                cli

                lda #$93
                jsr chrout      ; clear the screen

                ldy #$00        ; put the first 128 characters on the screen
                sty $fb
                lda #$cc
                sta $fc
loop            tya
                sta ($fb),y
                iny
                cpy #$80
                bne loop

                clc
                ldx #10
                ldy #0
                jsr plot

                rts

; *** subroutines ***

; name:         copy
; description:  copy the character generator ROM from $d000 to $f000-$ffff
; input:        -
; output:       -
copy
.block
                lda #$00
                sta $fb
                sta $fd
                lda #$d0
                sta $fc
                lda #$f0
                sta $fe
                ldx #$10
a00
                ldy #$00
a01
                lda ($fb),y
                sta ($fd),y
                dey
                bne a01
                inc $fc
                inc $fe
                dex
                bne a00
                rts
.bend
