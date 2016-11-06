; C64 User's Guide p. 40 - FOR loop with step .5
;
; Prints the number from 1 to 10 using a step of 0.5

; *** labels ***

strout = $ab1e
prtspc = $ab3b ; print space
prtchr = $ab47
givayf = $b391
faddh  = $b849
fout   = $bddd
movfm  = $bba2
movmf  = $bbd4 
fcomp  = $bc5b
plot   = $fff0

; *** main ***

                *=$033e         ; sys 830 ($33C-$3FB TBUFFER)

                ; fac = 0.5 (0.5 constant at $bf11)
                lda #$11
                ldy #$bf
                jsr movfm
                
                ; fac = fac + 0.5
loop            jsr faddh
                
                ; savefac <- fac
                ldx #<savefac
                ldy #>savefac
                jsr movmf
                
                ; fac to string + print
                jsr fout
                jsr strout
                
                ; get cursor pos
                sec
                jsr plot
                stx row
                sty column
                
                ; column < 10 -> jump to column 10
                lda #$0a
                sec
                sbc column
                bmi col20
                ldy #$0a
                jmp setxy
                
                ; column < 20 -> jump to column 20
col20           lda #$14
                sec
                sbc column
                bmi col30
                ldy #$14
                jmp setxy
                
                ; column < 30 -> jump to column 30
col30           lda #$1e
                sec
                sbc column
                bmi col00
                ldy #$1e
                jmp setxy
                
                ; column > 30 -> move to the beginning of the next row
col00           ldy #$00
                inc row
                ldx row
                cpx #$19        ; row > 24?
                bne setxy       ; no -> jump
                dec row         ; yes -> decrease row
                lda #$0d        ; print a carriage return to scroll the screen
                jsr prtchr
                ldy #$00        ; set column to 0
                
                ; set cursor position
setxy           ldx row
                clc
                jsr plot
                
                ; fac <- savefac
                lda #<savefac
                ldy #>savefac
                jsr movfm
                
                ; fac == const10
                lda #<const10
                ldy #>const10
                jsr fcomp
                cmp #$00        ; if a == 0 then fac equals const10
                bne loop
                
                rts
                
; *** data ***

const10         .byte $84, $20, $00, $00, $00
savefac         .repeat 5, $00
row             .byte $00
column          .byte $00

