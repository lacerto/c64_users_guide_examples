; C64 User's Guide p. 40 - FOR loop with step .5
;
; Prints the number from 1 to 10 using a step of 0.5

; *** labels ***

strout          = $ab1e         ; print null terminated string
prtspc          = $ab3b         ; print space
prtchr          = $ab47         ; print a single character in a
givayf          = $b391         ; convert 16-bit signed integer to floating point (FAC1)
faddh           = $b849         ; add 0.5 to FAC1
fout            = $bddd         ; converts FAC1 to string, pointer to the string in a/y
movfm           = $bba2         ; move floating point num from a/y to FAC1
movmf           = $bbd4         ; save FAC1 to x/y as a 5-byte floating point num
fcomp           = $bc5b         ; compare FAC1 to a floating point num at a/y
plot            = $fff0         ; read/set cursor location

; *** main ***

                *=$0334         ; sys820 ($33C-$3FB TBUFFER)
                
                lda #$11        ; 0.5 floating point constant at $bf11
                ldy #$bf
                jsr movfm       ; initialize FAC1=0.5
                                
loop            jsr faddh       ; FAC1+=0.5
                
                ldx #<savefac   ; save FAC1
                ldy #>savefac
                jsr movmf
                
                jsr fout        ; convert FAC1 to string
                jsr strout      ; and print it
                                
                sec             ; get cursor pos
                jsr plot
                stx row         ; and store it
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
                                
setxy           ldx row         ; set cursor position
                clc
                jsr plot
                                
                lda #<savefac   ; restore FAC1
                ldy #>savefac
                jsr movfm
                
                lda #<const10   ; pointer to constant 10
                ldy #>const10
                jsr fcomp
                cmp #$00        ; FAC1 == 10?
                bne loop        ; no -> next iteration
                
                rts
                
; *** data ***

const10         .byte $84, $20, $00, $00, $00   ; floating point constant 10
savefac         .repeat 5, $00                  ; reserving mem for saving FAC1
row             .byte $00                       ; current screen row
column          .byte $00                       ; current screen column

