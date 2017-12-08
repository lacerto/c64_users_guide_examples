; C64 User's Guide pp. 58-59 - Automatic color bars
;
; Prints bars of random color.

; *** labels ***

givayf          = $b391
getadr          = $b7f7
fmul            = $ba28
strout          = $ab1e
rnd             = $e097

; *** main ***

                *=$02a8         ; sys 680

                lda #<clr
                ldy #>clr
                jsr strout
mainloop                
                ldy #$01
                lda #$00
                jsr givayf
                jsr rnd
                lda #<const8
                ldy #>const8
                jsr fmul
                jsr getadr
                lda colortable, y
                sta bar
                lda #<bar
                ldy #>bar
                jsr strout
                jmp mainloop
                rts

; *** data ***

clr             .byte $93, $00

bar             .byte $20       ; placeholder for color control code
                .byte $12       ; reverse on
                .repeat 5, $20  ; 5 spaces for the bar
                .byte $92       ; reverse off
                .byte $00       ; string terminator

colortable      .byte $05, $1c, $1e, $1f, $90, $9c, $9e, $9f

const8          .byte $84
                .repeat 4, $00