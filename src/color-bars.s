; C64 User's Guide pp. 58-59 - Automatic color bars
;
; Prints bars of random color.

; *** labels ***

givayf          = $b391         ; 16-bit signed int to FAC1
getadr          = $b7f7         ; FAC1 to unsigned 16-bit int
fmul            = $ba28         ; load float from a/y then FAC1=FAC1*(a/y)
strout          = $ab1e         ; print null terminated string
rnd             = $e097         ; basic's rnd function

; *** main ***

                *=$02a8         ; sys 680

                lda #<clr       ; clear the screen
                ldy #>clr
                jsr strout
mainloop                
                ldy #$01        ; load 1 into FAC1
                lda #$00
                jsr givayf
                jsr rnd         ; call rnd with FAC1 as its argument
                lda #<const8    ; rnd's result is also in FAC1, a floating point value between 0-1
                ldy #>const8    ; multiply this value by 8
                jsr fmul
                jsr getadr      ; convert the result in FAC1 to int
                                ; y is the low byte; should contain a number between 0-7
                lda colortab, y ; index the color table using y    
                sta bar         ; store the value from the table
                lda #<bar       ; print the resulting string
                ldy #>bar
                jsr strout
                jmp mainloop    ; loop forever

; *** data ***

clr             .byte $93, $00  ; clr (clear screen) control code

bar             .byte $20       ; placeholder for color control code
                .byte $12       ; reverse on
                .repeat 5, $20  ; 5 spaces for the bar
                .byte $92       ; reverse off
                .byte $00       ; string terminator

                ; white, red, green, blue, black, purple, yellow, cyan
colortab        .byte $05, $1c, $1e, $1f, $90, $9c, $9e, $9f

const8          .byte $84       ; a constant 8 as a 5-byte floating point number
                .repeat 4, $00