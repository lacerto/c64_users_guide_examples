; C64 User's Guide p. 39 - For ... Next Loops
;
; Implement a loop
; Prints text a couple of times.

; *** labels ***

strout          = $ab1e


; *** main ***

                *=$02a8         ; sys 680
                
                ldx #$00        ; x = $00                
loop            stx index       ; save x in mem
                lda #<str       ; string pointer low byte
                ldy #>str       ; string pointer high byte
                jsr strout      ; print str; destroys register values!
                ldx index       ; load counter back into x
                inx             ; x++
                cpx count       ; x == count
                bne loop        ; not equal -> jump to loop
                rts
    
                
; *** data ***

str             .text "commodore 64"
                .byte $0d, $00      ; carriage return, null terminated string
                
count           .byte $05           ; print text 5 times                
                
index           .byte $00           ; store the x register here
