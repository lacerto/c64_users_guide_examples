; C64 User's Guide p. 39 - For ... Next Loops
;
; Implement a loop
; Prints text a couple of times.

; *** labels ***

strout          = $ab1e


; *** main ***

                *=$02a8         ; sys 680
                
                lda #$00        ; a = $00                
loop            pha             ; push a onto the stack
                lda #<str       ; string pointer low byte
                ldy #>str       ; string pointer high byte
                jsr strout      ; print str; destroys register values!
                pla             ; pull a from the stack
                clc             ; clear carry
                adc #$01        ; a += $01
                cmp count       ; a == count ?
                bne loop        ; not equal -> jump to loop
                rts
    
                
; *** data ***

str             .text "commodore 64"
                .byte $0d, $00      ; carriage return, null terminated string
                
count           .byte $05           ; print text 5 times
