; Filtered string input.
; Allows only enetering the specified characters and limits the maximum length.

; *** labels ***

getin           = $ffe4         ; get one byte from the input device in A
chrout          = $ffd2         ; output a byte (A)
strout          = $ab1e         ; print str in A/Y
prcrlf          = $aad7         ; print cr/lf
blnsw           = $cc           ; cursor blink enable (0 = flash cursor)

; *** main ***

                *=$c000         ; sys49152
                lda #<filter
                ldy #>filter
                ldx #$10
                jsr input

                jsr prcrlf
                lda #<inputstr
                ldy #>inputstr
                jsr strout
end             rts             ; return

; *** subroutines ***

; name:         input
; description:  This input routine allows only the input of characters defined in a filter list.
;               The maximum allowed number of characters input can also be specified.
;               Note: the null terminating the input string is not included in the maximum length!
;               Thus, the resulting string needs a maximum of maxchars+1 bytes of storage.
; input:        X - the maximum allowed length of the input string
;               A/Y - pointer to the filter list, a null terminated string containing the allowed chars
;                     allows to use different filters at each call
; output:       the input string at the inputstr address (null terminated, ready to print)
; see also/thx: https://codebase64.org/doku.php?id=base:robust_string_input
input
.block
                stx maxchars        ; store the max number of characters allowed
                sta filtertext+1    ; self modifying code
                sty filtertext+2
                lda #<prompt        ; print prompt
                ldy #>prompt
                jsr strout
                lda #$00            ; characters received
                sta charcount
                sta blnsw           ; enable cursor blink
inputloop       
                jsr getin           ; get a character from the input device
                beq inputloop       ; nothing received, loop

                cmp #$14            ; handle delete
                beq delete
                cmp #$0d            ; handle return
                beq done

                ldy charcount       ; max length reached?
                cpy maxchars        ; then loop and allow only delete and return
                beq inputloop

                sta lastchar        ; store the last character received

                ldx #$00            ; index in the filter text
filtertext      lda $ffff,x         ; $ffff is overwritten! see above
                beq inputloop       ; end of filter list reached (null terminated)
                cmp lastchar        ; compare to the last character
                beq inputok         ; char found in the filter list -> OK
                inx                 ; X++
                jmp filtertext      ; loop

inputok
                lda lastchar        ; get the last character
                ldy charcount       ; count functions as the index
                sta inputstr,y      ; store the character
                jsr chrout          ; echo it on the screen
                cpy maxchars        ; max length reached?
                beq inputloop       ; yes -> back to the main input loop
                inc charcount       ; no -> count++
                jmp inputloop       ; back to the main input loop

delete
                lda charcount
                bne dodel           ; count != 0 then perform delete
                jmp inputloop       ; otherwise back to the main loop
dodel
                dec charcount       ; count--
                ldy charcount
                lda #$00
                sta inputstr,y      ; zero out the current char in the string
                lda #$14            ; print delete
                jsr chrout
                jmp inputloop

done
                ldy charcount       ; Y = count
                beq firstchar       ; Y == 0 then jump
                iny                 ; Y++
firstchar       lda #$00            ; add the terminating null
                sta inputstr,y
                rts
.bend

; *** data ***

prompt          .null "? "
filter          .null " abcdefghijklmnopqrstuvwxyz1234567890"
maxchars        .byte $00
charcount       .byte $00
lastchar        .byte $00

inputstr        ; store the input string here
