; Get a byte parameter from BASIC text and convert it to string and print it.
; 
; Purpose: test byte to string conversion.

; *** labels ***

chrgot          = $79       ; read current BASIC text character again
strout          = $ab1e     ; print str in A/Y
chkcom          = $aefd     ; check for and skip comma; SYNTAX ERROR if the next char is not a comma
getbyt          = $b79e     ; get a byte parameter from BASIC text into X
                            ; (TYPE MISMATCH if not a number, ILLEGAL QUANTITY if not in 0-255,
                            ; floating point numbers are truncated)

; *** main ***

                *=$0334         ; sys820
getbyte         jsr chrgot      ; what was the last character again?
                beq eolerror    ; if 0 then it is the end of the line -> parameter missing				
                jsr chkcom      ; check comma
                jsr getbyt      ; get a byte parameter (stored in X)
                txa
                jsr byt2str     ; convert A to null terminated string and store it at str
                lda #<str       ; print the result
                ldy #>str
                jsr strout
end             rts             ; return

; Parameter missing error.	
eolerror        lda #<eolerrstr ; string addr low byte
                ldy #>eolerrstr ; string addr high byte
                jsr strout      ; print str in A/Y
                jmp end         ; jump to end

; *** subroutines ***

; name:         byt2str
; description:  convert a byte to a null terminated right aligned string with leading spaces
; input:        a byte in A
; output:       a null terminated string at str
; see also/thx: https://codebase64.org/doku.php?id=base:tiny_.a_to_ascii_routine
byt2str
.block
                ldy #$2f
                ldx #$3a
                sec             ; set carry
a01             iny             ; Y++
                sbc #100        ; A -= 100
                bcs a01         ; carry still set? -> jump back
a02             dex             ; X--
                adc #10         ; A += 10
                bmi a02         ; negative? -> next iteration
                adc #$2f        ; after the last adc #10 operation the carry will be set, so add only $2f
                                ; Y = hundreds, X = tens, A = ones
                cpy #$30        ; store space(s) instead of one or two leading zeroes
                bne a03
                ldy #$20
a03             sty str
                cpx #$30
                bne a04
                ldx #$20
a04             stx str+1
                sta str+2
                lda #$00        ; null terminated string
                sta str+3                
                rts                
.bend

; *** data ***

eolerrstr		.byte $0d
                .null "?parameter missing  error"

str
