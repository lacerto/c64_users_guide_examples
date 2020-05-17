; Show a character from chargen
;
; Usage:
;   SYS49152,x
;
; where x is a screen code in the range 0-255. The corresponding character
; will be printed as an 8x8 matrix.

; *** labels ***
hibase          = $288          ; top page for screen memory
ci1icr          = $dc0d         ; CIA1 interrupt control register
ci1cra          = $dc0e         ; CIA1 control register A
ci2pra          = $dd00         ; CIA2 data port register A
vmcsb           = $d018         ; VIC memory control register
chrout          = $ffd2         ; output a byte in A
strout          = $ab1e         ; print str in A/Y
chrgot          = $79           ; read current BASIC text character again
chkcom          = $aefd         ; check for and skip comma; SYNTAX ERROR if the next char is not a comma
getbyt          = $b79e         ; get a byte parameter from BASIC text into X
                                ; (TYPE MISMATCH if not a number, ILLEGAL QUANTITY if not in 0-255,
                                ; floating point numbers are truncated)

; *** main ***

                *=$c000         ; sys49152
                jsr chrgot      ; what was the last character again?
                beq eolerror    ; if 0 then it is the end of the line -> parameter missing				
                jsr chkcom      ; check comma
                jsr getbyt      ; get a byte parameter (stored in X)
                stx screencode  ; store the screen code
                jmp switchrom   ; switch in the chargen ROM

; Parameter missing error.	
eolerror        lda #<eolerrstr ; string addr low byte
                ldy #>eolerrstr ; string addr high byte
                jsr strout      ; print error message
                jmp end         ; jump to end

switchrom
                sei
                lda #%01111111  ; $7f - turn off the system timer interrupt
                sta ci1icr
                lda ci1cra
                and #%11111110  ; $fe - bit #0=0 stops CIA1 timer A (system timer)
                sta ci1cra
                lda $01
                and #%11111011  ; select character generator ROM at $d000
                sta $01

                lda #$00
                sta $fb
                sta $fc

                ; as all characters consist of 8 bytes, we have to multiply
                ; the screen code by 8 = 2^3
                ; the result is a 16-bit integer
                ldx #$03        ; shift the screen code 3 times to the left
                lda screencode
multiply        asl             ; low byte shift left (lsb is always 0)
                rol $fc         ; high byte shift left using carry, lsb is the carry
                dex             ; x--
                bne multiply    ; 0 reached? no -> goto multiply
                sta $fb         ; store the low byte

                clc             ; add $d0 to the high byte
                lda $fc
                adc #$d0
                sta $fc 

                ; copy the character (8 bytes) from chargen
                lda #<char
                sta $fd
                lda #>char
                sta $fe
                jsr copy
                
                ; switch out chargen, enable I/O again
                lda $01
                ora #%00000100  ; select I/O instead of chargen ROM
                sta $01
                lda ci1cra
                ora #%00000001  ; enable system timer
                sta ci1cra
                lda #%10000001  ; enable system timer interrupt
                sta ci1icr
                cli

                ; print character as 8x8 matrix
                ldy #$00
loop            lda ($fd),y
                ldx #$08
a00             asl
                bcs bithigh
                pha
                lda #$a6
                jsr chrout
                pla
                dex 
                bne a00
                jmp next
bithigh         pha
                lda #$20
                jsr chrout
                pla
                dex
                bne a00
next            lda #$0d
                jsr chrout
                iny
                cpy #$08
                bne loop
end             rts

; *** subroutines ***

; name:         copy
; description:  copy a character from the character generator ROM
; input:        -
; output:       -
copy
.block
                ldy #$00
a01
                lda ($fb),y
                sta ($fd),y
                iny
                cpy #$08
                bne a01
                rts
.bend

; *** data ***

eolerrstr		.byte $0d
                .null "?parameter missing  error"

screencode      .byte $00
char            .repeat 8, $00
