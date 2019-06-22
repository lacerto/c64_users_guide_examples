; Convert a string to a floating point number and display
; its exponent/mantissa representation.

; *** labels ***

inlin  = $a560 ; read a line into the basic buffer
strout = $ab1e ; print 0 terminated string
prtqm  = $ab45 ; print "?"
prtspc = $ab3b ; print space
val    = $b7b5 ; convert string to floating point, store in fac1
fout   = $bddd ; convert fac1 to string, sets pointer to string in a/y 
mov2f  = $bbd4 ; store fac1 in the memory
movfm  = $bba2 ; load fac1 from memory
prtchr = $ab47 ; print char in a

; *** macros ***

; name:         convstrfac1
; description:  this macro converts a 0 terminated string to a floating point
;               number and stores it in fac1.
;               the 5 bytes of fac1 are then stored at the given location.
; input:        a  - length of the string
;               \1 - address of the null terminated string
;               \2 - address where fac1 will be stored
; output:       the 5 bytes of fac1 stored at \2

convstrfac1 .macro    
                ldx #<\1
                ldy #>\1    
                stx $22
                sty $23   
                jsr val     ; input: a - strlen; $22/$23 - ptr to string
                
                ; store fac1
                ldx #<\2
                ldy #>\2
                jsr mov2f
.endm    

; name:         printhex
; description:  this macro converts a given number of bytes to 2 digit hex 
;               strings and prints them one by one
; input:        \1 - pointer to the one byte integer to be printed as hex str
;               \2 - pointer to a 3 byte buffer for storing the converted
;                    2 digit hex string and its terminating 0

printhex .macro
                ldx #$00        ; x = 0
writenum        txa
                pha
                lda \1,x        ; get a byte from the floating point number
                ldx #<\2
                ldy #>\2
                jsr hexify
                lda #"$"        ; print the $ character
                jsr prtchr
                lda #<\2        ; load the ptr to the hex string
                ldy #>\2
                jsr strout      ; print the hex string
                jsr prtspc      ; print a space
                pla             ; get back a from the stack (the counter value)
                tax             ; a -> x
                inx             ; increase counter
                cpx #\3         ; value in param 3 reached?
                bne writenum    ; no - convert the next byte
.endm


; *** main ***

                *=$0801
                
                ; 10 sys 2064    
                .byte $0c,$08,$0a,$00
                .byte $9e,$20,$32,$30
                .byte $36,$34,$00,$00
                .byte $00    
                
                *=$0810         ; sys 2064
                
                ; print intro text
                jsr intro
                
                ; get user input into a buffer
                lda #<strbuf
                ldy #>strbuf
                jsr getinput    ; a = length of string
                
                ; convert the string to a floating point number and
                ; store its exp/manitssa representation
                #convstrfac1 strbuf, fpnum
                
                ; print the header
                lda #<header
                ldy #>header
                jsr strout
                
                ; print the floating point number's bytes one by one
                ; as hex numbers
                #printhex fpnum, hexstr, 5
                
                ; print "=" and a space
                lda #"="
                jsr prtchr
                jsr prtspc
                
                ; load fac1 from memory, convert it to string and print it
                lda #<fpnum
                ldy #>fpnum    
                jsr ldprtfac1
                
                ; print a carriage return
                lda #$0d
                jsr prtchr
                rts

                
; *** subroutines ***

; name:        intro
; description: prints introductory text
; input: -
; output: -

intro           lda #<introtext
                ldy #>introtext
                jsr strout
                rts
    
; name:         getinput
; description:  gets user input using basic inlin and copies it to a buffer
; input:        a - buffer pointer low byte
;               y - buffer pointer high byte
; output:       a - length of the input string
;               0 terminated string beginning at the buffer pointer
; uses:         a, x, y  - not restored after return
;               $fb, $fc - these are restored after return

getinput
.block
                ; push the values of $fb and $fc to the stack
                tax         ; a -> x
                lda $fb
                pha
                lda $fc
                pha
            
                ; store the buffer pointer on the zero page
                txa         ; x -> a
                sta $fb
                sty $fc
            
                ; print question mark and space line input
                jsr prtqm
                jsr prtspc
            
                ; get user input into the basic input buffer at $0200
                jsr inlin
                
                ; copy the string from $0200 to the buffer
                ldy #$00        ; y=0; this will be the index                
copystr         lda $0200,y     ; get a byte from $0200+y
                beq endcopy     ; str is 0 terminated. 0 reached? then end
                sta ($fb),y     ; copy one byte to the buffer
                lda #$00
                sta $0200,y     ; 0 out the basic buffer to avoid a syntax error
                iny             ; y++
                jmp copystr     ; copy next byte
endcopy         sta ($fb),y     ; terminate the buffer with a 0 byte
                
                ; restore $fb and $fc from the stack
                pla
                sta $fc
                pla
                sta $fb
                
                ; y contains the length of the string
                ; transfer this to a and return
                tya
                rts
.bend    

; name:         ldprtfac1
; description:  loads a number from the specified memory location into fac1,
;               then converts fac1 to string and prints it
; input:        a - pointer to 5 byte floating point number (low byte)
;               y - pointer to 5 byte floating point number (high byte)
; output:       -

ldprtfac1
.block
                ; load fac1
                jsr movfm
                
                ; convert fac1 to string and print
                jsr fout        ; pointer to string is in a/y
                jsr strout      ; thus strout can be called immediately
            
                rts
.bend

; name:         hexify
; description:  
; input:        a - this value will be converted to a hex string
;               x - ptr to a 3 byte hex string buffer (low)
;               y - ptr to a 3 byte hex string buffer (high)
; output:       -
; uses:         $fb and $fc
; note:         taken from codebase64 - many thanks
;               http://codebase64.org/doku.php?id=base:integer_to_hex_string

hexify
.block
                stx $fb
                sty $fc
                ldy #$00
                tax             ; a -> x
                lsr
                lsr
                lsr
                lsr             ; upper nybble -> lower nybble 
                jsr hexc        ; convert upper nybble
                jsr output      ; and store it in the buffer
                txa             ; x -> a; the original value of a
                and #$0f		; zero out the upper nybble
                jsr hexc        ; convert lower nybble
                jsr output      ; store it in the buffer
                lda #$00        ; terminate string with 0
                sta ($fb),y
                rts
            
output          sta ($fb),y     ; output a byte using a zp-ptr and y-index
                iny             ; increment the output address
                rts
                
hexc            cmp #$0a		; subroutine converts 0-f to a character
                bcs hexa
                clc             ; digit 0-9
                adc #$30        ; "0"
                bne hexb        ; unconditional jump as z=0 always
hexa            clc
                adc #$37   		; digit a-f
hexb            rts

.bend    


; *** data ***    

introtext
    .byte $0d
    .text "this utility displays fac1"
    .byte $0d
    .text "please enter a number"
    .byte $0d, $00

; EXP MA3 MA2 MA1 MA0 in reverse    
header
    .byte $0d
    
    .byte $12 ; reverse on
    .text "exp"
    .byte $92 ; reverse off
    .byte $20
    
    .byte $12 ; reverse on
    .text "ma3"
    .byte $92 ; reverse off
    .byte $20
    
    .byte $12 ; reverse on
    .text "ma2"
    .byte $92 ; reverse off
    .byte $20
    
    .byte $12 ; reverse on
    .text "ma1"
    .byte $92 ; reverse off
    .byte $20
    
    .byte $12 ; reverse on
    .text "ma0"
    .byte $92 ; reverse off
    .byte $20
    
    .byte $0d, $00

; buffer for a 0 terminated hexa string    
hexstr
    .repeat 3, $00

; location for the 5 byte floating point number    
fpnum 
    .repeat 5, $00
    
; string buffer    
strbuf

