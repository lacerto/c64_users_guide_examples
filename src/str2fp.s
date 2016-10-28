; Convert a strint to floating point.

; *** labels ***
inlin  = $a560 ; read a line into the basic buffer
strout = $ab1e ; print 0 terminated string
prtqm  = $ab45 ; print "?"
prtspc = $ab3b ; print space
s2fac1 = $b7b5 ; convert string to floating point, store in fac1
fac12s = $bddd ; convert fac1 to string
stf1m  = $bbd4 ; store fac1 in the memory
ldfac1 = $bba2 ; load fac1 from memory
prtchr = $ab47 ; print char in a

dst    = $fb   ; zp pointer for converting to hex
    
; *** macros ***

; name:         convstrfac1
; description:  this macro converts a 0 terminated string to a floating point
;               number and stores it in fac1.
;               the 5 bytes of fac1 are then stored at the given location.
; input:        \1 - address of the null terminated string
;               \2 - address where fac1 will be stored
; output:       the 5 bytes of fac1 stored at \2

convstrfac1 .macro    
    ldx #<\1
    ldy #>\1    
    stx $22
    sty $23   
    jsr s2fac1
    
    ; store fac1
    ldx #<\2
    ldy #>\2
    jsr stf1m
.endm    

printhex .macro
    ; convert fac1 bytes to hex string
    lda #<\2
    sta dst
    lda #>\2
    sta dst+1    
        
    ldx #$00
writenum    
    lda \1,x
    tay
    txa
    pha
    tya
    ldy #$00
    jsr hexify
    lda #"$"
    jsr prtchr
    lda #<\2
    ldy #>\2
    jsr strout
    jsr prtspc
    pla
    tax
    inx
    cpx #\3
    bne writenum
.endm

; *** main ***

    *=$0801
    
    ; 10 sys 2064    
    .byte $0c,$08,$0a,$00
    .byte $9e,$20,$32,$30
    .byte $36,$34,$00,$00
    .byte $00    

    *=$0810         ; sys2064

    ; print intro text
    jsr intro
    
    ; get user input into a buffer
    lda #<strbuf
    ldy #>strbuf
    jsr getinput
    
    ; convert the string to a floating point number and
    ; store its exp/manitssa representation
    #convstrfac1 strbuf, fpnum
    
    lda #<header
    ldy #>header
    jsr strout
    
    ; print the floating point number's bytes one by one as hex numbers
    #printhex fpnum, hexstr, 5
    
    ; print "="
    lda #"="
    jsr prtchr
    jsr prtspc
    
    ; load fac1 from memory, convert it to string and print it
    lda #<fpnum
    ldy #>fpnum    
    jsr ldprtfac1
    
    ; carriage return
    lda #$0d
    jsr prtchr
    rts
    
; *** subroutines ***

; name:        intro
; description: prints introductory text
; input: -
; output: -

intro
    lda #<introtext
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
copystr    
    lda $0200,y     ; get a byte from $0200+y
    beq endcp       ; str is 0 terminated. 0 reached? then end
    sta ($fb),y     ; copy one byte to the buffer
    lda #$00
    sta $0200,y     ; zero out the basic buffer to avoid a syntax error
    iny             ; y++
    jmp copystr     ; copy next byte
endcp
    sta ($fb),y     ; terminate the buffer with a 0 byte
    
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
         
ldprtfac1
.block
    ; load fac1
    jsr ldfac1
    
    ; convert fac1 to string and print
    jsr fac12s
    jsr strout    

    rts
.bend
    
hexify
.block
    tax
	lsr
	lsr
	lsr
	lsr
	jsr hexc        ; convert upper nybble
	jsr output
	txa
	and #$0f		; convert lower nybble
	jsr hexc
	jsr output
	lda #$00        ; terminate string with 0
	sta (dst),y
	rts

output	
    sta (dst),y     ; output a byte using a zp-ptr and Y-index
    iny             ; increment the output address
    rts
    
hexc
    cmp #$0a		; subroutine converts 0-F to a character
	bcs hexa
	clc             ; digit 0-9
	adc #$30        ; "0"
	bne hexb        ; unconditional jump coz Z=FALSE always
hexa
    clc
	adc #$37   		; digit A-F
hexb
    rts
.bend    

; *** data ***    

introtext
    .byte $0d
    .text "this utility displays fac1"
    .byte $0d
    .text "please enter a number"
    .byte $0d, $00

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
    
hexsign
    .text "$"

hexstr
    .repeat 3, $00

fpnum 
    .repeat 5, $00
    
strbuf

