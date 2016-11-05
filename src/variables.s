; C64 User's Guide p. 36 - Variables
;
;

; *** labels ***

inlin           = $a560
strout          = $ab1e
prtqm           = $ab45 ; print "?"
prtspc          = $ab3b ; print space
prtchr          = $ab47 ; print char in a
movfm           = $bba2 ; load fac1 from memory
movint          = $a9ca ; load an integer value into a variable
fout            = $bddd ; convert fac1 to string
linprt          = $bdcd ; x/a
movfvar         = $bbd0 ; fac -> variable
frestr          = $b6a3 ; evaluate string
strvec          = $b475 ; copy string pointer and make string space A bytes long

varnam          = $45

; name:         createfloatvar
; description:  calls the necessary subroutines to create a float variable
;               and loads it with the given value
; input:        \1 - variable name first char
;               \2 - variable name second char
;               \3 - 5 byte floating point number (exponent-mantissa)
; output:       -

createfloatvar .macro
                lda #\1
                ldy #\2
                jsr floatvaraddr    ; create variable
                
                lda #<\3
                ldy #>\3
                jsr movfm           ; num -> FAC
                jsr movfvar         ; FAC -> variable
.endm

; name:         createintvar
; description:  calls the necessary subroutines to create an integer variable
;               and loads it with the given value
; input:        \1 - variable name first char + $80
;               \2 - variable name second char + $80
;               \3 - two-byte integer (low / high)
; output:       -

createintvar .macro
                lda #\1
                ldy #\2
                jsr intvaraddr      ; create variable
                
                lda #<\3
                ldy #>\3
                sta $fb             ; store the pointer to the two-byte                                    
                sty $fc             ; int on the zp
                ldy #$01
                lda ($fb),y
                sta $64             ; store the integer in $64 & $65
                dey                 ; in high byte - low byte order
                lda ($fb),y
                sta $65                
                jsr movint          ; int -> variable
.endm

; name:         createstrvar
; description:  calls the necessary subroutines to create a string variable
;               and loads it with the given value
; input:        \1 - variable name first char
;               \2 - variable name second char + $80
;               \3 - string length
;               \4 - pointer to the string
; output:       -

createstrvar .macro
                ; create variable x$
                lda #\1
                ldy #\2
                jsr strvaraddr      ; create variable
                jsr frestr
                lda #\3
                jsr strvec          ; pass the length to strvec
                ldy #$02
a0              lda $0061,y         ; strvec stores the length at $61
                sta ($49),y         ; and the pointer to the str at $62/63
                dey                 ; copy this to the variable descriptor
                bpl a0
                iny
a1              lda @4,y            ; copy the string to the area pointed 
                sta ($62),y         ; by $62/63
                iny
                cpy $61             ; length reached?
                bne a1              ; no, copy next byte
.endm


; *** main ***

                *=$c000         ; sys 49152
                
                ; create floating point variable x
                ; and assign it a value
                ; $58 = x
                ; $00 = no second character
                #createfloatvar $58, $00, fpnum

                ; create integer variable x%
                ; and assign it a value    
                ; $d8 = $80 + $58 -> "x"+$80
                ; $80 = $80 + $00 -> no second character
                #createintvar $d8, $80, intnum
                
                ; create string variable x$
                ; and assign it a value
                ; $58 = x
                ; $80 = $80 + $00 -> no second character
                #createstrvar $58, $80, strend-str, "str"
                
                ; print text
                lda #<strxint
                ldy #>strxint
                jsr strout
                
                ; print the integer variable's value
                lda #"X"
                ldy #$80
                jsr printintvar
                
                ; print a space
                jsr prtspc
                
                ; print text
                lda #<strxfloat
                ldy #>strxfloat
                jsr strout
                
                ; print the floating point variable's value
                lda #"x"
                ldy #$00
                jsr printfloatvar

                ; print a carriage return
                lda #$0d
                jsr prtchr
                
                ; print the string variable's value                
                lda #"x"
                ldy #$80
                jsr printstrvar
                                
                ; calculate x% + x and print result
                
                lda #"X" 
                ldy #$80
                jsr intvaraddr
                
                ldy #$00
                lda ($49),y
                pha
                iny
                lda ($49),y
                tay
                pla
                jsr $b391       ; 16 bit int -> fac
                jsr $bc0c       ; fac -> arg
                
                lda #"x"
                ldy #$00
                jsr floatvaraddr
                jsr $bba2       ; var -> fac                
                
                jsr $b86a       ; fac = fac + arg
                jsr fout
                jsr strout                
                
                rts     
                
; *** subroutines ***
                
printintvar     jsr intvaraddr
                ldy #$01
                lda ($49),y
                tax
                dey
                lda ($49),y
                jsr linprt
                rts
                
printfloatvar   jsr floatvaraddr
                jsr $bba2       ; var -> fac
                jsr fout
                jsr strout
                rts
                
printstrvar     
.block
                jsr strvaraddr
                ldy #$00
                lda ($49),y
                sta $fd
                iny
                lda ($49),y
                sta $fb
                iny
                lda ($49),y
                sta $fc
                ldy #$00
printloop       lda ($fb),y
                jsr prtchr
                iny
                cpy $fd
                bne printloop
.bend         
                
intvaraddr      ldx #$00
                stx $0d
                ldx #$80
                stx $0e
                bne getvar
floatvaraddr    ldx #$00
                stx $0d
                stx $0e
                beq getvar
strvaraddr      ldx #$ff
                stx $0d
                ldx #$00
                stx $0e
getvar          sta varnam
                sty varnam+1
                jsr $b0e7
                sta $49
                sty $4a
                rts
    
; *** data ***                
                
; 23.5
fpnum           .byte $85, $3c, $00, $00, $00

; 15
intnum          .word $000f
    
str             .text "the sum of x%+x ="  
strend

strxint         .null "x% = "
strxfloat       .null "x ="

