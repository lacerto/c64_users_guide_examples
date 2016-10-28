; C64 User's Guide p. 35 - Variables
;
;

inlin           = $a560
strout          = $ab1e
prtqm           = $ab45 ; print "?"
prtspc          = $ab3b ; print space
prtchr          = $ab47 ; print char in a
ldfac1          = $bba2 ; load fac1 from memory
fac2s           = $bddd ; convert fac1 to string
linprt          = $bdcd ; x/a

varnam          = $45

                *=$c000         ; sys 49152
                
                lda #<fpnum
                ldy #>fpnum
                jsr ldfac1
                
                ; create variable x
                lda #"x"
                ldy #$00
                jsr floatvaraddr
                jsr $bbd0
                
                ; create variable x%
                lda #"X"        ; shifted name!!!
                ldy #$80
                jsr intvaraddr
                lda #$00        
                ldy #$0f        ; value = 15
                sta $64
                sty $65
                jsr $a9ca
                
                ; create variable x$
                lda #"x"        ; mixed: first letter normal, second one shifted
                ldy #$80
                jsr strvaraddr
                jsr $b6a3       ; frestr
                lda #strend-str
                jsr $b475
                ldy #$02
a0              lda $0061,y
                sta ($49),y
                dey
                bpl a0
                iny
a1              lda str,y
                sta ($62),y
                iny
                cpy $61
                bne a1
    
                lda #<strxint
                ldy #>strxint
                jsr strout
                
                lda #"X"
                ldy #$80
                jsr intvaraddr
                ldy #$01
                lda ($49),y
                tax
                dey
                lda ($49),y
                jsr linprt
                
                jsr prtspc
                
                lda #<strxfloat
                ldy #>strxfloat
                jsr strout
                
                ; transfer floating point variable to fac and print its value
                lda #"x"
                ldy #$00
                jsr floatvaraddr
                jsr $bba2       ; var -> fac
                jsr fac2s
                jsr strout

                lda #$0d
                jsr prtchr
                
                ; print x$
                
                lda #"x"        ; mixed: first letter normal, second one shifted
                ldy #$80
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
                jsr fac2s
                jsr strout                
                
                rts     
                
; *** subroutines ***

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
                
fpnum           .byte $85, $3c, $00, $00, $00
    
str             .text "the sum of x%+x ="  
strend

strxint         .null "x% = "
strxfloat       .null "x ="

