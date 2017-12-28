; *** labels ***

clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
getin           = $ffe4         ; get one byte from the input device
plot            = $fff0         ; set cursor position if carry clear / get position if carry set

pnt             = $d1
pntr            = $d3
hibase          = $0288         ; top page of screen memory
rptflag         = $028a
extcol          = $d020         ; border color
bgcol0          = $d021         ; background color
colramh         = $d8           ; high byte of color ram address

; sprite registers
spena           = $d015
sp0col          = $d027
sp0x            = $d000
sp0y            = $d001
msigx           = $d010

offset          = $fb           ; offset of the current character in the frame
addr            = $fd           ; screen address to put a character to

bgcolor         = $00           ; background color
frmcolor        = $01           ; frame color
curscolor       = $07           ; cursor color

; *** macros ***

; name:         setbgcol
; description:  sets the border and background colors
;               does not preserve register values
; input:        -
; output:       -
setbgcol .macro
                lda #\1
                sta extcol
                lda #\2
                sta bgcol0
                jsr clrscr
                lda #\2
                sta bgcol0
.endm

; *** main ***

                *=$0801
                ; 2017 SYS2064
                .word $080b     ; address of next basic line
                .word $07e1     ; line number
                .byte $9e       ; sys token
                .text "2061"    ; address as string
                .byte $00       ; end of basic line
                .word $0000     ; address of next basic line; $0000 -> end of basic program

                *=$080d         ; sys2061
                #setbgcol bgcolor, bgcolor
                jsr drawframe
                jsr initcursor
                jsr togglecursor
                jsr togglerepeat
                jsr getkey
                jsr togglerepeat
                #setbgcol $0e, $06
                jsr togglecursor
                rts

; *** subroutines ***

togglerepeat
.block
                lda rptflag
                bmi delflag
                lda #$80
                bne store
delflag         lda #$00
store           sta rptflag                
                rts
.bend

getkey
.block
                jsr getin
                cmp #$44        ; D
                beq right
                cmp #$41        ; A
                beq left
                cmp #$57        ; W
                beq up
                cmp #$53        ; S
                beq down
                cmp #$20        ; space
                beq draw
                cmp #$0d
                beq end
                bne getkey

right           jsr cursright
                jmp getkey                
left            jsr cursleft
                jmp getkey                
up              jsr cursup
                jmp getkey                
down            jsr cursdown
                jmp getkey                
draw            jsr togglepixel
                jmp getkey
end                
                rts
.bend

; name:         drawframe
; description:  draws the frame for the sprite pixels
;               does not preserve register values
; input:        -
; output:       -
drawframe
.block                
                lda #<frame
                sta offset
                lda #>frame
                sta offset+1

                ldy #$00
                ldx #$00
copyscr                
                lda (offset),y
                sta addr
                iny
                bne c01
                inc offset+1
c01                
                lda (offset),y
                bmi end
                iny
                bne c02
                inc offset+1
c02             
                pha
                sta addr+1
                lda hibase
                clc
                adc addr+1
                sta addr+1
                lda (offset),y
                iny
                bne c03
                inc offset+1
c03                
                sta (addr,x)
                pla
                sta addr+1
                lda #colramh
                clc
                adc addr+1
                sta addr+1
                lda #frmcolor
                sta (addr,x)
                jmp copyscr
end
                rts
.bend                

; name:         initcursor
; description:  initializes the cursor sprite
; input:        -
; output:       -
initcursor
.block
                ; calculate sprite data pointer location
                lda sproffset
                sta addr
                lda sproffset+1
                sta addr+1
                lda hibase      ; add the screen memory address
                clc
                adc addr+1      ; data pointer is in addr
                sta addr+1

                ; set data pointer to the sprite block
                lda #$0b        ; sprite block 11 ($02c0-$02ff otherwise unused area)
                ldx #$00        ; sprite #0
                sta (addr,x)

                ; copy sprite data to block
copydata        lda cursor,x
                sta $02c0,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set sprite color for sprite #0
                lda #curscolor
                sta sp0col

                ; zero out the 9th bit of the x coordinate (sprite #0 -> bit 0 of msigx)
                lda #$fe
                and msigx
                sta msigx
		        
                ; set initial sprite position
                ldx #32
                ldy #58
                stx sp0x
                sty sp0y
                lda #$01
                sta cursx
                sta cursy
                rts                
.bend

; name:         togglecursor
; description:  enables and disables the cursor sprite
; input:        -
; output:       -
togglecursor
                lda #$01
                eor spena
                sta spena
                rts

cursright
.block
                lda #$08
                clc
                adc sp0x
                cmp #$e0
                beq end
                sta sp0x
                inc cursx
end
                rts                
.bend

cursleft
.block
                lda sp0x
                sec
                sbc #$08
                cmp #$18
                beq end
                sta sp0x
                dec cursx
end
                rts                
.bend

cursup
.block
                lda sp0y
                sec
                sbc #$08
                cmp #$32
                beq end
                sta sp0y
                dec cursy
end
                rts                
.bend

cursdown
.block
                lda #$08
                clc
                adc sp0y
                cmp #$e2
                beq end
                sta sp0y
                inc cursy
end
                rts                
.bend

togglepixel
.block
                clc
                ldx cursy
                ldy cursx
                jsr plot
                lda pnt
                clc
                adc pntr
                sta addr
                lda pnt+1
                sta addr+1
                ldy #$00
                lda (addr),y
                cmp #$a0
                beq delpixel
                lda #$a0
                bne store
delpixel                
                lda #$20
store
                sta (addr),y
                rts
.bend

; *** data ***

frame           .byte $00, $00, $70 ; top line
                .byte $01, $00, $43
                .byte $02, $00, $43
                .byte $03, $00, $43
                .byte $04, $00, $43
                .byte $05, $00, $43
                .byte $06, $00, $43
                .byte $07, $00, $43
                .byte $08, $00, $71 ; mark
                .byte $09, $00, $43
                .byte $0a, $00, $43
                .byte $0b, $00, $43
                .byte $0c, $00, $43
                .byte $0d, $00, $43
                .byte $0e, $00, $43
                .byte $0f, $00, $43
                .byte $10, $00, $71 ; mark
                .byte $11, $00, $43
                .byte $12, $00, $43
                .byte $13, $00, $43
                .byte $14, $00, $43
                .byte $15, $00, $43
                .byte $16, $00, $43
                .byte $17, $00, $43
                .byte $18, $00, $43
                .byte $19, $00, $6e
                
                .byte $28, $00, $42 ; 7 lines
                .byte $41, $00, $42
                .byte $50, $00, $42
                .byte $69, $00, $42
                .byte $78, $00, $42
                .byte $91, $00, $42
                .byte $a0, $00, $42
                .byte $b9, $00, $42
                .byte $c8, $00, $42
                .byte $e1, $00, $42
                .byte $f0, $00, $42
                .byte $09, $01, $42  
                .byte $18, $01, $73 ; mark
                .byte $31, $01, $42

                .byte $40, $01, $42 ; 7 lines
                .byte $59, $01, $42
                .byte $68, $01, $42
                .byte $81, $01, $42
                .byte $90, $01, $42
                .byte $a9, $01, $42
                .byte $b8, $01, $42
                .byte $d1, $01, $42
                .byte $e0, $01, $42
                .byte $f9, $01, $42
                .byte $08, $02, $42
                .byte $21, $02, $42
                .byte $30, $02, $73 ; mark
                .byte $49, $02, $42

                .byte $58, $02, $42 ; 7 lines
                .byte $71, $02, $42
                .byte $80, $02, $42
                .byte $99, $02, $42
                .byte $a8, $02, $42
                .byte $c1, $02, $42
                .byte $d0, $02, $42
                .byte $e9, $02, $42
                .byte $f8, $02, $42
                .byte $11, $03, $42
                .byte $20, $03, $42
                .byte $39, $03, $42
                .byte $48, $03, $42
                .byte $61, $03, $42

                .byte $70, $03, $6d ; bottom line
                .byte $71, $03, $43
                .byte $72, $03, $43
                .byte $73, $03, $43
                .byte $74, $03, $43
                .byte $75, $03, $43
                .byte $76, $03, $43
                .byte $77, $03, $43
                .byte $78, $03, $43
                .byte $79, $03, $43
                .byte $7a, $03, $43
                .byte $7b, $03, $43
                .byte $7c, $03, $43
                .byte $7d, $03, $43
                .byte $7e, $03, $43
                .byte $7f, $03, $43
                .byte $80, $03, $43
                .byte $81, $03, $43
                .byte $82, $03, $43
                .byte $83, $03, $43
                .byte $84, $03, $43
                .byte $85, $03, $43
                .byte $86, $03, $43
                .byte $87, $03, $43
                .byte $88, $03, $43
                .byte $89, $03, $7d

                .byte $ff, $ff      ; end

                ; offset of sprite data registers (from beginning of screen memory)
sproffset       .word $03f8

                ; cursor sprite data
cursor          .byte $ff, $00, $00
                .byte $81, $00, $00
                .byte $81, $00, $00
                .byte $81, $00, $00
                .byte $81, $00, $00
                .byte $81, $00, $00
                .byte $81, $00, $00
                .byte $ff, $00, $00
                .repeat 39, $00

cursx           .byte $01                
cursy           .byte $01
