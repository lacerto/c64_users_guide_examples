; Pixie
;
; A very rudimentary sprite editor written for creating the balloon
; sprite for the "Up, up and away!" example (C64 User's Guide p. 71)
; and for learning about displaying and moving simple sprites.

; *** labels ***

; BASIC/KERNAL routines
clrscr          = $e544         ; initializes and clears the screen, puts cursor into home position
getin           = $ffe4         ; get one byte from the input device
plot            = $fff0         ; set cursor position if carry clear / get position if carry set

; BASIC/KERNAL variables and pointers
pnt             = $d1           ; $d1/$d2 points to the address of the beginning of the current screen line
pntr            = $d3           ; cursor column in the current screen line
hibase          = $0288         ; top page of screen memory
rptflag         = $028a         ; flag for which keys do repeat ($80 - all; default value is $00 which allows only
                                ;   the cursor movement keys, insert/delete key, and the space bar to repeat)
; VIC registers
extcol          = $d020         ; border color VIC register
bgcol0          = $d021         ; background color VIC register

; sprite registers
spena           = $d015         ; sprite enable register
sp0col          = $d027         ; sprite 0 color register
sp0x            = $d000         ; sprite 0 x position
sp0y            = $d001         ; sprite 0 y position
sp1col          = $d028         ; sprite 1 color register
sp1x            = $d002         ; sprite 1 x position
sp1y            = $d003         ; sprite 1 y position
msigx           = $d010         ; most significant bits of sprites 0-7 x position

; zero page pointers
offset          = $fb           ; offset of the current character in the frame
addr            = $fd           ; screen address to put a character to

; color constants
bgcolor         = $00           ; background color
frmcolor        = $01           ; frame color
curscolor       = $07           ; cursor color
prevcolor       = $01           ; preview sprite color

; other constants
colramh         = $d8           ; high byte of color ram address
 
; *** macros ***

; name:         setbgcol
; description:  sets the border and background colors
; input:        \1 - border color
;               \2 - background color
; output:       -
setbgcol .macro
                lda #\1
                sta extcol
                lda #\2
                sta bgcol0
                jsr clrscr
.endm

; name:         setspriteblock
; description:  sets the block for the given sprite
; input:        \1 - sprite number (0-7)
;               \2 - block number
; output:       -
setspriteblock .macro
                ; calculate sprite data pointer location
                lda sproffset   ; offset low byte
                sta addr
                lda sproffset+1 ; offset high byte
                sta addr+1
                lda hibase      ; add the screen memory address
                clc
                adc addr+1      ; data pointer is in addr
                sta addr+1

                ; set data pointer to the sprite block
                ldy #\1         ; sprite number
                lda #\2         ; sprite block
                sta (addr),y
.endm

; name:         togglesprite
; description:  enables or disables a sprite
; input:        \1 - sprite number (0-7)
; output:       -
togglesprite .macro
                lda #$00
                ldx #\1
                inx
                sec
shiftleft       rol
                dex 
                bne shiftleft
                eor spena
                sta spena
.endm

; *** main ***

                *=$0801
                ; BASIC program to start the ML program that comes directly after it
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
                jsr initpreview
                jsr togglecursor
                jsr togglepreview
                jsr togglerepeat
                jsr getkey
                jsr togglerepeat
                #setbgcol $0e, $06
                jsr togglecursor
                jsr togglepreview
                rts

; *** subroutines ***

; name:         togglerepeat
; description:  switches between all keys repeat and only default keys repeat behavior
; input:        -
; output:       -
togglerepeat
.block
                lda rptflag
                bmi delflag     ; branch if negative flag is high (bit 7 of rptflag high)
                lda #$80        ; $80 -> all keys repeat
                bne store
delflag         lda #$00        ; restore default value 
store           sta rptflag                
                rts
.bend

; name:         getkey
; description:  wait for key presses and process user input
; input:        -
; output:       -
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
                cmp #$0d        ; return
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
                jsr updatepreview
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
                ; set sprite data pointer location
                #setspriteblock $00, $0b    ; sprite #0 uses block 11 ($02c0-$02ff otherwise unused area)

                ; copy sprite data to block
                ldx #$00
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
                #togglesprite 0
                rts

; name:         cursright
; description:  move the cursor sprite to the right
; input:        -
; output:       -
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

; name:         cursleft
; description:  move the cursor sprite to the left
; input:        -
; output:       -
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

; name:         cursup
; description:  move the cursor sprite up
; input:        -
; output:       -
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

; name:         cursdown
; description:  move the cursor sprite down
; input:        -
; output:       -
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

; name:         togglepixel
; description:  toggle a "pixel" in the sprite drawing area (toggles between a space and inverted space character)
; input:        -
; output:       a     - $a0 if pixel set, $20 if deleted
;               addr  - the screen address of the current character
;               cursy - cursor row
;               cursx - cursor column
togglepixel
.block
                clc
                ldx cursy
                ldy cursx
                jsr plot
                lda pnt+1
                sta addr+1
                lda pnt
                clc
                adc pntr
                bcc nooverflow
                inc addr+1
nooverflow
                sta addr
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

; name:         initpreview
; description:  initialize the preview sprite
; input:        -
; output:       -
initpreview
.block
                ; sprite #1 in block 13 ($0340-$037f in tbuffer)
                #setspriteblock $01, $0d
                ; zero out the block
                ldx #$00
                lda #$00
copydata        sta $0340,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set sprite color
                lda #prevcolor
                sta sp1col

                ; set initial sprite position (272; 226)
                lda #%00000010  ; x position 9th bit high
                ora msigx
                sta msigx
		        
                ldx #$10        ; x = %100010000 = 272; MSB in msigx
                ldy #$e2        ; y = 226
                stx sp1x
                sty sp1y
                rts                                          
.bend

; name:         togglepreview
; description:  enable or disable the preview sprite
; input:        -
; output:       -
togglepreview
                #togglesprite 1
                rts

; name:         updatepreview
; description:  update the preview sprite
; input:        -
; output:       -
updatepreview
.block
                ; handle row
                lda #$40        ; addr = (cursy-1)*3
                sta addr
                lda #$03
                sta addr+1
                ldy cursy
                dey
                ldx #$03                
loop            tya
                clc
                adc addr
                bcc nooverflow
                inc addr+1
nooverflow
                sta addr
                dex
                bne loop

                ; handle column
                lda cursx
                cmp #$09        
                bcc continue    ; a<9 then continue
                cmp #$11
                bcc a01         ; a<17 -> a01
                sec
                sbc #$08        ; a-=8
                clc
a02             inc addr        ; addr++
                bcc a01
                inc addr+1
a01             sec             ; a-=8; addr++
                sbc #$08
                clc
                inc addr
                bcc continue
                inc addr+1

                ; toggle bit at addr
continue        sec
                sbc #$08        ; calculate abs(a-8)
                eor #$ff
                clc
                adc #$01
                tax
                inx
                lda #$00
                sec
shiftleft       rol             ; set bit number a
                dex 
                bne shiftleft
                ldy #$00
                eor (addr),y    ; toggle bit number a at addr
                sta (addr),y
                rts
.bend

; *** data ***

                ; The frame marking the boundaries of the sprite drawing area.
                ; byte 1: offset from the start address of the current screen memory - low byte
                ; byte 2: offset high byte (if bit 7 is high the copy routine finishes)
                ; byte 3: screen code to put into screen memory
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

                .byte $ff, $ff      ; end - the first byte is ignored
                                    ;       if the second byte's 7th bit is high the copy routine exits

                ; offset of sprite data registers (from beginning of screen memory; default $0400+$03f8=$07f8)
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

                ; current position of the cursor sprite in screen columns and rows
cursx           .byte $01                
cursy           .byte $01
