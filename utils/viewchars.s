; View Chars
;
; Switch to VIC Bank #3, copy CHARGEN to RAM and show the
; characters in it.

; *** includes ***
.include "../include/colors.s"

; *** labels ***

; BASIC & KERNAL working storage
pnt             = $d1           ; $d1/$d2 points to the address of the beginning of the current screen line
color           = $286          ; text foreground color
hibase          = $288          ; top page for screen memory

; BASIC & KERNAL routines
strout          = $ab1e         ; print 0 terminated string
linprt          = $bdcd
plot            = $e50a         ; set cursor position if carry clear / get position if carry set
clrscr          = $e544         ; initialize the screen line link table and clear the screen
fscrad          = $e9f0         ; fetch address of line in x and store it in pnt ($d1/$d2)
clrlin          = $e9ff 
chrout          = $ffd2         ; output a character in A
getin           = $ffe4

; VIC registers
scroly          = $d011         ; VIC vertical fine scrolling & control register
vmcsb           = $d018         ; VIC memory control register
extcol          = $d020         ; border color (default 14 light blue)
bgcol0          = $d021         ; background color 0 (default 6 blue)
bgcol1          = $d022         ; background color 1 (default 1 white)
bgcol2          = $d023         ; background color 2 (default 2 red)
bgcol3          = $d024         ; background color 3 (default 3 cyan)
spena           = $d015         ; sprite enable register
sp0x            = $d000         ; sprite 0 x position
sp0y            = $d001         ; sprite 0 y position
sp1x            = $d002         ; sprite 1 x position
sp1y            = $d003         ; sprite 1 y position
msigx           = $d010         ; most significant bits of sprites 0-7 x position
yxpand          = $d017         ; sprite vertical expansion register
spmc            = $d01c         ; sprite multicolor register
xxpand          = $d01d         ; sprite horizontal expansion register
spmc0           = $d025         ; sprite multicolor register 0
spmc1           = $d026         ; sprite multicolor register 1
sp0col          = $d027         ; sprite 0 color register
sp1col          = $d028         ; sprite 1 color register

; CIA registers
ci1icr          = $dc0d         ; CIA1 interrupt control register
ci1cra          = $dc0e         ; CIA1 control register A
ci2pra          = $dd00         ; CIA2 data port register A

; sprite
sprdbp          = $cff8         ; sprite #0 data block pointer at the end of the relocated screen

; *** macros ***

; name:         setbgcolors
; description:  set the border and background colors
; input:        \1 - border color
;               \2 - background color 0
;               \3 - background color 1
;               \4 - background color 2
;               \5 - background color 3
; output:       -
setbgcolors .macro
                lda #\1
                sta extcol
                lda #\2
                sta bgcol0
                lda #\3
                sta bgcol1
                lda #\4
                sta bgcol2
                lda #\5
                sta bgcol3
.endm

; name:         setextbgcolormode
; description:  set the extended background color text mode
; input:        -
; output:       -
setextbgcolormode .macro
                lda scroly
                ora #%01000000
                sta scroly
.endm

; name:         resetextbgcolormode
; description:  set the extended background color text mode
; input:        -
; output:       -
resetextbgcolormode .macro
                lda scroly
                and #%10111111
                sta scroly
.endm

; *** basic ***

                *=$0801
                ; 1 REM VIEW CHARGEN
                .word $0814     ; address of next line
                .word $0001     ; line number (1)
                .byte $8f       ; rem token
                .text " view chargen"
                .byte $00       ; end of line

                ; 2020 SYS2090
                .word $081e     ; address of next line
                .word $07e4     ; line number (2020)
                .byte $9e       ; sys token
                .text "2080"    ; address of ML program ($0820)
                .byte $00       ; end of line

                ; end of BASIC program
                .word $0000

; *** main ***

                *=$0820
                ; initialize screen
                ; switch to VIC bank #3 & copy CHARGEN to RAM, screen=$cc00; chargen= $f000
                jsr switchbank

                ; set extended background color text mode
                #setextbgcolormode

                ; set border & background color registers
                #setbgcolors black, black, lightblue, lightgray, cyan 

                lda #white      ; text color
                sta color
                lda #$0e        ; text mode (lower/uppercase)
                jsr chrout
                jsr clrscr      ; clear the screen
                jsr prtitle     ; print prg title
                jsr logo        ; show logo (multi-color sprite)
                jsr prtprev     ; print preview text
                jsr prevena     ; enable character preview
loop
                ; get the character data (8 bytes) form the chargen for
                ; the character with the index in charidx (range 0-511)
                ; the data is stored at address chardata
                lda charidx
                ldx charidx+1
                jsr getchardata

                ; convert to preview sprite data
                jsr convprev

                ; display the character as 8*8 characters on the screen
                ; (spaces with different background colors)
                ldx #$02        ; row offset
                ldy #$02        ; column offset
                jsr showchar

                ; display the index of the currently displayed character
                ldx #$0a        ; clear line 11
                jsr clrlin
                clc
                ldy #$02
                jsr plot        ; set the cursor pos to 11,3
                ldx charidx
                lda charidx+1
                jsr linprt      ; print the index in decimal

                jsr getkey
                bmi end         ; negative? -> exit
                jmp loop
end                                
                lda #%00000000  ; disable all sprites 
                sta spena

                ; restore screen
                ; set border & background color registers to their default values
                #setbgcolors lightblue, blue, white, red, cyan 

                lda #lightblue  ; text color
                sta color
                lda #$8e        ; graphics / uppercase mode
                jsr chrout
                #resetextbgcolormode                
                jsr clrscr      ; clear the screen
                rts

; *** subroutines ***

; name:         getkey
; description:  get a key from the keyboard
; input:        -
;               -
; output:       negative flag set if caller should exit
getkey
.block
                jsr getin       ; get a char from the keyboard
                cmp #$44        ; D
                beq right
                cmp #$41        ; A
                beq left
                cmp #$57        ; W
                beq up
                cmp #$53        ; S
                beq down
                cmp #$1d        ; right
                beq right
                cmp #$9d        ; left
                beq left
                cmp #$91        ; up
                beq up
                cmp #$11        ; down
                beq down
                cmp #$20        ; space
                beq exit
                cmp #$0d        ; return
                beq exit               
                bne getkey

                ; handle key press events
right                       
up              clc
                inc charidx
                bne end
                inc charidx+1
                lda charidx+1
                cmp #$02        ; charidx > 511 ?
                bne end         ; no -> continue
                lda #$00        ; yes -> charidx = 0
                sta charidx
                sta charidx+1
                jmp end                
left            
down            clc
                lda charidx
                bne a00
                dec charidx+1
                bmi a01         ; nagative?
a00             dec charidx     ; no -> decrement index
                jmp end
a01             lda #$ff        ; yes -> charidx = 511 ($1ff)
                sta charidx
                lda #$01
                sta charidx+1                
end             lda #$00
                rts
exit    
                ; set the negative flag signalling the caller that it schould exit
                lda #$ff
                rts
.bend

; name:         prtitle
; description:  print program title
; input:        -
; output:       -
prtitle
.block
                clc
                ldx #$02
                ldy #$17
                jsr plot
                lda #<title
                ldy #>title
                jsr strout
                rts
.bend

; name:         prtprev
; description:  print preview text
; input:        -
; output:       -
prtprev
.block
                clc
                ldx #$12
                ldy #$02
                jsr plot
                lda #<preview
                ldy #>preview
                jsr strout
                rts
.bend

; name:         logo
; description:  show the logo sprite
; input:        -
; output:       -
logo
.block
                ; copy sprite data to sprite block
                ldx #$00
copydata        lda logosprite,x
                sta $e000,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set data pointer to the sprite block
                ldx #$00        ; sprite #0
                lda #$80        ; sprite block #128 ($e000)
                sta sprdbp,x

                ; reset expand
                lda #%00000000
                sta xxpand
                sta yxpand

                ; set sprite 0 multicolor mode
                lda #%00000001
                sta spmc

                ; set colors
                lda #$08
                sta spmc0
                lda #$06
                sta spmc1
                lda #$03
                sta sp0col

                ; set sprite coordinates
                lda #63
                sta sp0y

                lda #176
                sta sp0x

                ; enable sprite #0 
                lda #%00000001
                sta spena
                rts                
.bend

; name:         prevena
; description:  enable sprite for normal sized character preview
; input:        -
; output:       -
prevena
.block
                ; copy sprite data to sprite block
                ldx #$00
copydata        lda previewsprite,x
                sta $e040,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata

                ; set data pointer to the sprite block
                ldx #$01        ; sprite #1
                lda #$81        ; sprite block #129 ($c000 + $81*$40 = $e040)
                sta sprdbp,x

                ; set color
                lda #white
                sta sp1col

                ; set sprite coordinates
                lda #194
                sta sp1y

                lda #112
                sta sp1x

                ; enable sprite #1
                lda spena
                ora #%00000010
                sta spena
                rts                
.bend

; name:         convprev
; description:  convert character data to preview sprite data
; input:        -
; output:       -
convprev
.block
                ldx #$00
                ldy #$00
loop            lda chardata,x
                sta $e040,y     ; sprite block 129 ($81) in bank #3
                iny             ; skip next 2 bytes as each row of
                iny             ; a sprite consists of 3 bytes
                iny             ; we use only the 8x8 pixel upper left
                inx             ; corner of this sprite
                cpx #$08
                bne loop
                rts
.bend

; name:         showchar
; description:  draw the shape of a character at chardata to the screen
; input:        x - line number
;               y - column number
;               chardata - 8 bytes of the current character to show
; output:       -
; uses          $fb - counter
;               $fd - line number
;               $fe - column number
showchar
.block
                stx $fd         ; store line and column numbers
                sty $fe     
                lda #$00        ; counter starts at 0
                sta $fb
loop            ldx $fd         ; x=line number
                jsr fscrad      ; get the address of the line in x (address at pnt)
                ldx $fb         ; x=counter value
                lda chardata,x  ; get a byte of character data
                ldy $fe         ; reset the column number offset (it gets destroyed in prtline)
                jsr prtline     ; print a line of character data (different colored spaces for each bit)
                inc $fd         ; next line
                inx             ; increase counter
                stx $fb
                cpx #$08        ; x==8
                bne loop        ; no -> process the next line
                rts
.bend

; name:         prtline
; description:  print a line of character data
;               pixel on  - bgcol1
;               pixel off - bgcol2
; input:        a - one byte of character data
;               y - column number
;               pnt - start of current screen line
; output:       -
; uses          $fc - counter
prtline
.block
                pha             ; push data byte to stack
                lda #$08        ; counter starts at 8
                sta $fc
                pla             ; get back the character data byte from the stack
                clc             ; clear carry
printbit        asl             ; shift left - MSB will be in carry
                bcs biton       ; carry set? MSB was 1
                pha             ; not set -> MSB was 0
                lda #$a0        ; space with background color from bgcol2
                bne pokechar
biton           pha
                lda #$60        ; space with background color from bgcol1
pokechar        sta (pnt),y     ; store in screen mem
                iny             ; next column
                pla
                dec $fc         ; decrease counter
                bne printbit    ; counter == 0? no -> check next bit
                rts
.bend

; name:         switchbank
; description:  switch to VIC Bank #3
; input:        -
; output:       -
switchbank
.block
                sei

                ; ACK CIA1 interrupts and stop timer A, so that no
                ; IRQ can occur.
                lda #%01111111  ; $7f - acknowledge CIA1 interrupts
                sta ci1icr
                lda ci1cra
                and #%11111110  ; $fe - bit #0=0 stops CIA1 timer A (system timer)
                sta ci1cra

                ; Enable CHARGEN access.
                lda $01
                and #%11111011  ; select character generator ROM at $d000
                sta $01

                ; Copy CHARGEN to RAM.
                jsr copychrg
                
                ; Turn off CHARGEN access.
                lda $01
                ora #%00000100  ; select I/O instead of chargen ROM
                sta $01

                ; Restart CIA1 timer A.
                lda ci1cra
                ora #%00000001  ; start timer A
                sta ci1cra
                lda #%10000001  ; enable system timer interrupt
                sta ci1icr

                ; The lowest 2 bits of CIA2 Port A select the VIC bank.
                ; 00 -> VIC Bank #3
                lda ci2pra
                and #%11111100  ; bit 0-1 select VIC bank - select bank #3
                sta ci2pra

                ; Set screen ram and CHARGEN addresses.
                lda #%00111100  ; bit 7-4: 0011 -> screen ram offset = 3*1k = $c00
                sta vmcsb       ; bit 3-1: 110 -> 4*2k = $3000 chargen offset
                                ; as we are in bank #3: screen=$cc00; chargen= $f000

                lda #$cc        ; tell the OS that the screen RAM is at page 204 ($cc)
                sta hibase

                cli
                rts
.bend

; name:         copychrg
; description:  copy the character generator ROM from $d000 to $f000-$ffff
; input:        -
; output:       -
copychrg
.block
                ; set start addresses
                ; $fb/$fc -> $d000
                ; $fd/$fe -> $f000
                lda #$00
                sta $fb
                sta $fd
                lda #$d0
                sta $fc
                lda #$f0
                sta $fe
                ldx #$10        ; outer loop: run inner loop 16 times (16*256 = 4kB)
a00
                ldy #$00        ; inner loop: copy 256 bytes
a01
                lda ($fb),y
                sta ($fd),y
                dey
                bne a01
                inc $fc
                inc $fe
                dex
                bne a00
                rts
.bend

; name:         getchardata
; description:  Copy the 8 bytes of a character from the relocated CHARGEN.
; input:        A - character index low byte
;               X - character index high byte
; output:       8 bytes beginning at chardata
getchardata
.block
                sei
                stx $fc         ; store character index high/low bytes on the zero page
                sta $fb

                asl $fb         ; multiply by 8 to get the offset within CHARGEN
                rol $fc         ; example: get the data for the reversed $ sign
                asl $fb         ;   index = $a4
                rol $fc         ;   offset = $a4 * 8 = $520
                asl $fb
                rol $fc
                lda $fc

                clc             ; the relocated CHARGEN sits at $f000
                adc #$f0        ; add $f0 to the high byte of the offset
                sta $fc         ; so the reversed $ sign is at $f520

                ; switch out KERNAL in order to access the underlying RAM containing CHARGEN
                lda $01
                and #%11111101
                sta $01

                ; copy 8 bytes for the calculated address to chardata
                ldy #$00
copyloop                
                lda ($fb),y
                sta chardata,y
                iny
                cpy #$08
                bne copyloop

                ; switch on KERNAL
                lda $01
                ora #%00000010
                sta $01
                cli            
                rts
.bend

; *** data ***

title           .null "ViewChars v1.0"
preview         .null "Preview:"
charidx         .byte $00, $00  ; character index (range 0-511 to index the 512 characters in CHARGEN)
chardata        .repeat 8, $00  ; the 8 bytes of the character currently showing on screen

logosprite      
                .byte $00,$0a,$00,$00,$20,$80,$00,$80
                .byte $20,$02,$00,$08,$08,$05,$02,$08
                .byte $10,$42,$08,$10,$42,$08,$10,$42
                .byte $08,$15,$42,$08,$10,$42,$02,$00
                .byte $08,$00,$80,$20,$00,$e0,$80,$03
                .byte $fa,$00,$0f,$f0,$00,$0f,$c0,$00
                .byte $3f,$00,$00,$3f,$00,$00,$fc,$00
                .byte $00,$f0,$00,$00,$f0,$00,$00

previewsprite   .repeat 63, $00