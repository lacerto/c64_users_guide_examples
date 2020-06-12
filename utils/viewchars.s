; View Chars
;
; Switch to VIC Bank #3, copy CHARGEN to RAM and show the
; characters in it.
;
; Screen ram is at $cc00-$cfff, the relocated chargen sits at $f000-$ffff below KERNAL.
; Sprite data begins at $e000.s

; *** includes ***
.include "../include/colors.s"

; *** labels ***

; BASIC & KERNAL working storage
linnum          = $14           ; integer line number at $14/$15 - temporary integer (see linget)
chrgot          = $79           ; enter chrget ($73) without increasing txtptr first
txtptr          = $7a           ; $7a/$7b txtptr within chrget
blnsw           = $cc           ; 0 -> cursor blinks; nonzero -> cursor does not blink
pnt             = $d1           ; $d1/$d2 points to the address of the beginning of the current screen line
color           = $286          ; text foreground color
hibase          = $288          ; top page for screen memory

; BASIC & KERNAL routines
linget          = $a96b         ; convert string to integer at linnum ($14/$15); call $73 first
strout          = $ab1e         ; print 0 terminated string
givayf          = $b391         ; convert fixed integer y/a to float and store in FAC1
fout            = $bddd         ; convert FAC1 to string result in a/y
plot            = $e50a         ; set cursor position if carry clear / get position if carry set
clrscr          = $e544         ; initialize the screen line link table and clear the screen
fscrad          = $e9f0         ; fetch address of line in x and store it in pnt ($d1/$d2)
clrlin          = $e9ff         ; clear screen line in x
chrout          = $ffd2         ; output a character in A
getin           = $ffe4         ; get one byte from the input device

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
sp2x            = $d004         ; sprite 2 x position
sp2y            = $d005         ; sprite 2 y position
sp3x            = $d006         ; sprite 3 x position
sp3y            = $d007         ; sprite 3 y position
yxpand          = $d017         ; sprite vertical expansion register
spmc            = $d01c         ; sprite multicolor register
xxpand          = $d01d         ; sprite horizontal expansion register
spmc0           = $d025         ; sprite multicolor register 0
spmc1           = $d026         ; sprite multicolor register 1
sp0col          = $d027         ; sprite 0 color register
sp1col          = $d028         ; sprite 1 color register
sp2col          = $d029         ; sprite 2 color register
sp3col          = $d02a         ; sprite 3 color register

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
                ; border, bgcol0, bgcol1, bgcol2, bgcol3
                #setbgcolors black, black, mediumgray, lightgray, lightblue 

                lda #white      ; text color
                sta color
                lda #$0e        ; text mode (lower/uppercase)
                jsr chrout
                jsr clrscr      ; clear the screen
                jsr prtitle     ; print prg title
                jsr logo        ; show logo (multi-color sprite)
                jsr prtusage    ; print usage info
                jsr showarrows  ; show arrow sprites in usage info
                jsr prtidxstr   ; print index text
                jsr prtoffset   ; print offset text
                jsr prtaddress  ; print address text
                jsr prtprev     ; print preview text
                jsr prtdlrs     ; print dollar signs (for the hex values of the rows)
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
                ldy #$03        ; column offset
                jsr showchar

                ; print hex values for each row
                jsr prtrowhex

                ; display the index of the currently displayed character
                jsr prtidx

                ; display the hex offset of the current character in CHARGEN
                jsr prtoffshex

                ; display the hex address of the current character in the relocated CHARGEN
                jsr prtaddrhex

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
                cmp #$47        ; G
                beq gotoidx
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
                cmp #$51        ; Q
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
                jmp end

gotoidx         jsr getindex    ; let the user enter the index
                jmp end

end             lda #$00
                rts
exit    
                ; set the negative flag signalling the caller that it schould exit
                lda #$ff
                rts
.bend

; name:         printxy
; description:  print text at the given coordinates
;               a/y contains a pointer to the data
;               the first data byte is the row, the next one the column number (zero based)
;               then a null terminated string follows
; input:        a - data pointer low byte
;               y - data pointer high byte
; output:       -
; uses:         $fb - pointer low
;               $fc - pointer high
printxy
.block
                sta $fb         ; store pointer low
                sty $fc         ; and high byte
                ldy #$00        ; index
                lda ($fb),y     ; get the row number
                tax             ; transfer it to x
                iny             ; increase index
                lda ($fb),y     ; get the column number
                tay             ; transfer it to y
                clc             ; clear carry (means that plot sets the cursor coordinates)
                jsr plot        ; call plot
                lda $fb         ; load the low byte
                clc
                adc #$02        ; add 2 (skip the row/column numbers)
                bcc nooverflow  ; did it overflow?
                inc $fc         ; yes -> increase high byte
nooverflow      ldy $fc         ; load high byte (low byte is already in a)
                jsr strout      ; print null terminated text
                rts                 
.bend

; name:         prtitle
; description:  print program title
; input:        -
; output:       -
prtitle
.block
                lda #<title
                ldy #>title
                jsr printxy
                rts
.bend

; name:         prtusage
; description:  print usage information
; input:        -
; output:       -
prtusage
.block
                lda #<usage0
                ldy #>usage0
                jsr printxy
                lda #<usage1
                ldy #>usage1
                jsr printxy
                lda #<usage2
                ldy #>usage2
                jsr printxy
                lda #<usage3
                ldy #>usage3
                jsr printxy
                rts
.bend

; name:         prtdlrs
; description:  print dollar signs in rows 2 through 9 at column 12 for the
;               hex values of the character data rows
; input:        -
; output:       -
prtdlrs
.block
                ldx #$02        ; start at line 2
loop            jsr fscrad      ; get screen line address in pnt for the line in x
                ldy #$0d        ; offset for the column
                lda scdollar    ; screen code of the dollar sign
                sta (pnt),y     ; poke it into the screen mem
                inx             ; x++
                cpx #$0a        ; line < 10
                bne loop        ; yes -> next iteration
                rts
.bend

; name:         prtidxstr
; description:  print index text
; input:        -
; output:       -
prtidxstr
.block
                lda #<indexstr
                ldy #>indexstr
                jsr printxy
                rts
.bend

; name:         prtoffset
; description:  print offset text
; input:        -
; output:       -
prtoffset
.block
                lda #<offsetstr
                ldy #>offsetstr
                jsr printxy
                rts
.bend

; name:         prtaddress
; description:  print address text
; input:        -
; output:       -
prtaddress
.block
                lda #<addressstr
                ldy #>addressstr
                jsr printxy
                rts
.bend

; name:         prtprev
; description:  print preview text
; input:        -
; output:       -
prtprev
.block
                lda #<preview
                ldy #>preview
                jsr printxy
                rts
.bend

; name:         prtoffshex
; description:  print offset hex value
; input:        -
; output:       -
; uses:         $fb - used by hexify
;               $fc - used by hexify
prtoffshex
.block
                ; convert and print high byte
                lda charoffs+1
                ldx #<bytehex   ; hex value is stored here
                ldy #>bytehex
                jsr hexify      ; convert byte to hex string (null terminated)
                clc
                ldx #$0e
                ldy #$0c
                jsr plot        ; set cursor position
                lda #<bytehex
                ldy #>bytehex
                jsr strout      ; print hex value

                ; convert and print low byte
                lda charoffs
                ldx #<bytehex   ; hex value is stored here
                ldy #>bytehex
                jsr hexify      ; convert byte to hex string (null terminated)
                clc
                ldx #$0e
                ldy #$0e
                jsr plot        ; set cursor position
                lda #<bytehex
                ldy #>bytehex
                jsr strout      ; print hex value
                rts
.bend

; name:         prtaddrhex
; description:  print address hex value
; input:        -
; output:       -
; uses:         $fb - used by hexify
;               $fc - used by hexify
prtaddrhex
.block
                ; convert and print high byte
                lda charaddr+1
                ldx #<bytehex   ; hex value is stored here
                ldy #>bytehex
                jsr hexify      ; convert byte to hex string (null terminated)
                clc
                ldx #$10
                ldy #$0c
                jsr plot        ; set cursor position
                lda #<bytehex
                ldy #>bytehex
                jsr strout      ; print hex value

                ; convert and print low byte
                lda charaddr
                ldx #<bytehex   ; hex value is stored here
                ldy #>bytehex
                jsr hexify      ; convert byte to hex string (null terminated)
                clc
                ldx #$10
                ldy #$0e
                jsr plot        ; set cursor position
                lda #<bytehex
                ldy #>bytehex
                jsr strout      ; print hex value
                rts
.bend

; name:         prtrowhex
; description:  print hex values for each row
; input:        -
; output:       -
; uses:         $fb - used by hexify
;               $fc - used by hexify
;               $fd - counter
;               $fe - y coordinate of text
prtrowhex
.block
                lda #$02        ; y-coord: start in line 3
                sta $fe
                lda #$00
                sta $fd         ; counter=0
loop            ldx $fd
                lda chardata,x  ; load a byte from the character data
                ldx #<bytehex   ; hex value is stored here
                ldy #>bytehex
                jsr hexify      ; convert byte to hex string (null terminated)
                clc
                ldx $fe         ; load y-coord
                ldy #$0e        ; x-coord
                jsr plot        ; set cursor position
                lda #<bytehex
                ldy #>bytehex
                jsr strout      ; print hex value
                inc $fe         ; next line (y-coord++)
                inc $fd         ; counter++
                lda $fd
                cmp #$08        ; counter==8?
                bne loop        ; no -> next iteration
                rts
.bend

; name:         prtidx
; description:  print the current character index
; input:        -
; output:       -
; uses:         $fb - str pointer low byte
;               $fc - str pointer high byte
;               $fd - counter
prtidx
.block
                ldy charidx     ; store character index in fac                
                lda charidx+1
                jsr givayf
                jsr fout        ; convert to string in a/y (low/high)
                sta $fb         ; store the pointer to the string
                sty $fc

                ldy #$ff        ; get the length of the string
loop            iny
                lda ($fb),y
                bne loop
                sty $fd         ; and store it at $fd

                ldx #$0c        ; set the cursor position
                ldy #$0c
                clc
                jsr plot

                ldx $fd         ; pad the string using spaces
continue        cpx #$04        ; the generated string always begins with a space
                beq printnum    ; so the index range 0..511 converted to string
                lda #" "        ; can have a max length of 4 (" 511")
                jsr chrout      ; if length < 4 then print extra spaces in front
                inx             ; (max 2 spaces as the min length is 2 (e.g. idx 0 -> " 0"))
                bne continue

printnum        lda $fb         ; load the string pointer
                ldy $fc
                jsr strout      ; print the string
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

; name:         showarrows
; description:  show arrows using sprites in the usage info area
; input:        -
; output:       -
showarrows
.block
                ; copy sprite data to sprite block
                ldx #$00
copydata1       lda rightupsprite,x
                sta $e080,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata1

                ; set data pointer to the sprite block
                ldx #$02        ; sprite #2
                lda #$82        ; sprite block #130 ($c000 + $82*$40 = $e080)
                sta sprdbp,x

                ; set color
                lda #white
                sta sp2col

                ; set sprite coordinates
                lda #98
                sta sp2y

                lda #184
                sta sp2x

                ; enable sprite #2
                lda spena
                ora #%00000100
                sta spena

                ; copy sprite data to sprite block
                ldx #$00
copydata2       lda leftdownsprite,x
                sta $e0c0,x
                inx
                cpx #63         ; copy 63 bytes
                bne copydata2

                ; set data pointer to the sprite block
                ldx #$03        ; sprite #3
                lda #$83        ; sprite block #131 ($c000 + $83*$40 = $e0c0)
                sta sprdbp,x

                ; set color
                lda #white
                sta sp3col

                ; set sprite coordinates
                lda #122
                sta sp3y

                lda #184
                sta sp3x

                ; enable sprite #3
                lda spena
                ora #%00001000
                sta spena
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
;               pixel on  - bgcol3
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
                lda #$e0        ; space with background color from bgcol3
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

                lda $fb         ; store the offset for printing
                sta charoffs
                sta charaddr
                lda $fc
                sta charoffs+1

                clc             ; the relocated CHARGEN sits at $f000
                adc #$f0        ; add $f0 to the high byte of the offset
                sta $fc         ; so the reversed $ sign is at $f520
                sta charaddr+1

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

; name:         getindex
; description:  Allow the user to type in the desired index.
; input:        -
; output:       -
getindex
.block
                clc
                ldx #24         ; set cursor position
                ldy #2
                jsr plot
                lda #<prompt    ; print prompt
                ldy #>prompt
                jsr strout
                ldx #$03        ; max number of input characters allowed
                lda #<numfilter ; address of the filter string
                ldy #>numfilter
                jsr strinput
                ldx #24         ; line 24 (zero based)
                jsr clrlin      ; clear line
                lda charcount
                beq return      ; 0 characters read? -> return without updating charidx

                lda txtptr      ; push txtptr to the stack
                pha             ; now txtptr points to the end of line in the basic prg
                lda txtptr+1    ; in front of all this asm
                pha

                lda #<inpstr    ; set txtptr to inpstr
                sta txtptr
                lda #>inpstr
                sta txtptr+1

                ; convert the string at inpstr to int (linget reads up to the closing $00 using chrget)
                jsr chrgot      ; get the first char from txtptr (the other chars are handled by linget)
                jsr linget      ; convert string to temporary int at $14/$15

                pla             ; restore txtptr
                sta txtptr+1
                pla
                sta txtptr

                ; check the converted integer as it must be in the 0-511 ($00 - $1ff) range
                lda $15         ; check the high byte
                cmp #$02
                bcs getindex    ; a>=2 -> invalid value, prompt again for index

                ; value is in range -> update charidx with the converted integer
                sta charidx+1   ; the high byte is already in a
                lda linnum
                sta charidx
return                
                rts
.bend

; name:         strinput
; description:  This input routine allows only the input of characters defined in a filter list.
;               The maximum allowed number of characters input can also be specified.
;               Note: the null terminating the input string is not included in the maximum length!
;               Thus, the resulting string needs a maximum of maxchars+1 bytes of storage.
; input:        X - the maximum allowed length of the input string
;               A/Y - pointer to the filter list, a null terminated string containing the allowed chars
;                     allows to use different filters at each call
; output:       the input string at the inpstr address    (null terminated, ready to print)
; see also/thx: https://codebase64.org/doku.php?id=base:robust_string_input
strinput
.block                
                stx maxchars        ; store the max number of characters allowed
                sta filtertext+1    ; self modifying code
                sty filtertext+2
                lda blnsw           ; load cursor blink value
                pha                 ; store it on the stack
                lda #$00            ; characters received = 0
                sta charcount
                sta blnsw           ; enable cursor blink (blnsw = 0)
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
                sta inpstr,y        ; store the character
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
                sta inpstr,y        ; zero out the current char in the string
                lda #$14            ; print delete
                jsr chrout
                jmp inputloop
done
                ldy charcount       ; Y = count
                lda #$00            ; add the terminating null
                sta inpstr,y
                pla                 ; get the cursor blink value from stack
                sta blnsw           ; restore it
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

; labels
title           .byte $02, $17  ; row, column coordinates for plot
                .null "ViewChars v1.0"
preview         .byte $12, $02
                .null "Preview:"
indexstr        .byte $0c, $02
                .null "Index:"
offsetstr       .byte $0e, $02
                .null "Offset:  $"
addressstr      .byte $10, $02
                .null "Address: $"

; usage information
usage0          .byte $07, $14
                .null "w/d - increase index"
usage1          .byte $0a, $14
                .null "a/s - decrease index"
usage2          .byte $0c, $14
                .null "g   - go to index"
usage3          .byte $0e, $14
                .null "q/ret/space - exit"                

; character properties
charidx         .byte $00, $00  ; character index (range 0-511 to index the 512 characters in CHARGEN)
charoffs        .word $0000     ; getchardata calculates the offsets and addresses and stores them here
charaddr        .word $0000     ; so that they can easily be printed
chardata        .repeat 8, $00  ; the 8 bytes of the character currently showing on screen
bytehex         .repeat 3, $00  ; 3 bytes for a null terminated hex string (byte value)
scdollar        .screen "$"     ; screen code of the dollar sign

; input routine data
prompt          .null "Character index (0-511): "
numfilter       .null "1234567890"
maxchars        .byte $00
charcount       .byte $00
lastchar        .byte $00
inpstr          .repeat 4, $00

; sprites
logosprite      
                .byte $00,$0a,$00,$00,$20,$80,$00,$80
                .byte $20,$02,$00,$08,$08,$05,$02,$08
                .byte $10,$42,$08,$10,$42,$08,$10,$42
                .byte $08,$15,$42,$08,$10,$42,$02,$00
                .byte $08,$00,$80,$20,$00,$e0,$80,$03
                .byte $fa,$00,$0f,$f0,$00,$0f,$c0,$00
                .byte $3f,$00,$00,$3f,$00,$00,$fc,$00
                .byte $00,$f0,$00,$00,$f0,$00,$00

rightupsprite
                .byte $00, $00, $00
                .byte $18, $03, $04
                .byte $3c, $06, $06
                .byte $7e, $0c, $7f
                .byte $18, $18, $7f
                .byte $18, $30, $06
                .byte $18, $60, $04
                .byte $18, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00

leftdownsprite
                .byte $00, $00, $00
                .byte $10, $03, $18
                .byte $30, $06, $18
                .byte $7f, $0c, $18
                .byte $7f, $18, $18
                .byte $30, $30, $7e
                .byte $10, $60, $3c
                .byte $00, $00, $18
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00
                .byte $00, $00, $00

previewsprite   .repeat 63, $00