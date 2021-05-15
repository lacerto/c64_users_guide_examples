; C64 User's Guide p. 80 - Sample sound program
;
; Play a short sound on voice 1

; *** labels ***

; zero page

freezp          = $fb           ; free zero page byte
                                ; freezp+1 ($fc) is also used

; SID registers
frelo1          = $d400         ; voice 1 frequency control low byte
frehi1          = $d401         ; voice 1 frequency control high byte
vcreg1          = $d404         ; voice 1 control register
atdcy1          = $d405         ; voice 1 attack/decay register
surel1          = $d406         ; voice 1 sustain/release register
sigvol          = $d418         ; volume and filter select register

; CIA #1 registers
timblo          = $dc06         ; CIA #1 timer B low byte
timbhi          = $dc07         ; CIA #1 timer B high byte
ciacrb          = $dc0f         ; CIA #1 control register B

; *** macros ***

; name:         delay
; description:  delays for \1 * 0.1s using CIA #1 timer B
; input:        \1 - length of the delay in 0.1s steps
;                    byte value, so max delay is about 25.5s
; output:       -
delay .macro
                ; CIA #1 timer A is used to generate the interrupt
                ; for reading the keyboard and operates continuously.
                ; Timer A reaches 0 about every 1/60 second.
                ; Timer B can be set to count down every time timer A
                ; hits zero.
                ; The first argument of this macro is the delay value in
                ; 1/10 second.
                ;
                ; Example:
                ;   #delay 10
                ;
                ; So if we want to delay approx. 1s then \1 will be 10 and
                ; timer B = 6*10 = 60. As timer A zeros ~60 times in a second
                ; this will give us a 1s delay (approximately).

                ; calculate 6*\1
                ldx #\1         ; \1 -> x
                lda #$00
                sta freezp      ; $fb=0; $fc=0
                sta freezp+1
calctimlohi     clc             ; clear carry before addition
                lda freezp      ; $fb -> a
                adc #$06        ; a = a + 6
                bcc nooverflow  ; did a overflow?
                inc freezp+1    ; yes -> $fc++ inc high byte
nooverflow      sta freezp      ; a -> $fb
                dex             ; x--
                bne calctimlohi ; x==0? no -> next iteration

                ; load the calculated value to CIA #1 timer B latch
                lda freezp
                sta timblo
                lda freezp+1
                sta timbhi

                ; bit 0: start timer B
                ; bit 3: in one-shot mode
                ; bit 4: load the value from the latch
                ; bit 5-6: count timer A zeros
                lda #%01011001
                sta ciacrb

                ; busy wait until timer B reaches 0
waithi          lda timbhi
                bne waithi      ; is timer B high byte 0? -> no, wait a bit more
waitlo          lda timblo      ; high byte is already 0, low byte, too?
                bne waitlo      ; no -> wait, wait, wait
.endm

; *** main ***

                *=$0334         ; SYS820
                lda #$00        ; clear all SID registers
                ldx #$00
clearsid        sta frelo1,x
                inx
                cpx #$19
                bne clearsid

                lda #$0f        ; set maximum volume
                sta sigvol

                ; set attack and decay times (values from BASIC prg)
                ; high nibble: attack = $b = 800ms
                ; low nibble: decay = $e = 15s
                lda #$be
                sta atdcy1

                ; set sustain and release
                ; high nibble: sustain volume level = $f = max volume
                ; low nibble: release cycle duration = $8 = 300ms
                lda #$f8
                sta surel1

                lda #$11        ; frequency high byte
                sta frehi1
                lda #$25        ; frequency low byte
                sta frelo1

                ; turn on voice 1
                lda #$11        ; bit 4 = 1 select triangle waveform
                sta vcreg1      ; bit 0 = 1 start attack/decay/sustain

                #delay 5        ; delay for 0.5s

                ; turn off voice 1
                lda #$10        ; bit 0 = 0 start release
                sta vcreg1
                rts
