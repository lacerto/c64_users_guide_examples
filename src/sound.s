; C64 User's Guide p. 80 - Sample sound program
;
; Play a short sound on voice 1

; *** labels ***

; SID registers
frelo1          = $d400         ; voice 1 frequency control low byte
frehi1          = $d401         ; voice 1 frequency control high byte
vcreg1          = $d404         ; voice 1 control register
atdcy1          = $d405         ; voice 1 attack/decay register
surel1          = $d406         ; voice 1 sustain/release register
sigvol          = $d418         ; volume and filter select register

; *** macros ***

; name:         busywait
; description:  a small busywait routine using nested loops
;               the length of the outer loop is configurable
;               preserves all register values
; input:        \1 - length of the outer loop
; output:       -
busywait .macro
                ; Save regs on the stack
                pha
                tya
                pha
                txa
                pha

                ; Wait
                ldy #\1
                ldx #$00
a00
                dex
                bne a00
                dey
                bne a00

                ; Restore regs
                pla
                tax
                pla
                tay
                pla
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

                ; set attack and decay times
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

                #busywait $ff   ; wait a bit before turning off sound

                ; turn off voice 1
                lda #$10        ; bit 0 = 0 start release
                sta vcreg1
                rts
