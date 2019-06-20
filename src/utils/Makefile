CC 			= tmpx
DiskFile	= utils.d64

.PHONY: all
all: decstr.prg

decstr.prg: decstr.s

.PHONY: clean
clean:
	rm *.prg

%.prg : %.s
	$(CC) $<
	c1541 -attach $(DiskFile) -delete $(notdir $(basename $@))
	c1541 -attach $(DiskFile) -write $@ $(notdir $(basename $@))
