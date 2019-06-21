CC 			= tmpx
DiskFile	= utils.d64

.PHONY: all
all: decstr.prg decstr2.prg strinput.prg

.PHONY: clean
clean:
	rm *.prg

%.prg : %.s
	$(CC) $<
	c1541 -attach $(DiskFile) -delete $(notdir $(basename $@))
	c1541 -attach $(DiskFile) -write $@ $(notdir $(basename $@))
