CC 			= tmpx
DiskFile	= ../disk/utils.d64

PRGS = $(wildcard *.s)

.PHONY: all
all: $(PRGS:.s=.prg)

.PHONY: clean
clean:
	rm *.prg

%.prg : %.s
	$(CC) $<
	c1541 -attach $(DiskFile) -delete $(notdir $(basename $@))
	c1541 -attach $(DiskFile) -write $@ $(notdir $(basename $@))
