CC 			= $(shell ../get-compiler.sh ../bin)
DiskFile	= ../disk/utils.d64

PRGS = $(wildcard *.s)

.PHONY: all disk
all: $(PRGS:.s=.prg)

disk:
	../make-disk.sh . $(DiskFile)

.PHONY: clean
clean:
	rm *.prg

%.prg : %.s
	$(CC) $<
