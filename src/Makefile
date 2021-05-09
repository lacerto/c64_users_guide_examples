CC = $(shell ../get-compiler.sh ../bin)
PRGS = $(wildcard *.s)

all: $(PRGS:.s=.prg)

.PHONY: clean
clean:
	rm -f *.prg

%.prg : %.s
	$(CC) -i $< -o $@
