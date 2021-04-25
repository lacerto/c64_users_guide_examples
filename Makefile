CC = $(shell ./get-compiler.sh)
SourceDir = src
PrgDir = bin
SRCS = $(wildcard $(SourceDir)/*.s)
PRGS = $(addprefix $(PrgDir)/, $(notdir $(SRCS:.s=.prg)))

DiskFile = disk/examples.d64

.PHONY: all utils disk
all: $(PRGS) utils

utils:
	$(MAKE) -C utils

$(PrgDir)/%.prg: $(SourceDir)/%.s
	$(CC) -i $< -o $@

disk:
	./make-disk.sh $(PrgDir) $(DiskFile)
	$(MAKE) -C utils disk

.PHONY: clean
clean:
	rm -f $(PrgDir)/*.prg
	rm -f utils/*.prg