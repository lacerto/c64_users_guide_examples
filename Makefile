SourceDir = src
PrgDir = bin
SRCS = $(wildcard $(SourceDir)/*.s)
PRGS = $(addprefix $(PrgDir)/, $(notdir $(SRCS:.s=.prg)))

DiskFile = disk/examples.d64

.PHONY: all utils
all: $(PRGS) utils

utils:
	$(MAKE) -C utils

$(PrgDir)/%.prg: $(SourceDir)/%.s
	tmpx -i $< -o $@
	c1541 -attach $(DiskFile) -delete $(notdir $(basename $@))
	c1541 -attach $(DiskFile) -write $@ $(notdir $(basename $@))

.PHONY: clean
clean:
	rm -f $(PrgDir)/*.prg
	rm -f utils/*.prg