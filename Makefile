SourceDir = src
UtilsDir = utils
ExampleDiskFile = disk/examples.d64
UtilsDiskFile = disk/utils.d64

.PHONY: all disk clean
all:
	$(MAKE) -C src
	$(MAKE) -C utils

disk:
	./make-disk.sh $(SourceDir) $(ExampleDiskFile)
	./make-disk.sh $(UtilsDir) $(UtilsDiskFile)

clean:
	$(MAKE) -C src clean
	$(MAKE) -C utils clean
