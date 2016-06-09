all: dependencies

caskgit = https://github.com/cask/cask.git
caskdir = ${PWD}/.cask
cask    = $(caskdir)/cask/bin/cask

$(caskdir):
	mkdir $@
$(cask): $(caskdir)
	cd $^ && git clone $(caskgit)

.PHONY: dependencies
dependencies: $(cask)
	$(cask) install
