# This Makefile compiles my .el files into .elc files.

EMACS = emacs

# Do _NOT_ put ../.gnus in ELFILES or it will be deleted, because it will end up
# in ELCFILES unchanged!
ELFILES = $(wildcard my-*.el)
ELCFILES = $(ELFILES:.el=.elc) ../.gnus.elc

.SILENT:

all: clean
	echo "Byte-compiling .el files ..."
	echo
	$(EMACS) -batch --eval='(setq my-byte-compiling-all-files t)' -l ../.emacs \
		-f batch-byte-compile $(ELFILES) ../.gnus
ifeq ($(OSTYPE),cygwin)
	echo
	echo "Fixing permissions on .elc files ..."
	fixperm $(ELCFILES)
endif

clean:
	echo "Deleting .elc files ..."
	echo
	rm -f $(ELCFILES)
