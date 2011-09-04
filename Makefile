VERSION = 1.0.0

sampl: sampl_version.ml sampl.ml
	ocamlopt -o sampl -annot unix.cmxa sampl_version.ml sampl.ml

sampl_version.ml: Makefile
	echo 'let version = "$(VERSION)"' > sampl_version.ml

ifndef PREFIX
PREFIX = $(HOME)
endif

ifndef BINDIR
BINDIR = $(PREFIX)/bin
endif

.PHONY: install uninstall
install:
	@if [ -f $(BINDIR)/sampl ]; \
	  then echo "Error: run '$(MAKE) uninstall' first."; \
	  else \
	    echo "Installing sampl into $(BINDIR)"; \
	    cp sampl $(BINDIR); \
	fi

uninstall:
	rm $(BINDIR)/sampl

.PHONY: clean
clean:
	rm -f *.cm[iox] *.o *.annot *~ sampl sampl_version.ml
