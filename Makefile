BINDIR=bin
SRCDIR=src

all: $(BINDIR)/ocabug

$(BINDIR)/ocabug:$(SRCDIR)/ocabug
	cp $< $@

$(SRCDIR)/ocabug:
	$(MAKE) -C $(SRCDIR)

clean:
	-rm -f $(BINDIR)/ocabug
	$(MAKE) -C $(SRCDIR) clean

test: $(BINDIR)/ocabug
	./$< tests/test