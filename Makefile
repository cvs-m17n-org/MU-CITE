#
# Makefile for mu-cite.
#

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l MU-MK

PREFIX =

FILES =	mu/Makefile mu/MU-CFG mu/MU-MK mu/MU-ELS \
	mu/*.el mu/README.?? \
	emu/Makefile emu/EMU-MK emu/EMU-CFG emu/EMU-ELS \
	emu/*.el emu/README.?? \
	apel/Makefile apel/APEL-MK apel/APEL-CFG apel/APEL-ELS \
	apel/*.el 


TARFILE = mu-0.1.tar


elc:
	$(EMACS) $(FLAGS) -f compile-mu

install:	elc
	$(EMACS) $(FLAGS) -f install-mu $(PREFIX)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)
