# Updating dependencies:
# ELPA master branch (for hydra):
#   git clone git://git.savannah.gnu.org/emacs/elpa.git
# ELPA externals setup:
#   make depsetup
# ELPA externals update
#   make depupdate

.POSIX:
EMACS = emacs
EL = scanner.el
TEST = scanner-test.el
ELPA_EXT = dash
LDFLAGS = -L ./dep/dash

.PHONY: compile test clean depclean depsetup depupdate run

compile: $(EL:.el=.elc) $(TEST:.el=.elc)
test: $(TEST:.el=.elc)

clean:
	rm -f scanner.elc scanner-test.elc

depclean:
	rm -rf ./dep/*

depsetup:
	mkdir -p ./dep
	$(foreach dep,$(ELPA_EXT),git clone			\
	git://git.savannah.gnu.org/emacs/elpa.git		\
	--single-branch --branch externals/$(dep) ./dep/$(dep);)

depupdate:
	$(foreach dep,$(ELPA_EXT),cd ./dep/$(dep) && git pull && cd ..)

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . $(LDFLAGS) -f batch-byte-compile $<


check: $(EL:.el=.elc) $(TEST:.el=.elc)
	$(EMACS) -Q --batch -L . $(LDFLAGS) -l scanner-test.elc	\
	-f ert-run-tests-batch-and-exit

run: $(EL:.el=.elc)
	$(EMACS) -Q -L . $(LDFLAGS) -l scanner.el &
