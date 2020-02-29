# Updating dependencies:
# ELPA externals setup:
#   make depsetup
# ELPA externals update
#   make depupdate

.POSIX:
EMACS = emacs
EL = scanner.el
TEST = scanner-test.el
RUN_ENTRY = scanner.elc
TEST_ENTRY = scanner-test.elc
ELPA_EXT = dash
LDFLAGS = -L ./dep/dash

.PHONY: compile test check clean depclean depsetup depupdate run

compile: $(EL:.el=.elc) $(TEST:.el=.elc)
test: $(TEST:.el=.elc)

clean:
	rm -f *.elc

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
	$(EMACS) -Q --batch -L . $(LDFLAGS) -l $(TEST_ENTRY)	\
	-f ert-run-tests-batch-and-exit

run: $(EL:.el=.elc)
	$(EMACS) -Q -L . $(LDFLAGS) -l $(RUN_ENTRY) &
