
LOAD_PATH ?=
BATCH = emacs -Q --batch $(LOAD_PATH)

all: bog.elc bog-autoloads.el

.PHONY: test
test: bog.elc
	@$(BATCH) -L . -l bog-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.PHONY: clean
clean:
	$(RM) bog.elc bog-autoloads.el

%.elc: %.el
	@$(BATCH) -L . -f batch-byte-compile $<

bog-autoloads.el: bog.el
	@$(BATCH) -L . --eval \
	"(let (make-backup-files) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"
