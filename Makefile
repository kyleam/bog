
EMACS ?= emacs
LOAD_PATH ?=
BATCH = $(EMACS) -Q --batch $(LOAD_PATH)

all: bog.elc bog-autoloads.el

.PHONY: test
test: bog.elc bog-tests.elc
	@$(BATCH) -L . -l bog-tests.elc \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.PHONY: clean
clean:
	$(RM) bog.elc bog-autoloads.el bog-tests.elc

%.elc: %.el
	@$(BATCH) -L . -f batch-byte-compile $<

bog-autoloads.el: bog.el
	@$(BATCH) -L . --eval \
	"(let* ((default-directory (file-name-as-directory \"$(CURDIR)\")) \
	        (target (expand-file-name \"$@\")) \
	        (excludes (list \"bog-tests.el\")) \
	        (make-backup-files nil)) \
	   (if (fboundp (quote loaddefs-generate)) \
	       (loaddefs-generate default-directory target excludes) \
	     (update-file-autoloads \"$<\" t target)))"
