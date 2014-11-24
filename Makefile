EMACS := emacs
CURL := curl --silent

DASH_URL := https://raw.githubusercontent.com/magnars/dash.el/master/dash.el

.PHONY: test

test: .downloads
	${EMACS} -Q --batch -L . \
		-l bog-tests \
		--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.downloads:
	${CURL} ${DASH_URL} > dash.el
	touch .downloads

clean:
	@rm -rf  *.elc
