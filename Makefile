org-transclusion.org: docs/org-transclusion-manual.org
	-emacs --batch -L "$$(pwd)" -l org-transclusion $< \
	       --eval '(progn (org-transclusion-add-all) (write-region nil nil "org-transclusion.org"))'

.PHONY: test-compile
test-compile:
	emacs --batch --eval "(add-to-list 'load-path default-directory)" \
	      -f batch-byte-compile ./*.el
	# Check declare-function
	emacs --batch --eval "(check-declare-directory default-directory)"

.PHONY: clean
clean:
	eldev clean all

.PHONY: prepare
prepare:
	eldev -C --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev -C --unstable -T lint

.PHONY: test
test:
	eldev -C --unstable -T test

docs:
	make -C doc all

html:
	make -C doc html-dir

install: install-docs

install-docs: docs
	make -C doc install-docs

install-info: info
	make -C doc install-info
