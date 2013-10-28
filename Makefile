all: check doc
	@echo done

check:
	R -e 'library("devtools"); dev_mode(); load_all(); check()'

test:
	R -e 'library("devtools"); dev_mode(); load_all(); test()'

doc:
	R -f vignettes/make_vignettes.R
	rm -rf inst/doc/figure
	mv figure inst/doc
	rm inst/doc/*.md

.PHONY:
	all test check doc
