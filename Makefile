DOC_RENDER="library(knitr); library(markdown); \
	knit(input='vignettes/foo.Rmd', output='inst/doc/foo.md'); \
	markdownToHTML(file='inst/doc/foo.md', output='foo.html')"

check:
	R -e 'library("devtools"); dev_mode(); load_all(); check()'

test:
	R -e 'library("devtools"); dev_mode(); load_all(); test()'

doc:
	R -e $(DOC_RENDER)
	mv figure inst/doc

.PHONY:
	all test check
