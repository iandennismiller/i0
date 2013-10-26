check:
	R -e 'library("devtools"); dev_mode(); load_all(); check()'

test:
	R -e 'library("devtools"); dev_mode(); load_all(); test()'

.PHONY:
	all test check
