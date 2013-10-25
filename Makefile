all:
	echo "Done"

test:
	R -e 'library("devtools"); dev_mode(); load_all(); test()'

check:
	R -e 'library("devtools"); dev_mode(); load_all(); check()'

.PHONY:
	all test check
