all:
	echo "Done"

test:
	R -e 'library("devtools"); dev_mode(); load_all(); test()'
