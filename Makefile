all:
	echo "Done"

install_dev:
	R -f tests/installation.R

test:
	R -f tests/main.R
