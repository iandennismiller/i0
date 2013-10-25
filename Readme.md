# i0

# getting started with the git package

    git clone http://github.com/iandennismiller/i0
    cd i0 && R

# then inside R

    install.package("devtools")
    library("devtools")
    install_github("devtools")
    dev_mode()
    load_all("./", TRUE)
