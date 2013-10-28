library(knitr)
library(markdown)

knit(input='vignettes/page-gould_miller.Rmd', output='inst/doc/page-gould_miller.md')
markdownToHTML(file='inst/doc/page-gould_miller.md', output='page-gould_miller.html')
