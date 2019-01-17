library(rmarkdown)
library(knitr)

rmdfiles <- Sys.glob("*.rmd")

sapply(rmdfiles, function(x) render(x, output_format="all"))
# sapply(rmdfiles, purl)
