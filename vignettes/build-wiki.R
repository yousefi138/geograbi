library(rmarkdown)
library(knitr)

rmdfiles <- Sys.glob("~/repos/geograbbi/vignettes/*.rmd")

sapply(rmdfiles, function(x) render(x, output_format="all",
	output_dir = "~/repos/geograbbi.wiki"))
# sapply(rmdfiles, purl)
