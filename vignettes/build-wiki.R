library(rmarkdown)
library(knitr)

rmdfiles <- Sys.glob("~/repos/geograbi/vignettes/*.rmd")

sapply(rmdfiles, function(x) render(x, output_format="all",
	output_dir = "~/repos/geograbi.wiki"))
# sapply(rmdfiles, purl)
