#' The steps below are needed to regenerate
#' the data objects and documentation files
#' included with the package and then
#' run all tests.

#' install.packages("devtools")
#' devtools::install_github("klutometis/roxygen")
library(devtools)
library(roxygen2)

document("geograbbi")

system("R CMD INSTALL geograbbi")
reload(inst("geograbbi"))

## inherited meffil code for running tests
# source("geograbbi/data-raw/globals.r",chdir=T)
# system("R CMD INSTALL geograbbi") 

