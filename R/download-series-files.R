#' Download GEO series matrix files
#'
#' Takes a vector of GSEs, say as returned by \code{geo.query.db.gses}, and 
#' their corresponding gpl codes
#' and downloads the GEO series matrix files to a specified directory
#'
#' @param path path for saving downloaded GEO series matrix files
#' @param gses vector of gses to download from GEO 
#' @param gpls vector of platform gpl codes with same length as \code{gses}
#'
#' @export 
geograbbi.download.series.files <- function(path=".", gses, gpls = NULL) {
    require(RCurl)
    filenames <- sapply(seq_along(gses), function(gse) {
        urls <- get.gse.matrix.url(gses[gse])
        if(!is.null(gpls)){           
          urls <- c(urls, gsub("_series_", paste0("-",gpls[gse],"_series_"), urls[1]))
        }

        base::cat(base::date(), gses[gse], urls[1], "\n")

        for (url in urls) {
            destination <- file.path(path, basename(url))
            if (file.exists(destination)) return(destination)
        }
        
        for (url in urls) {
            destination <- file.path(path, basename(url))
            tryCatch(download.file(url, destfile=destination),
                     error=function(e) {
                         unlink(destination)
                         print(e)
                     })
            Sys.sleep(3)
            if (file.exists(destination)) 
                return(destination)
        }
        NA
    })

    filenames
}

#' Download a GEO supplementary file
#'
#' Takes a GSE and a corresponding GEO supplementary file name
#' and downloads that file to a specified directory
#'
#' @param path path for saving downloaded GEO series matrix files
#' @param gse character of gse of interest  
#' @param filename name of specific supplemental file from gse to downloads 
#' @param url optional manual url supply for non-ftp based file downloads
#'
#' @export
geograbbi.download.supplementary.file <- function(path=".", gse=NULL, filename=NULL, url=NULL) {
    require(RCurl)
    if (is.null(url)) {
        stopifnot(!is.null(gse) && !is.null(filename))
        url <- file.path(get.gse.supp.url(gse), filename) 
    }        
    destination <- file.path(path, basename(url))
    
    base::cat(base::date(), url[1], "\n")
    if (file.exists(destination)) 
        return(destination)
    tryCatch(download.file(url, destfile = destination), error = function(e) {
        unlink(destination)
        print(e)
    })
    Sys.sleep(3)
    if (file.exists(destination)) 
        return(destination)
    NA
}


#' @param x A character string.
#' @return \code{TRUE} if \code{x} is a valid GSE identifier,
#' otherwise \code{FALSE}.
is.gse <- function(x) {
    grepl("^gse[0-9]+$", x, ignore.case = T)
}

#' @param x GSE identifier
#' @return The identifier with the final 3 digits
#' replaced with 'nnn'. If there are fewer than
#' 3 digits, then all digits are replaced.
gse.prefix <- function(x) {
    x <- toupper(x)
    if (nchar(x) < 6) "GSEnnn"
    else sub("[0-9]{3}$", "nnn",x)
}


#' @param x GSE identifier
#' @return the url of the series matrix file
get.gse.matrix.url <- function(x) {
    prefix <- gse.prefix(x)
    paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/", 
          prefix, "/", x, "/matrix/", x, "_series_matrix.txt.gz", 
          sep = "")
}

#' @param x GSE identifier
#' @return the url of the supplementary folder for the GSE
get.gse.supp.url <- function(x) {
    prefix <- gse.prefix(x)
    paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/", 
          prefix, "/", x, "/suppl/", sep = "")
}
