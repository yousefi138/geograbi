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
#' 
geograbr.download.series.files <- function(path=".", gses, gpls = NULL) {
    require(RCurl)
    filenames <- sapply(seq_along(gses), function(gse) {
        prefix <- substring(gses[gse],1,5)
        urls <- paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                      prefix, "nnn/",
                      gses[gse], "/matrix/",
                      gses[gse], "_series_matrix.txt.gz", sep="")
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
#' @param sup.file.name name of specific supplemental file from gse to downloads 
#' @param url optional manual url supply for non-ftp based file downloads
#'
#' @export
geograbr.download.supplementary.file <- function(path=".", gse, sup.file.name=NULL, url=NULL) {
    require(RCurl)
    if (is.null(url)){
        prefix <- substring(gse,1,5)
        url <- paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                      prefix, "nnn/",
                      gse, "/suppl/",
                      sup.file.name, sep="")
        destination <- file.path(path, basename(url))
      } else {
        destination <- file.path(path, sup.file.name)
      }
        base::cat(base::date(), gse, url[1], "\n")

        if (file.exists(destination)) return(destination)
    
        tryCatch(download.file(url, destfile=destination),
                 error=function(e) {
                     unlink(destination)
                     print(e)
                 })
        Sys.sleep(3)
        if (file.exists(destination)) 
            return(destination)
    NA
}

