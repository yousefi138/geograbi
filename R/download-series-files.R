#' Download GEO series matrix files
#'
#' Takes a vector of GSEs, say as returned by \code{geo.query.db.gses}, and their corresponding gpl codes
#' and downloads the GEO series matrix files to a specified directory
#'
#' @param path path for saving downloaded GEO series matrix files
#' @param gpl the gpl platform codes from GEO for all 
#' @param gses vector of gses to download from GEO 
#'
#' @export
## curently returns the filepath of each series file saved
geograbr.download.series.files <- function(path=".", gpl = "GPL13534", gses) {
    require(RCurl)
    filenames <- sapply(gses, function(geo) {
        prefix <- substring(geo,1,5)
        urls <- paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                      prefix, "nnn/",
                      geo, "/matrix/",
                      geo, "_series_matrix.txt.gz", sep="")
        urls <- c(urls, gsub("_series_", paste0("-",gpl,"_series_"), urls[1]))
        
        cat(date(), geo, urls[1], "\n")

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