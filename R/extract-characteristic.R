#' Extract a characteristic field in GEO sample data
#' 
#' Pattern searches (greps) for a particular pattern in a data object with 
#' sample
#' 
#' @param x GEO characteristc field, say as returned by 
#' \code{geograbr.get.samples}
#' @param pattern for grep search
#' 
#' @export
#' 
geograbr.extract.characteristic <- function(x, pattern) {
    idx <- grep(pattern, x, ignore.case=T)
    ret <- rep("", length(x))
    ret[idx] <- sub(paste0(".*((", pattern, ")[^:]*: ([^;]+)).*"), "\\3", x[idx], ignore.case=T)
    ret
}
