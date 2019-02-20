extract.characteristic <- function(ch) {
    sub("([^:]+): (.*)", "\\2", ch)
}

extract.name <- function(ch) {
    sub("([^:]+): (.*)", "\\1", ch)
}

#' Extract a characteristic field in GEO sample data
#' 
#' Pattern searches (greps) for a particular pattern in a data object with 
#' sample
#' 
#' @param sample.frame a data.frame of descriptives for a GEO experiment (i.e GSE) 
#' say as returned by \code{geograbbi.get.samples}
#' @param chr.field.str string that identifies the names of the characteristic
#' columns to be extracted
#' 
#' @export
#' 
geograbbi.extract.characteristics <- function(sample.frame, 
						chr.field.str = "characteristics") {
	chrs <- grep(chr.field.str, names(sample.frame))
	ret <- data.frame(lapply(sample.frame[,chrs], extract.characteristic), 
				stringsAsFactors = F)
	names(ret) <- extract.name(sample.frame[1,chrs])
	ret
}
