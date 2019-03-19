#' Get GEO sample information
#' @export
geograbi.get.samples <- function(filename, FUN=geograbi.read.gse.matrix) {
    gse <- tolower(names(filename))
    cat(date(), "reading", gse,"\n")
    FUN(filename, data=FALSE)$samples
}

#' Get GEO series information
#' @export
geograbi.get.series <- function(filename, FUN=geograbi.read.gse.matrix) {
    gse <- tolower(names(filename))
    cat(date(), "reading", gse,"\n")
    FUN(filename, data=FALSE)$series
}

#' Get GEO data 
#' 
#' Grabs the expression/microarray/etc. data contained in the GEO series
#'  matrix files
#' 
#' @param filename 
#' 
#' @export
geograbi.get.data <- function(filename, path=NULL, 
    FUN=geograbi.read.gse.matrix) {
    gse <- tolower(names(filename))

    cat(date(), "reading", gse,"\n")
    x <- FUN(filename)$data

    if(!is.null(path)) {
      data.filename <- file.path(path, paste(tolower(gse), "rda", sep="."))
        if (file.exists(data.filename)){
          cat("rdata file already exists for", gse, "\n", data.filename, "\n")
          return(-1)
          }
      cat(date(), "saving", gse, "\n")
      save(x, file=data.filename)
    }
    return(x)
}

is.url <- function(x) "url" %in% class(tryCatch(url(x), error=function(e) e))



#' Read lines from a connection until a pattern is matched
#'
#' @param pattern Character pattern to search for using \code{\link{grep}()}.
#' @param con A connection object or filename.
#' @param n The starting number of lines to read.  Each time the pattern
#' is not identified, the number of lines is doubled.
#' @param ... Additional arguments to \code{\link{readLines}()}.
#' @return The initial lines read from the connection with at least
#' one matching the pattern or all lines available from the connection
#' if the pattern was not matched.
read.until <- function(pattern, con, n=100, ...) {
    if (is.character(con)) {
        con <- file(con, "r")
        on.exit(close(con))
    }
        
    lines <- c()
    finished <- F
    while (!finished) {
        new <- readLines(con=con, n=n, ...)
        lines <- c(lines, new)
        finished <- (length(grep(pattern, new)) > 0
                     || length(new) < n)
        n <- 2*n
    }
    lines
}

#' Load a GSE matrix file from GEO
#'
#' @param filename A character string that is a GSE identifier,
#' a url for GSE matrix file online, or a filename for a local
#' GSE matrix file.
#' @param data If \code{TRUE}, then load the data matrix,
#' otherwise load only the sample information.
#' @return A list with three elements:
#' \itemize{
#' \item{"samples"}{The sample data frame with one row per sample.}
#' \item{"series"}{The series vector with information on the entire gse 
#' series record}
#' \item{"data"}{The data matrix with one row per feature and one per sample.}
#' }
#' @export
geograbi.read.gse.matrix <- function (filename, data = TRUE) {
    ## if filename is a GSE number, then obtain the url
    if (is.gse(filename))
        filename <- get.gse.matrix.url(filename)

    ## if filename is a url and we want the data matrix
    ## then it's most efficient to download the file
    if (is.url(filename) & data) {
        file.url <- filename
        filename <- basename(file.url)
        download.file(file.url, destfile=filename)
        on.exit(unlink(filename))
    }

    ## open a conection the file/url
    if (is.url(filename))
        con <- gzcon(url(filename))
    else
        con <- file(filename, "r")
    
    ## read until the data matrix starts
    lines <- read.until("!series_matrix_table_begin", con)

    close(con)

    ## number of lines about the series
    nseries <- sum(grepl("^!Series_", lines))
    ## number of lines about the samples
    nsamples <- sum(grepl("^!Sample_", lines))

    ## position in the file where the data matrix starts
    matrix.start <- which(lines == "!series_matrix_table_begin") + 1

    ## parse series and sample information
    txt <- textConnection(lines)    
    series <- read.table(txt, sep = "\t", header = F, nrows = nseries, 
                         stringsAsFactors = F)    
    samples <- read.table(txt, sep = "\t", header = F, nrows = nsamples, 
                          stringsAsFactors = F)
    close(txt)

    ## transpose so we have 1 row per sample
    samples <- t(samples)
    series <- t(series)

    ## the first row gives the column names
    colnames(samples) <- sub("!Sample_", "", samples[1,])
    samples <- data.frame(samples[-1, ], stringsAsFactors = F)
    rownames(samples) <- samples$geo_accession

    ## the first row gives the column names
    series.names <- sub("!Series_", "", series[1,])
    series <- series[-1, ]
    names(series) <- series.names

    ## if just sample information requested, then return
    if (data == FALSE) 
        return(list(samples = samples, series = series))

    ## otherwise, reopen the file
    con <- file(filename, "r")

    ## read to the beginning of the matrix
    readLines(con, matrix.start-1) 

    ## read the matrix
    data <- read.table(con, sep = "\t", header = TRUE, quote = "\"", 
                       dec = ".", fill = TRUE,
                       na.strings = c("NA", "null", "NULL", "Null"),
                       comment.char = "")
    close(con)

    ## if the data matrix is empty (GEO sometimes does this!),
    ## then the matrix will contain one row of NAs, remove them.
    if (nrow(data) <= 1)
        data <- data[-(1:nrow(data)),,drop=F]
    
    ## the first column identifies the features (rows) of the matrix
    if (nrow(data) > 0) 
        rownames(data) <- data[, 1]
    data <- as.matrix(data[, -1])

    rownames(samples) <- colnames(data)
    colnames(data) <- as.character(samples$geo_accession)

    ## ... isn't this equivalent to ncol(data) == 0
    ## because of the line immediately before?
    #if (length(intersect(colnames(data), samples$geo_accession)) == 0) 
    #    return(0)

    if (any(data < -0.5 | data > 1.5, na.rm = T)) 
        data <- ilogit2(data)

    ## ... isn't this true by definition?
    #stopifnot(length(rownames(data)) == nrow(data))
    #stopifnot(length(colnames(data)) == ncol(data))

    ## ... above colnames(data) is assigned to samples$geo_accession
    ## so not sure what this is doing.
    #i <- match(colnames(data), as.character(samples$geo_accession))
    #if (any(is.na(i))) {
    #    data <- data[, which(!is.na(i)), drop = F]
    #}

    ## ... annoying if this fails, user would want to know why
    ## perhaps better to check outside this function.
    #if (!is.null(geo.sites)) 
    #    stopifnot(length(intersect(rownames(data), geo.sites)) > 1e+05)
    #if (!is.null(geo.sites)) 
    #    data <- data[match(geo.sites, rownames(data)), , drop = F]
    
    list(data = data, samples = samples, series = series)
}


#' Read a very large table
#' 
#' \code{\link{read.table}()} is extremely memory inefficient and slow.
#' This function somewhat resolves this by applying \code{\link{read.table}()}
#' multiple times to partitions of the table.
#'
#' @param file A character connection or filename.
#' @param header A logical indicating if the file contains a header
#' (default: \code{FALSE}).
#' @param chunk.size Size of partitions to read in bytes
#' (default: 21234).
#' @param verbose A logical indicating whether or not to print
#' status updates (default: \code{FALSE}).
#' @param debug A logical indicating whether or not to run
#' \code{\link{browser}()} after attempting to read the file partitions.
#' @param ... Additional arguments passed to \code{\link{read.table}()}.
#' @return A data frame as if \code{\link{read.table}()} had been used.
#' 
#' @export
geograbi.read.table <- function(file, header=F, chunk.size=21234, verbose=F, debug=F, ...) {
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    x <- list()
    is.more <- T
    while (is.more) {
        if (verbose)
            cat(date(), "reading chunk", length(x), "\n")
        tryCatch({
            x[[length(x) + 1]] <- read.table(file, header=header && length(x) == 0,
                                             nrows=chunk.size, ...)
        }, error=function(e) {
            is.more <<- F
            if (e$message != "no lines available in input") {
                signalCondition(e)
            }              
        })
    }
    if (debug)
        browser()
    if (length(x) > 1)
        for (i in 2:length(x)) colnames(x[[i]]) <- colnames(x[[1]])
    do.call(rbind, x)
}


#' Read a very large csv file
#'
#' This function is a wrapper for \code{\link{geograbi.read.table}()}
#' for reading csv files.
#'
#' @examples
#' \donttest{
#' ## download a DNA methylation dataset
#' dataset <- geograbi.read.gse.matrix("GSE116339")
#' ## dataset$data is empty because sometimes GEO
#' ## stores the genomic data in a supplementary file
#' ## instead of in the series matrix file
#'
#' ## download the supplementary file
#' filename <- geograbi.download.supplementary.file(gse="GSE116339",
#'                     filename="GSE116339_Processed_Matrix.csv.gz")
#' ## 15 minutes
#'
#' ## load the file
#' dataset$data <- geograbi.read.csv(filename)
#' ## 35 minutes
#'
#' ## make the first column the rownames (features)
#' ## and convert to a matrix
#' sites <- as.character(dataset$data[,1])
#' dataset$data <- as.matrix(dataset$data[,-1])
#' rownames(dataset$data) <- sites
#'
#' ## reorder the samples according to data matrix columns
#' colnames(dataset$data) <- sub("X", "", colnames(dataset$data))
#' idx <- match(colnames(dataset$data), dataset$samples$title)
#' dataset$samples <- dataset$samples[idx,]
#' }
#' 
#' @export
geograbi.read.csv <- function(file, header = TRUE, sep = ",", quote = "\"", dec = ".",
                              fill = TRUE, comment.char = "",
                              verbose=F, debug=F, chunk.size=21234, ...) {
    geograbi.read.table(file = file, header = header, sep = sep, quote = quote,
                        dec = dec, fill = fill, comment.char = comment.char,
                        chunk.size=chunk.size,
                        verbose=verbose, debug=debug,...)
}
