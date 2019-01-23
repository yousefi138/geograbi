#' Get GEO sample information 
geograbr.get.samples <- function(filename, FUN=geograbr.read.gse.matrix) {
    gse <- tolower(names(filename))
    cat(date(), "reading", gse,"\n")
    FUN(filename, data=FALSE)
}

#' Get GEO data 
#' 
#' Grabs the expression/microarray/etc. data contained in the GEO series
#'  matrix files
#' 
#' @param filename 
#' 
#' @export
#' 
geograbr.get.data <- function(filename, path=NULL, geo.sites=NULL,  
    FUN=geograbr.read.gse.matrix) {
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

#' Read GEO gse series matrix
#' 
#' Interface for reading a gse series matrix file as downloaded by 
#' \code{geograbr.download.series.files}
#' 
geograbr.read.gse.matrix <- function(filename, data=TRUE) {
    # parse filename 
    if(grepl("^gse*", filename, ignore.case = T)) {
        prefix <- sub("([0-9]{3}$)", "nnn", filename)        
        filename <- paste("ftp://ftp.ncbi.nlm.nih.gov/geo/series/",
                      prefix, "/",
                      filename, "/matrix/",
                      filename, "_series_matrix.txt.gz", sep="")
    }

    if (is.url(filename)) {
        filename <- gzcon(url(filename))
    } 

    i <-100; dat <-NULL;
    while (sum(sign(grepl("!series_matrix_table_begin", dat)))<1) {
        dat <- c(dat, readLines(filename, n=i))
       i = i*2
    }

    nseries <- sum(grepl("^!Series_", dat))
    nsamples <- sum(grepl("^!Sample_", dat))
    ndata <- length(dat) - match("!series_matrix_table_begin", dat) - 2
    con <- textConnection(dat)
    header <- read.table(con, sep="\t", header=F, nrows=nseries, 
        stringsAsFactors=F)
    samples <- read.table(con, sep="\t", header=F, nrows=nsamples, 
        stringsAsFactors=F)
    closeAllConnections()    
    samples <- t(samples)
    colnames(samples) <- samples[1,]
    colnames(samples) <- sub("!Sample_", "", colnames(samples))
    samples <- data.frame(samples[-1,], stringsAsFactors=F)
    rm(dat)
    gc()

    if(data==FALSE) return(samples)
    
    con <- file(filename, "r")
    readLines(con,1)
    data <- read.table(con, sep="\t", header=TRUE, quote="\"", dec=".", 
        fill=TRUE,
        na.strings = c("NA", "null", "NULL", "Null"), comment.char = "")
    close(con)
    if (ndata == 0)
        data <- data[-(1:nrow(data)),]
    
    if (nrow(data) > 0) 
        rownames(data) <- data[,1]
    data <- as.matrix(data[,-1])
    
    rownames(samples) <- colnames(data)
    colnames(data) <- samples$geo_accession

    if (length(intersect(colnames(data), samples$geo_accession)) == 0)
        return(0)

    if (is.data.frame(data))
        data <- as.matrix(data)
    
    if (any(data < -0.5 | data > 1.5, na.rm=T))
        data <- ilogit2(data)

    stopifnot(length(rownames(data)) == nrow(data))
    stopifnot(length(colnames(data)) == ncol(data))
    if(!is.null(geo.sites)) stopifnot(length(intersect(rownames(data), 
        geo.sites)) > 100000)
    
    i <- match(colnames(data), as.character(samples$geo_accession))
    if (any(is.na(i))) {
        data <- data[,which(!is.na(i)), drop=F]
    }
    
    if(!is.null(geo.sites)) data <- data[match(geo.sites, rownames(data)),,
        drop=F]

   list(data=data, samples=samples)
}



