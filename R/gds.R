#' Retrieve GEO dataset summaries
#'
#' @param gpl Restrict summaries to those datasets that use
#' the platforms listed.  Default is NULL, no platform restriction.
#' @param retmax Maximum number of dataset summaries to retrieve
#' in a single step.
#' @return Data frame with one row per dataset.
#' @examples 
#'   ## all datasets in GEO
#'   datasets <- geograbbi.retrieve.datasets()  ## about 4min
#' 
#'   ## all Illumina Infinium 27K DNA methylation datasets
#'   datasets <- geograbbi.retrieve.datasets(c("GPL8490")) ## 2sec
#' 
#'   ## all Illumina Infinium 27K/450K/EPIC DNA methylation datasets
#'   datasets <- geograbbi.retrieve.datasets(c("GPL8490", "GPL13534", "GPL21145")) ## 5sec
#' 
#' @export
geograbbi.retrieve.datasets <- function(gpl=NULL,retmax=10000) {
    ## https://www.ncbi.nlm.nih.gov/geo/info/geo_paccess.html

    ## retrieve summaries of datasets 'retmax' at a time
    url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
    lines <- NULL
    retstart <- 0
    done <- F
    if (is.null(gpl))
        term <- "gse[ETYP]"
    else {
        term <- paste0(gpl, "[ACCN]+AND+gse[ETYP]")
        term <- paste0(term, collapse="+OR+")
    }
    while (!done) {
        cat(date(), "retrieving dataset UID", retstart+1, "...\n")
        gds <- readLines(paste0(url, "esearch.fcgi",
                                "?db=gds",
                                "&term=", term,
                                "&retstart=", format(retstart,scientific=F),
                                "&retmax=", format(retmax,scientific=F),
                                "&usehistory=y"))        
        gds <- xmlToList(xmlParse(gds, asText=T))
        id.n <- length(gds$IdList)
        
        new <- readLines(paste0(url, "efetch.fcgi",
                                "?db=gds",
                                "&query_key=", gds$QueryKey,
                                "&WebEnv=", gds$WebEnv,
                                "&retstart=", gds$RetStart,
                                "&retmax=", gds$RetMax))
        new.n <- length(grep("^[0-9]+[.]+ ", new))
        if (new.n != id.n)
            warning("Retrieved only", new.n, "of", id.n, "summaries.")

        lines <- c(lines, new)

        cat(date(), "retrieved", new.n, "\n")
        retstart <- retstart + new.n
        done <- new.n < retmax
    }

    ## fix some weird stuff
    lines <- gsub("Platforms:", "Platform:", lines)
    lines <- gsub("[[:space:]]+Platform:", "XXXXPlatform:", lines)
    lines <- gsub("[[:space:]]+ID:", "XXXXID:", lines)
    lines <- unlist(strsplit(lines, "XXXX"))
    lines <- gsub("Series[[:space:]]+Accession:","Accession:",lines)
    lines <- gsub("Datasets:", "Dataset:", lines)

    ## identify data types in the dataset
    columns <- unique(sub("^([[:alpha:]]+:).*", "\\1", lines[grep("^[[:alpha:]]+:", lines)]))

    ## link each line to the dataset number
    idx <- grep("^[0-9]+[.]+ ",lines)
    rows <- c(tail(idx,-1),length(lines)+1) - idx
    idx <- rep(1:length(idx),rows)

    ## organize lines into a data frame with one row per dataset
    filter.and.insert <- function(x, pattern, idx) {
        ret <- rep("", max(idx))
        p.idx <- grep(pattern, x)
        idx <- idx[p.idx]
        x <- x[p.idx]
        ret[idx] <- sub(pattern, "", x)
        ret
    }
    ret <- data.frame(title=filter.and.insert(lines, "^[0-9]+[.]+ ", idx),
                      description=filter.and.insert(lines, "^\\(Submitter supplied\\) ",idx),
                      stringsAsFactors=F)
    for (column in columns)
        ret[[sub(":", "", column)]] <- filter.and.insert(lines,
                                                         paste0("^", column, "[^A-Za-z0-9]*"),
                                                         idx)
    colnames(ret) <- tolower(colnames(ret))

    ## extract the number of samples in each dataset
    ret$samples <- as.integer(sub(".* ([0-9]+) Sample[s]*$", "\\1", ret$platform))
    ret$platform <- sub(" [0-9]+ Sample[s]*$", "", ret$platform)
    
    ret
}
