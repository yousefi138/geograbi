#' Open connection with GEOmetadb
#'
#' A wrapper for connecting to the GEOmetadb sql database to allow 
#' dbListFields into sql queries
#'
#' @param db.path path where GEOmetadb.sqlite is stored
#'
#' @export
geograbr.connect.db <- function(db.path="."){
    require(GEOmetadb)
    filename <- file.path(db.path, 'GEOmetadb.sqlite')
    if(!file.exists(filename)) getSQLiteFile()
    con <- dbConnect(SQLite(), filename)
    con 
}

#' Update the GEOmetadb sql directory
#'
#' Moves the current GEOmetadb.sqlite database to an archived directory of 
#' your choice and then downloads a new version of this file
#'  
#'
#' @param archive.path path where GEOmetadb.sqlite will be archived
#'
#' @export
geograbr.replace.GEOmetadb.sqlite <-function(archive.path="."){
    archive.dir <- file.path(archive.path, paste("GEOmetadb.sqlite.archive", gsub(" ", "_", date(), fixed = TRUE), sep = "_"))
    dir.create(archive.dir)

    stopifnot(file.copy("GEOmetadb.sqlite", file.path(archive.dir,"GEOmetadb.sqlite"),  copy.date = T))
    stopifnot(file.remove("GEOmetadb.sqlite"))

    cat("Previous sql archive now stored in:\n", archive.dir, "\n")

    getSQLiteFile()
}


#' Query the GEOmetadb sql directory for GSEs
#'
#' Uses an sql expression to query the GEOmetadb database and returns all GSE 
#' numbers meeting the search criteria
#'
#' @param sql an sql expression to query the GEOmetadb database
#' @param db.path path where GEOmetadb.sqlite is stored
#'
#' @export
geograbr.query.db.gses <- function(sql, db.path="."){
    con <- geograbr.connect.db(db.path)
    geo.samples <- dbGetQuery(con,sql)
    gses <- gsub(".*,([^,]+)$", "\\1", geo.samples$series_id)
    gses
}