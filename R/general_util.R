#' Merge all tsv files in a path into one data frame
#'
#' This function opens all files in the given directory and merges
#' them into one dataframe. Files must meet the following requirement:\
#'      - Merge value in first column
#'      - Must have headers
#'      - Header value for data to be merged must be unique
#'
#' @param mypath the path containing files to load (all files will be loaded)
#' @keywords merge
#' @export
multMerge <- function(mypath){
    filenames=list.files(path=mypath, full.names=TRUE)
    datalist = lapply(filenames, function(x){read.table(file=x,header=T,stringsAsFactors=FALSE)})
    Reduce(function(x,y) {merge(x,y)}, datalist)
}

#' Merge all csv files in a path into one data frame
#'
#' This function opens all files in the given directory and merges
#' them into one data. Files must meet the following requirement:\
#'      - Merge value in first column
#'      - Must have headers
#'      - Header value for data to be merged must be unique
#'
#' @param mypath the path containing files to load (all files will be loaded)
#' @keywords merge
#' @export
multMergeCSV <- function(mypath){
	filenames=list.files(path=mypath, full.names=TRUE)
    datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
    Reduce(function(x,y) {merge(x,y)}, datalist)
}

#' Load script for intstalling Bioconductor packages
#'
#' Sources the Bioconductor script from their website.
#' Afterwards, use function biocLite("pkg name") to install/update pkgs.
#'
#' @export
loadBC <- function(){
    cat("\nSetting up bioconductor...\n")
    source("http://bioconductor.org/biocLite.R")
}

#' Removes all objects from the global environment.
#' @keywords rm
#' @export
rmAll <- function(){
    rm(list=ls(.GlobalEnv), envir=.GlobalEnv)
}

#' Quits without saving
#' @keywords quit
#' @export
qn <- function(){
	quit(save="no")
}

#' cat with newlines as sep
#'
#' Cats with newlines as seperator
#'
#' @param v A vector
#' @keywords util
#' @export
catn <- function(v){
	cat(v,sep="\n")
}

#' Writes to a file, elements seperated by newlines
#'
#' Cats with newlines to a file
#'
#' @param vec A vector
#' @param file Filename to write to
#' @keywords util
#' @export
catnf <- function(vec, file){
	cat(vec, file=file, sep="\n")
}

#' Prints a list of vectors to individual files
#'
#' One file is generated per vector in the list.
#' Files are named {suffix}{list_element_name}{prefix}
#'
#' @param l A list of vectors
#' @param prefix A prefix to use
#' @param suffix A suffix to use
#' @return None
#' @keywords util cat
#' @export
cat.list <- function(l, prefix="out_", suffix=".txt"){
	for(i in 1:length(l)){
		cat(l[[i]], file= paste0( prefix, names(l)[i], suffix ), sep="\n")
	}
}

#' Generates a list with names taken from variables used in its creation
#'
#' @param ... Elements to put in list
#' @return A named list
#' @keywords util
#' @export
named.list <- function(...){
	setNames(list(...), as.character( match.call()[-1] ) )
}

