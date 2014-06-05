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
    rm(list=ls())
}

#' Quits without saving
#' @keywords quit
#' @export
qn <- function(){
	quit(save="no")
}