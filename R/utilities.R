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

#' Performs ggsave with filename and dim 11 x 8.5
#' param filename The filename for saved file
#' @keywords ggplot2
#' @export
ggsave_page <- function(filename){
	ggsave(filename, width=11, height=8.5)
}

#' Loads the quantitation xlsx from a qPCR run into a melted data.frame
#'
#' @param quant.file the filename of the xlsx spreedsheet to load
#' @keywords qPCR
#' @export
loadPCRQuant <- function(quant.file){
	require(xlsx)
	require(reshape2)
	quant <- read.xlsx(quant.file, 1, stringsAsFactors=F)
	quant$NA. <- NULL
	quant.melt <- melt(quant, id.vars="Cycle")
	colnames(quant.melt) <- c("Cycle", "Well", "RFU")
	quant.melt$log2RFU <- log2(quant.melt$RFU)

	# Correct for incorrect Well numeric padding (i.e. C1 becomes C01)
	# This is needed for compatability with Summary xlsx



	return(quant.melt)
}

#' Loads the melting deriv xlsx from a qPCR run into a melted data.frame
#'
#' @param melt.file the filename of the xlsx spreedsheet to load
#' @keywords qPCR
#' @export
loadPCRMeltDeriv <- function(melt.file){
	require(xlsx)
	require(reshape2)
	melt <- read.xlsx(melt.file, 1, stringsAsFactors=F)
	melt$NA. <- NULL
	melt.melt <- melt(melt, id.vars="Temperature")
	colnames(melt.melt) <- c("Temp", "Well", "dA")
	melt.melt$Well <- as.character(melt.melt$Well)
	# Correct for incorrect Well numeric padding (i.e. C1 becomes C01)
	# This is needed for compatability with Summary xlsx
	.pad <- function(x){
		if(nchar(x) < 3){
			x <- paste0(substr(x,1,1), "0", substr(x,2,3))
		}
		return(x)
	}

	melt.melt$Well <- mapply(.pad, melt.melt$Well)

	return(melt.melt)
}

#' Saves several pdfs summarizing qPCR data
#'
#' @param prefix Prefix for files
#' @param quant.file the filename of the xlsx spreedsheet to load
#' @param melt.file the filename of the xlsx spreedsheet to load
#' @keywords qPCR
#' @export
summarizeqPCR <- function(prefix, quant.file, melt.file){
	q <- loadPCRQuant(quant.file)
	m <- loadPCRMeltDeriv(melt.file)
	library(ggplot2)

	q.plot <- ggplot(data=q, aes(x=Cycle, y=RFU, color=Well)) + geom_line() + theme_classic()
	ggsave(paste0(prefix, "_", "quant.pdf"), q.plot, width=11, height=8.5)

	log2.plot <- ggplot(data=q, aes(x=Cycle, y=log2(RFU), color=Well)) + geom_line() + theme_classic()
	ggsave(paste0(prefix, "_", "log2.pdf"), log2.plot, width=11, height=8.5)

	melt.plot <- ggplot(data=m, aes(x=Temp, y=dA, color=Well)) + geom_line() + theme_classic() + ylab(expression(-dA/dT))
	ggsave(paste0(prefix, "_", "melt.pdf"), melt.plot, width=11, height= 8.5)
}