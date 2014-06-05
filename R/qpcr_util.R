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