#' Merge all tsv files in a path into one data frame
#'
#' This function opens all files in the given directory and merges
#' them into one dataframe. Files must meet the following requirement:\
#'      - Merge value in first column
#'      - Must have headers
#'      - Header value for data to be merged must be unique
#'
#' @param mypath the path containing files to load (all files will be loaded)
#' @param header do files have a header? If not, generate based on filenames
#' @keywords merge
#' @export
multMerge <- function(mypath,header=T){
    filenames <- list.files(path=mypath, full.names=TRUE)
    if(header){
    	datalist <- lapply(filenames, function(x){read.table(file=x,header=T,stringsAsFactors=FALSE)})
    } else {
    	datalist <- lapply(filenames, function(x){
    		t <- read.table(file=x, header=F, stringsAsFactors=FALSE)
    		n <- ncol(t) - 1
    		my.names <- c("name", paste(basename(x), seq(n),sep="_"))
    		colnames(t) <- my.names
    		return(t)
    		})
    }
    Reduce(function(x,y) {merge(x,y,all=T)}, datalist)
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

#' Writes a TSV file
#'
#' Writes a TSV file using write.table. Defaults to:
#'    Tab for sep
#'    No quoting
#'    No row.names
#'
#' @param x Data.frame printable object
#' @param file Filename to write to
#' @param rownames Print rownames? Defaults to FALSE
#' @return None
#' @keywords file IO
#' @export
write.tsv <- function(x, file, rownames=F, ...){
    write.table(x, file, quote=F, sep="\t", row.names=rownames, ...)
}

#' Returns a data_table using read.table
#'
#' Returns a data_table using read.table
#'
#' @return A data_table
#' @keywords data_table write.table
#' @export
read_table <- function(...){
    read.table(...) %>% dplyr::as_data_frame
}
