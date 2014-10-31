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

#' Generates a list with names taken from variables used in its creation
#'
#' @param ... Elements to put in list
#' @return A named list
#' @keywords util
#' @export
named.list <- function(...){
	setNames(list(...), as.character( match.call()[-1] ) )
}

# TODO: Merges all vectors in a list into a data.frame (maybe?)
merge.named.list <- function(l){

}


#' Returns a data.frame grep'd on colnames
#'
#' Returns a data.frame grep'd on colnames
#'
#' @param pattern Pattern to search for
#' @param df data.frame to grep
#' @return A data.frame
#' @keywords data.frame grep
#' @export
grep.col <- function(pattern, df){
	out <- df[, grepl(pattern, colnames(df), perl=T)]
	return(out)
}

#' Returns a data.frame grep'd on rownames
#'
#' Returns a data.frame grep'd on rownames
#'
#' @param pattern Pattern to search for
#' @param df data.frame to grep
#' @return A data.frame
#' @keywords data.frame grep
#' @export
grep.row <- function(pattern, df){
	out <- df[grepl(pattern, rownames(df), perl=T),]
	return(out)
}

#' Interleaves two vectors
#'
#' Interleaves two vectors. Vectors may be of unequal length, but should be of the same type.
#'
#' @param a vector 1
#' @param b vector 2
#' @return A vector
#' @keywords vector
#' @export
interleave <- function(a, b){
	n <- min(length(a),length(b))
	p1 <- as.vector(rbind(a[1:n],b[1:n]))
	p2 <- c(a[-(1:n)],b[-(1:n)])
	c(p1,p2)
}

#' Lists all objects and sizes
#'
#' Lists all objects and sizes
#'
#' @param u Units for display, as listed in the object.size help ("b", "Kb", "Mb", "Gb", etc)
#' @return A data.frame containing a list of objects and their sizes
#' @keywords utility ls
#' @export
lh <- function(units="b"){
	    sizes <- sapply(ls(envir=.GlobalEnv), function (object.name)
	        format(object.size(get(object.name)), units=units))
	    suppressWarnings(df <- colsplit(sizes, " ", c("Size", "Unit")))
	    rownames(df) <- names(sizes)
	    return(df[order(df$Size, decreasing=T),])
}



