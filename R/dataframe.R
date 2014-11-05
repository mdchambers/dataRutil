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

#' Removes a data.frame's rownames and adds them as the first column
#'
#' Removes a data.frame's rownames and adds them as the first column
#'
#' @param df A data.frame
#' @param name Col name for added column
#' @return A data.frame
#' @keywords data.frame
#' @export
rowtofirst <- function(df, name="Row"){
	df <- cbind(rownames(df), df)
	colnames(df)[1] <- name
	rownames(df) <- NULL
	return(df)
}

#' Converts columns to factors
#'
#' Converts columns to factors. Defaults to all chr columns.
#'
#' @param df A data.frame
#' @param cols A int or chr vector of cols to factorize
#' @return df A data.frame
#' @keywords data.frame
#' @export
factorize <- function(df, cols=NULL){
	if(is.null(cols)){
		cols <- sapply(df, is.character)
	}
	if(length(cols) == 1){
		df[,cols] <- factor(df[,cols])
	} else{
		df[,cols] <- lapply(df[,cols], factor)
	}
	return(df)
}
