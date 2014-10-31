#' Executes commands from the clipboard
#'
#' Executes commands from the clipboard
#'
#' @keywords clipboard
#' @export
pbex <- function(){
	source(pipe("pbpaste"))
}

#' Reads commands from the clipboard
#'
#' Reads commands from the clipboard
#'
#' @keywords clipboard
#' @export
pbread <- function(...){
	return(read.table(pipe("pbpaste"), ...))
}


#' Copies history to clipboard
#'
#' Copies history to clipboard
#'
#' @param n Number of lines to print
#' @keywords clipboard
#' @export
pbhist <- function(n=1000){
	# Generate temp file
	thist <- tempfile()
	# Save history to this file
	savehistory(thist)
	# Do a bash command to cat file -> remove last line (pbhist call) -> report lines wanted -> send to clipboard
	system(paste0("cat ", thist, " | sed '$ d' | tail -n ", n, " | pbcopy "))
	# Remove tmpfile
	unlink(thist)
}