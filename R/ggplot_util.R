#' Performs ggsave with filename and dim 11 x 8.5
#' param filename The filename for saved file
#' @keywords ggplot2
#' @export
ggsave_page <- function(filename){
	ggsave(filename, width=11, height=8.5)
}