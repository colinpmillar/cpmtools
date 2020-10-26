#' @title installed_packages
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname installed_packages
#' @export


installed_packages <- function() {
  x <- installed.packages()
  x <- as.data.frame(x)
  x <- x[c("Package", "LibPath", "Version", "Built")]
  rownames(x) <- NULL
  x <- x[order(x$Package), ]
  by(x, x$LibPath, function(x) x)
}
