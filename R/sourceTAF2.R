#' @title sourceTAF2
#' @description FUNCTION_DESCRIPTION
#' @param script PARAM_DESCRIPTION
#' @param quiet PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sourceTAF2
#' @export

sourceTAF2 <- function(script, quiet = FALSE) {
  system2("Rscript", glue("-e icesTAF::sourceTAF('{script}')"))
}
