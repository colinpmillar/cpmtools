#' @title open.dir
#' @description FUNCTION_DESCRIPTION
#' @param dir PARAM_DESCRIPTION, Default: '.'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname open.dir
#' @export

open.dir <- function(dir = ".") {
  if (interactive()) {
    if (.Platform$OS.type == "windows") {
      shell.exec(dir)

    } else if (.Platform$OS.type == "unix" && Sys.info()["sysname"] != "Darwin") {
      system(paste0("xdg-open ", dir))

    } else if (Sys.info()["sysname"] == "Darwin") {
      system2("open", dir)

    }
  }
}
