#' add.roxy
#'
#' adds roxygen documentation headers to the top on R function and
#' optionally write this to a file in the R folder.
#'
#' @param obj the function to create documentation for
#' @param file if TRUE write to file (R/{function name}.R), Default: FALSE
#' @return TRUE if successful, FALSE if not
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @seealso
#'  \code{\link[sinew]{makeOxygen}}
#'  \code{\link[formatR]{tidy_source}}
#'
#' @rdname obj
#'
#' @export
#' @importFrom sinew makeOxygen
#' @importFrom formatR tidy_source

add.roxy <- function(obj, file = FALSE) {
  roxy <- makeOxygen(obj, print = FALSE)

  func_name <- deparse(substitute(obj))

  code <- paste(func_name, "<-", paste(deparse(obj), collapse = "\n"))
  code <-
    tidy_source(
      text = code, indent = 2, output = FALSE,
      blank = TRUE, arrow = TRUE, wrap = TRUE,
      comments = TRUE, width.cutoff = 80
    )

  if (isTRUE(file)) {
    file <- file.path("R", paste0(func_name, ".R"))
    if (file.exists(file)) {
      warning(file, " exists, please delete if you want to overwrite.", call. = FALSE)
      return(invisible(FALSE))
    }
  } else {
    file <- ""
  }

  cat(roxy, "\n\n", code$text.tidy, "\n", file = file, append = FALSE, sep = "")
  message("removing ", func_name, " from .GlobalEnv to avoid conflicts.")
  rm(list = func_name, envir = globalenv())
  invisible(TRUE)
}
