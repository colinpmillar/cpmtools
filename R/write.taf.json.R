#' write.taf.json
#'
#' FUNCTION_DESCRIPTION
#'
#' @param x PARAM_DESCRIPTION
#' @param file PARAM_DESCRIPTION, Default: NULL
#' @param dir PARAM_DESCRIPTION, Default: NULL
#' @param ... PARAM_DESCRIPTION
#'
#' @return no return value
#'
#' @details DETAILS
#'
#' @seealso
#'  \code{\link[jsonlite]{read_json}}
#'
#' @rdname write.taf.json
#'
#' @export
#'
#' @importFrom jsonlite write_json
write.taf.json <- function(x, file = NULL, dir = NULL, ...) {
  if (is.null(file)) {
    file <- deparse(substitute(x))
    file <- sub(".*[@$]", "", file)
    file <- paste0(file, ".json")
  }
  if (!is.null(dir) && file != "") {
    file <- file.path(sub("[/\\]+$", "", dir), file)
  }
  write_json(
    x,
    path = file,
    pretty = TRUE, auto_unbox = TRUE, digits = 9, ...
  )
}
