#' @title add_dataset
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#'
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[icesTAF]{taf.boot.path}}
#' @rdname add_dataset
#' @export
#' @importFrom glue glue
#' @importFrom icesTAF taf.boot.path

add_dataset <- function(name) {
  message(
    "browse TAF dataset scripts at:\n",
    "    https://github.com/ices-taf/doc/tree/master/datasets"
  )

  script <-
    readLines(
      glue("https://raw.githubusercontent.com/ices-taf/doc/master/datasets/{name}.R")
    )

  cat(
    script,
    sep = "\n",
    file = taf.boot.path(glue("{name}.R"))
  )

  message(
    "to add dataset to analysis run:\n",
    "taf.roxygenise()"
  )
}
