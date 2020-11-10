#' create a blank data set file
#'
#' description
#'
#' @param dataset the name of the dataset that will be created
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname taf_roxy
#' @export

taf_roxy <- function(dataset) {
  txt <-
    c(
      "#' title",
      "#'",
      "#' description",
      "#'",
      paste0("#' @name ", dataset),
      "#' @format csv file",
      "#' @tafOriginator ICES",
      "#' @tafYear 2020",
      "#' @tafAccess Public",
      "#' @tafSource script",
      "",
      "library(icesTAF)",
      ""
    )

  cat(txt, sep = "\n", file = taf.boot.path(paste0(dataset, ".R")))
}
