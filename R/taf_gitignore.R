#' @title taf_gitignore
#' @description FUNCTION_DESCRIPTION
#' @param path PARAM_DESCRIPTION, Default: '.'
#' @param append PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname taf_gitignore
#' @export

taf_gitignore <- function(path = ".", append = FALSE) {
  gitignore <- file.path(path, ".gitignore")

  if (!file.exists(gitignore) || append) {
    lines <-
      c(
        "# vs code workspace",
        "*.code-workspace",
        "",
        "# TAF folders",
        "/bootstrap/data/", "/bootstrap/software/", "/bootstrap/library/",
        "/data/", "/model/", "/output/", "/report/", "/shiny/"
      )

    cat(
      lines,
      sep = "\n",
      file = gitignore,
      append = append
    )
  }
}
