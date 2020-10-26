#' @title update_r_win
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname update_r_win
#' @export

update_r_win <- function(check.only = TRUE) {
  x <- readLines("https://cran.r-project.org/bin/windows/base/release.html")
  Rbin <- gsub(".*URL=(R-.*\\.exe).*", "\\1", x[grep("META", x)])

  binver <- gsub("R-([0-9]\\.[0-9]\\.[0-9])-win\\.exe", "\\1", Rbin)
  rver <- paste0(R.Version()$major, ".", R.Version()$minor)

  if (binver == rver) {
    message(glue("  most recent version: {rver} is installed."))
    return(invisible(FALSE))
  }

  if (check.only) {
    message(glue("  more recent version: {rver} is available."))
    return(invisible(FALSE))
  }

  exe_url <- glue("https://cran.r-project.org/bin/windows/base/{Rbin}")

  exe_fname <- file.path(tempdir(), Rbin)
  download.file(exe_url, mode = "wb", destfile = exe_fname)

  shell.exec(exe_fname)

  message(glue("  R version: {binver} is being installed, please update the system environmane variables."))
}
