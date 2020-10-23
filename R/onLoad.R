.onLoad <- function(libname, pkgname) {

  # set some default SG options
  opts <-
    c(
      cpmtools.quick_help_path = "'.'"
    )

  for (i in setdiff(names(opts), names(options()))) {
    eval(parse(text = paste0("options(", i, "=", opts[i], ")")))
  }

  invisible()
}
