.onLoad <- function(libname, pkgname) {

  # set some default SG options
  opts <-
    c(
      cpmtools.quick_help_path = file.path(path.expand("~"), "quick-help")
    )

  for (i in setdiff(names(opts), names(options()))) {
    eval(parse(text = paste0("options(", i, "=", opts[i], ")")))
  }

  invisible()
}
