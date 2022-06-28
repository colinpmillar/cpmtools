#' quick_help
#'
#' FUNCTION_DESCRIPTION
#'
#' @param person PARAM_DESCRIPTION
#' @param project PARAM_DESCRIPTION
#' @param path PARAM_DESCRIPTION, Default: getOption("cpmtools.quick_help_path")
#'
#' @return OUTPUT_DESCRIPTION
#'
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
#'  \code{\link[TAF]{taf.skeleton}}
#'  \code{\link[git2r]{init}},\code{\link[git2r]{add}},\code{\link[git2r]{commit}}
#'
#' @rdname quick_help
#'
#' @export
#' @importFrom TAF taf.skeleton
#' @importFrom git2r init add commit
#' @importFrom jsonlite toJSON

quick_help <- function(person, project, path = getOption("cpmtools.quick_help_path")) {
  # create an empty project directory for some quick helping :)
  date <- Sys.Date()

  dirname <- paste(date, gsub(" ", "_", person), gsub(" ", "_", project), sep = "-")
  new_dir <- file.path(path, dirname)

  message("Creating directory\n\t", new_dir)
  dir.create(new_dir, showWarnings = FALSE, recursive = TRUE)

  # make vs code workspace file
  code_workspace <- list(folders = list(list(path = ".")), settings = NULL)
  json <- toJSON(code_workspace, auto_unbox = TRUE, pretty = TRUE)

  workspace <- file.path(new_dir, paste0(dirname, ".code-workspace"))
  if (!file.exists(workspace)) {
    cat(json, file = workspace)
  }

  # create initial taf project
  taf.skeleton(new_dir)

  # make a git repo
  git_init(new_dir)

  open_dir(new_dir)
}
