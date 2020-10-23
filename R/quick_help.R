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
#'  \code{\link[icesTAF]{taf.skeleton}}
#'  \code{\link[git2r]{init}},\code{\link[git2r]{add}},\code{\link[git2r]{c("commit", "commit")}}
#'
#' @rdname quick_help
#'
#' @export
#' @importFrom icesTAF taf.skeleton
#' @importFrom git2r init add commit

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
  icesTAF::taf.skeleton(new_dir)

  gitignore <- file.path(new_dir, ".gitignore")
  if (!file.exists(gitignore)) {
    cat(
      c(
        "# vs code workspace",
        "*.code-workspace",
        "",
        "# TAF folders",
        "/bootstrap/data/", "/bootstrap/software/", "/bootstrap/library/",
        "/data/", "/model/", "/output/", "/report/", "/shiny/"
      ),
      sep = "\n",
      file = gitignore
    )
  }

  # make a git repo
  if (!dir.exists(file.path(new_dir, ".git"))) {
    git2r::init(new_dir)
    git2r::add(new_dir, "*")
    git2r::commit(new_dir, "initial commit", all = TRUE)
  }

  open.dir(new_dir)
}
