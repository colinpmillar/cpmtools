
#' git_init
#'
#' FUNCTION_DESCRIPTION
#'
#' @param path PARAM_DESCRIPTION, Default: '.'
#' @param commit PARAM_DESCRIPTION, Default: TRUE
#'
#' @return no return value
#'
#' @details DETAILS
#'
#' @examples
#' if (interactive()) {
#'   # initialise a git repository and stage all files
#'   git_init(commit = FALSE)
#' }
#' @seealso
#'  \code{\link[git2r]{init}},\code{\link[git2r]{add}},\code{\link[git2r]{c("commit", "commit")}}
#' @rdname git_init
#'
#' @importFrom git2r init add commit
#'
#' @export

git_init <- function(path = ".", commit = TRUE) {
  # make a git repo
  if (!dir.exists(file.path(path, ".git"))) {
    git2r::init(path)

    # add a git ignore file
    taf_gitignore()

    # stage and commit
    git2r::add(path, "*")
    if (commit) {
      git2r::commit(path, "initial commit", all = TRUE)
    }
  }
}
