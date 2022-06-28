
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
#'  \code{\link[git2r]{init}},\code{\link[git2r]{add}},\code{\link[git2r]{commit}}
#' @rdname git_init
#'
#' @importFrom git2r init add commit
#'
#' @export

git_init <- function(path = ".", commit = TRUE) {
  # make a git repo
  if (!dir.exists(file.path(path, ".git"))) {
    init(path)

    # add a git ignore file
    taf_gitignore()

    # stage and commit
    add(path, "*")
    if (commit) {
      commit(path, "initial commit", all = TRUE)
    }
  }
}
