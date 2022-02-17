#' gen_multiple_charters. Wrapper around generate_charter_export. This takes a list of dataframes and computes multiple exports at once
#'
#' @param list_of_scores Must be list of dataframes matching the expected output.
#' @param template excel template to use
#' @param path where to save files
#'
#' @return
#' @export
#'
#' @examples
gen_multiple_charters = function(list_of_scores, template, path) {
  if (is.data.frame(list_of_scores)) {
    stop(paste0("Expecting list: Object in 'list_of_scores' is ", typeof(list_of_scores)))
  } else if (is.list(list_of_scores)) {
    purrr::map(list_of_scores, ~ deceTools::generate_charter_export(.x, template, path))
  }
}
