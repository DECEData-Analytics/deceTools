#' Export a list of DataFrames to excel files.
#'
#' @param list_of_scores input list of dataframes
#' @param template excel template to write scores to
#' @param path file path for excels to be saved.
#'
#' @return Returns empty. Maps over a list of dataframes exporting them.
#' @export
#'
#' @examples
gen_multiple_scores = function(list_of_scores, template, path) {
  if (!is.list(list_of_scores)) {
    stop(paste0("Expecting list: " , list_of_scores, "is", typeof(list_of_scores)))
  } else if (is.list(list_of_scores)) {
    purrr::map(list_of_scores, ~ deceTools::generate_passport_exports(.x, template, path))
  }
}
