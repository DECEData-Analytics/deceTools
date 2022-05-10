#' save_installed_packages
#'
#' @param path_to_save character vector, needs to be a csv file path to save to
#'
#' @return no return, simply exports the file as a csv
#' @export
#'
#' @examples
#' x <- tempdir()
#' save_installed_packages(path_to_save = paste0(x, "my_pacakges.csv"))

save_installed_packages = function(path_to_save = NULL) {

  if (is.null(path_to_save) || grepl(pattern = "\\.csv$", x =  path_to_save) == FALSE) {
    errorCondition("File path is either NULL (meaning you forget to enter a file path) or does not end in a .csv file format")
  }
  else {
    x <- as.data.frame(installed.packages())[1]
    pck <- gsub(" ", "", x = paste("'", x$Package, "'", collapse = ","), fixed = TRUE)

    write.csv(pck, file = path_to_save)
  }
}

