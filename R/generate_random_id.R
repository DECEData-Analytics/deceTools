#' generate_random_id
#'
#' @param char_leng
#'
#' @return
#' @export
#'
#' @examples
generate_random_id = function(char_leng = 9){
  pool <- c(0:9, letters, 0:9)
  x <- paste0(sample(pool, char_leng, replace = TRUE), collapse = "")
  unique_id <- paste0(x, Sys.time(), collapse = "")

  return(unique_id)
}
