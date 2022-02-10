#' Get NYC Zip codes, borough, and neighborhood info from querying data.beta.nyc
#'
#' @return A tibble with 6 variables: zip, borough, post_office, neighborhood, population, and density.
#' Data should read in as double/numeric for zip, pop, and density. Borough, post_office, and neighborhood should be character.
#' @export
#'
#' @examples zips <- get_nyc_zips()
get_nyc_zips <- function() {
  readr::read_csv('https://data.beta.nyc/dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/7caac650-d082-4aea-9f9b-3681d568e8a5/download/nyc_zip_borough_neighborhoods_pop.csv')
}
