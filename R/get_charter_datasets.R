#' Get necessary datasets for running the charter fse process
#'
#' This will grab the necessary files & sheet info and save them as global variables. This also
#' keeps the nomenclature needed for using the sibling functions of prep_charter_scores and
#' generater_charter_export.
#'
#' @return
#' @export
#'
#' @examples
get_charter_datasets = function() {

  if (is.null(googlesheets4::gs4_user())) {
    errorCondition("STOP: No drive auths found. Please auth via googledrive::drive_auth() and your account key")
  } else {
    ss_ems_tool <<- googlesheets4::gs4_get('1XhFlZN8tCrYs6iBhI-Yj2mEkDVVj-ePjZ4ZJoqXV1x8')

    users <<- googlesheets4::read_sheet(ss_ems_tool, "Users")
    charter_interview <<- googlesheets4::read_sheet(ss_ems_tool, 'charter_interview_scores')

    #Read in the low inference comment tables.
    charter_lic_strength <<- googlesheets4::read_sheet(ss_ems_tool, 'charter_lic_strength') %>%
      dplyr::mutate_at('strength_statements', ~ stringr::str_extract(., "(?<=\\d\\.\\s)[A-Za-z].*"))

    charter_lic_growth <<- googlesheets4::read_sheet(ss_ems_tool, 'charter_lic_growth') %>%
      dplyr::mutate_at('growth_statements', ~ stringr::str_extract(., "(?<=\\d\\.\\s)[A-Za-z].*"))

  }
}


