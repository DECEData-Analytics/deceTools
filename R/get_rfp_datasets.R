#' Get RFP Datasets. This will read in all the necessary datasets for score exports for the 3kpk & col rfps.
#'
#' @return
#' @export
#'
#' @examples
get_rfp_datasets = function() {
  if (is.null(googlesheets4::gs4_user())) {
    errorCondition('STOP: No drive auths found. Please auth via googledrive::drive_auth() and your account key')
  } else {

    ss_ems_tool <<- googlesheets4::gs4_get('1XhFlZN8tCrYs6iBhI-Yj2mEkDVVj-ePjZ4ZJoqXV1x8')

    read_scores <<- googlesheets4::read_sheet(ss_ems_tool, sheet = "ReadScores")

    interview_scores <<- googlesheets4::read_sheet(ss_ems_tool, sheet = "SiteVisitScores")

    strength_comments <<- googlesheets4::read_sheet(ss_ems_tool, sheet = 'r_1395_96_lic_strength') %>%
      dplyr::mutate_at('statement_of_strength', ~ stringr::str_remove(., "\\d+\\.\\s")) %>%
      dplyr::mutate_at('statement_of_strength', ~ stringr::str_replace_na(.))

    growth_comments <<- googlesheets4::read_sheet(ss_ems_tool, sheet = 'r1395_96_lic_growth') %>%
      dplyr::mutate_at('statement_of_growth', ~ stringr::str_remove(., "\\d+\\.\\s")) %>%
      dplyr::mutate_at('statement_of_growth', ~ stringr::str_replace_na(.))

    users <<- googlesheets4::read_sheet(ss_ems_tool, sheet = "Users") %>%
      dplyr::mutate(Team = as.character(Team),
                    Team = stringr::str_replace_all(Team, "NULL", NA_character_)) %>%
      tidyr::separate_rows(., Team) %>%
      dplyr::mutate_at("Team", as.double)

    proposal_info <<- googlesheets4::read_sheet(ss_ems_tool, sheet = "Proposal_Info")

    coversheet <<- read_sheet(ss_ems_tool, sheet = "CoverSheet")

    final_score_entry <<- read_sheet(ss_ems_tool, sheet = 'final_score_entry')
  }
}
