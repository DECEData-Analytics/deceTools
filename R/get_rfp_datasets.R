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
    ss_ems_tool <<- gs4_get('1XhFlZN8tCrYs6iBhI-Yj2mEkDVVj-ePjZ4ZJoqXV1x8')

    read_scores <<- read_sheet(ss_ems_tool, sheet = "ReadScores")

    interview_scores <<- read_sheet(ss_ems_tool, sheet = "SiteVisitScores")

    strength_comments <<- read_sheet(ss_ems_tool, sheet = 'r_1395_96_lic_strength') %>%
      mutate_at('statement_of_strength', ~ str_remove(., "\\d+\\.\\s"))

    growth_comments <<- read_sheet(ss_ems_tool, sheet = 'r1395_96_lic_growth') %>%
      mutate_at('statement_of_growth', ~ str_remove(., "\\d+\\.\\s"))

    users <<- read_sheet(ss_ems_tool, sheet = "Users")

    proposal_info <<- read_sheet(ss_ems_tool, sheet = "Proposal_Info")
  }
}
