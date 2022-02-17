#' Prep charter scores for passport export
#'
#' @param df charter interview scores to export
#'
#' @return
#' @export
#'
#' @examples
prep_charter_scores = function(df) {
  if (exists('lic_strength') & exists('lic_growth') & exists('users')) {

 x <- df %>%
  dplyr::mutate(dplyr::across(dplyr::contains('lic_strength'), ~ charter_lic_strength$strength_statements[match(., charter_lic_strength$Id)]),
                dplyr::across(dplyr::contains('lic_growth'), ~ charter_lic_growth$growth_statements[match(., charter_lic_growth$Id)]),
                dplyr::across(dplyr::contains('lic_'), ~ stringr::str_replace_na(.)),
                dplyr::across(dplyr::contains('interview_'), ~ . / 2),
         comment_1 = stringr::str_c(lic_strength_pv,	lic_growth_pv, sep = ", "),
         comment_2 = stringr::str_c(lic_strength_pv,	lic_growth_pv, sep = ", "),
         comment_3 = stringr::str_c(lic_strength_3, lic_growth_3, sep = ", "),
         comment_4 = stringr::str_c(lic_strength_4, lic_growth_4, sep = ", "),
         comment_5 = stringr::str_c(lic_strength_5, lic_growth_5, sep = ", "),
         comment_6 = stringr::str_c(lic_strength_6, lic_growth_6, sep = ", "),
         comment_7 = stringr::str_c(lic_strength_7, lic_growth_7, sep = ", "),
         comment_8 = stringr::str_c(lic_strength_8, lic_growth_8, sep = ", "),
         comment_9 = stringr::str_c(lic_strength_9, lic_growth_9, sep = ", "),
         comment_10 = stringr::str_c(lic_strength_10, lic_growth_10, sep = ", "),
         evaluator = users$Name[match(Evaluator_fk, users$Id)],
         user_proposal = stringr::str_c(evaluator, Proposal_fk, sep = "_")
  ) %>%
   dplyr::mutate(dplyr::across(tidyselect::where(is.character), ~ str_replace_all(., ", NA", "")),
         user_proposal = str_replace_all(user_proposal, "\\s", "_")) %>%
   dplyr::select(., c(user_proposal, evaluator, proposal = Proposal_fk, dplyr::contains("interview_"), dplyr::contains("comment_")))

 return(x)

  }

}
