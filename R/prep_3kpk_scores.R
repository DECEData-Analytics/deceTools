#' Get 3KPK 1395 RFP ReadScores ready prepared for push into fse files
#'
#' @param df
#'
#' @return a gropued list broken out by evaluator and proposal id
#' @export
#'
#' @examples
prep_3kpk_scores = function(df) {
  if (exists('strength_comments') & exists('growth_comments') & exists('users') & exists('proposal_info')) {

    x <- df %>%
      dplyr::mutate(.,
                    lic_strength = dplyr::if_else(is.na(lic_strength), '', strength_comments$statement_of_strength[match(lic_strength, strength_comments$id_num)]),
                    lic_growth = dplyr::if_else(is.na(lic_growth), '', growth_comments$statement_of_growth[match(lic_growth, growth_comments$id_num)]),
                    user_name := users$Name[match(Evaluator_fk, users$Id)] %>% stringr::str_replace_all(., "\\s", "_"),
                    rfp_type := proposal_info$rfp_type[match(Proposal_fk, proposal_info$Id)]
      ) %>%
      dplyr::mutate(.,
                    comments := paste0(lic_strength,", ",lic_growth),
                    user_proposal := paste0(user_name,"_",Proposal_fk)
      ) %>%
      dplyr::select(., ProgramVision_1, EquityAndAccess_2:OrgCultureCommunity_12, comments, user_proposal, rfp_type) %>%
      dplyr::group_split(., user_proposal)

    return(x)

  } else {
    message("Missing one or more needed datasets to prep this. Need: strength_comments, growth_comments, users, proposal_info")
  }

}
