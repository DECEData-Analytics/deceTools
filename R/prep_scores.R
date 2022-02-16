#' Get 3KPK 1395 RFP ReadScores ready prepared for push into fse files
#'
#' @param df_read ReadScore export from VSET backend
#' @param df_interview Interview score exports from vset backend
#'
#'
#' @return a grouped list broken out by evaluator and proposal id
#' @export
#'
#' @examples
prep_scores = function(df_read, df_interview) {
  if (exists('strength_comments') & exists('growth_comments') & exists('users') & exists('proposal_info')) {

    x <- dplyr::left_join(df_read, df_interview, by = c("Id" = "rs_fk"), na_matches = "never") %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(is.na(.), 0, .))) %>%
      dplyr::mutate(.,
                    final_pgm_vision = sv_ProgramVision_1,
                    final_col_1a = sv_COL_1A,
                    final_col_1c = sv_COL_1C,
                    final_5 = sv_HealthSafety_5,
                    final_6a = sv_A_PlayBased_6,
                    final_6b = sv_B_PlayBased_6,
                    final_10a = sv_A_ContinousQuality_10,
                    final_10b = sv_B_ContiniousQuality_10,
                    final_lic_strength = dplyr::if_else(is.na(lic_strength.y), '', strength_comments$statement_of_strength[match(lic_strength.y, strength_comments$id_num)]),
                    final_lic_growth = dplyr::if_else(is.na(lic_growth.y), '', growth_comments$statement_of_growth[match(lic_growth.y, growth_comments$id_num)]),
                    user_name = users$Name[match(Evaluator_fk.x, users$Id)] %>% stringr::str_replace_all(., "\\s", "_"),
                    rfp_type = proposal_info$rfp_type[match(Proposal_fk.x, proposal_info$Id)]
      ) %>%
      dplyr::mutate(comment = stringr::str_c(final_lic_strength, final_lic_growth, sep = ", "),
                    user_proposal = stringr::str_c(user_name, Proposal_fk.x, sep = "_")) %>%
      dplyr::select(., Proposal_fk = Proposal_fk.x, user_name, user_proposal, rfp_type,final_pgm_vision, final_col_1a,
                    sv_COL_1B, final_col_1c, EquityAndAccess_2:PositiveClassCulture_4B,final_5:final_6b,
                    ResponsiveInstruction_7:CargiversTeachersAdvocates_9, final_10a, final_10b,
                    QualifiedLeadership_11, 	OrgCultureCommunity_12, comment)

    final_scores_table <<- x

    x <- x %>%
      dplyr::group_split(., user_proposal)

    return(x)

  }
}
