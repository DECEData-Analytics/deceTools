#' Prep scores for score export
#'
#' @param df_read
#' @param df_interview
#'
#' @return 3 lists of the scores to export over
#' @export
#'
#' @examples
prep_scores = function(df_read, df_interview) {
  if (exists('strength_comments') & exists('growth_comments') & exists('users') & exists('proposal_info')) {

    x <- dplyr::left_join(df_read, df_interview, by = c("Id" = "rs_fk"), na_matches = "never") %>%
      dplyr::left_join(., dplyr::select(coversheet, ProposalID, CompetitionPool), by = c("Proposal_fk.x" = "ProposalID"), na_matches = "never") %>%
      dplyr::mutate(user_comp_pool_join = str_c(CompetitionPool, Evaluator_fk.x, sep = '_')) %>%
      dplyr::filter(., Proposal_fk.x %in% ready_for_fse, user_comp_pool_join %notin% alternates) %>%
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
                    final_lic_strength = dplyr::coalesce(strength_comments$statement_of_strength[match(lic_strength.y, strength_comments$id_num)], strength_comments$statement_of_strength[match(lic_strength.x, strength_comments$id_num)]),
                    final_lic_growth = dplyr::coalesce(growth_comments$statement_of_growth[match(lic_growth.y, growth_comments$id_num)], growth_comments$statement_of_growth[match(lic_growth.x, growth_comments$id_num)]),
                    user_name = users$Name[match(Evaluator_fk.x, users$Id)] %>% stringr::str_replace_all(., "\\s", "_"),
                    rfp_type = proposal_info$rfp_type[match(Proposal_fk.x, proposal_info$Id)]
      ) %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_replace_na(., ' '))) %>%
      dplyr::mutate(comment = stringr::str_c(final_lic_strength, final_lic_growth, sep = ", "),
                    user_proposal = stringr::str_c(user_name, Proposal_fk.x, sep = "_")) %>%
      dplyr::select(., Proposal_fk = Proposal_fk.x, user_name, user_proposal, rfp_type, final_pgm_vision, final_col_1a,
                    sv_COL_1B, final_col_1c, EquityAndAccess_2:PositiveClassCulture_4B,final_5:final_6b,
                    ResponsiveInstruction_7:CargiversTeachersAdvocates_9, final_10a, final_10b,
                    QualifiedLeadership_11, 	OrgCultureCommunity_12, comment) %>%
      dplyr::mutate(final_score_prop = if_else(rfp_type == "3K/PK (1395)",
                                               round(rowSums(dplyr::select(., final_pgm_vision,EquityAndAccess_2:OrgCultureCommunity_12), na.rm = TRUE) / 75 * 5, digits = 2),
                                               round(rowSums(dplyr::select(., final_col_1a:OrgCultureCommunity_12), na.rm = TRUE) / 85 * 5, digits = 2)))


    final_scores_table <<- x %>%
      dplyr::mutate(., Evaluator_fk = users$Id[match(stringr::str_replace_all(user_name, "_", " "), users$Name)],
                                        evalProp_Id = as.numeric(stringr::str_c(Evaluator_fk, Proposal_fk))) %>%
      dplyr::mutate(read_score_id = df_read$Id[match(evalProp_Id, df_read$evalProp_fk)],
             interview_score_id = df_interview$Id[match(evalProp_Id, df_interview$evalProp_fk)],
             timestamp_export_generated = Sys.time())

    r1395_props <<- x %>%
      dplyr::filter(., rfp_type == '3K/PK (1395)') %>%
      dplyr::group_split(., user_proposal)

    r1396_props <<- x %>%
      dplyr::filter(., rfp_type == 'COL (1396)') %>%
      dplyr::group_split(., user_proposal)

  }
}
