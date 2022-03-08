#' Final 3KPK Score
#'
#' Uses the scores & variables created during the prep scores call to find what should be the
#'  final score proportions out of 5.
#'
#'  Should show up to 2 decimal places
#'
#'  To be used in a mutate in association with the prep scores call
#'
#'  INTERNAL FUNCTION FOR THIS PACKAGE
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
final_3kpk_score = function(.data) {

  x <-
    ((.data$final_pgm_vision / 5) * 15) +
    ((.data$EquityAndAccess_2 / 5) * 7) +
    ((.data$RespectingDifferences_3 / 5) * 8) +
    ((.data$PositiveClassCulture_4A / 5) * 6) +
    ((.data$PositiveClassCulture_4B / 5) * 6) +
    ((.data$final_5 / 5) * 8) +
    ((.data$final_6a / 5) * 8) +
    ((.data$final_6b / 5) * 7) +
    .data$ResponsiveInstruction_7 +
    .data$CollabWithFamilies_8 +
    .data$CargiversTeachersAdvocates_9 +
    .data$final_10a +
    .data$final_10b +
    .data$QualifiedLeadership_11 +
    .data$OrgCultureCommunity_12

  y <- round((x / 100) * 5, digits = 2)

  return(y)

}
