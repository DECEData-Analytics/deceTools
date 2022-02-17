#' Generate Charter Export. Only Generates one export. Expecting a single dataframe. Please use "prep_charter_scores"
#' to get the dataframe into the expected format
#'
#' @param df a dataframe to enter
#' @param template excel template from passport to use
#' @param file_path_to_save file location to save to.
#'
#' @return
#' @export
#'
#' @examples
generate_charter_export <- function(df, template, file_path_to_save) {

  eval_prop_name <- df$user_proposal

  q1 <- df[[4]]
  q2 <- df[[5]]
  q3 <- df[[6]]
  q4 <- df[[7]]
  q5 <- df[[8]]
  q6 <- df[[9]]
  q7 <- df[[10]]
  q8 <- df[[11]]
  q9 <- df[[12]]
  q10 <- df[[13]]
  comm_1 <- df[[15]]
  comm_2 <- df[[16]]
  comm_3 <- df[[17]]
  comm_4 <- df[[18]]
  comm_5 <- df[[19]]
  comm_6 <- df[[20]]
  comm_7 <- df[[21]]
  comm_8 <- df[[22]]
  comm_9 <- df[[23]]
  comm_10 <- df[[24]]
  blank_comment <- tibble::tibble(x = 1:19) %>% dplyr::mutate_at('x', as.character) %>% dplyr::mutate_at('x', ~ stringr::str_replace_all(., '\\d+', " "))

  wk <- openxlsx::copyWorkbook(template)

  openxlsx::writeData(wk, 'Questionnaire', x = q1, xy = c(5,3))
  openxlsx::writeData(wk, 'Questionnaire', x = q2, xy = c(5,4))
  openxlsx::writeData(wk, 'Questionnaire', x = q3, xy = c(5,6))
  openxlsx::writeData(wk, 'Questionnaire', x = q4, xy = c(5,8))
  openxlsx::writeData(wk, 'Questionnaire', x = q5, xy = c(5,10))
  openxlsx::writeData(wk, 'Questionnaire', x = q6, xy = c(5,12))
  openxlsx::writeData(wk, 'Questionnaire', x = q7, xy = c(5,14))
  openxlsx::writeData(wk, 'Questionnaire', x = q8, xy = c(5,16))
  openxlsx::writeData(wk, 'Questionnaire', x = q9, xy = c(5,18))
  openxlsx::writeData(wk, 'Questionnaire', x = q10, xy = c(5,20))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_1, xy = c(6,3))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_2, xy = c(6,4))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_3, xy = c(6,6))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_4, xy = c(6,8))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_5, xy = c(6,10))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_6, xy = c(6,12))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_7, xy = c(6,14))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_8, xy = c(6,16))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_9, xy = c(6,18))
  openxlsx::writeData(wk, 'Questionnaire', x = comm_10, xy = c(6,20))
  openxlsx::writeData(wk, 'Questionnaire', x = blank_comment, xy = c(3,2), colNames = FALSE)

  openxlsx::sheetVisibility(wk)[1] <- "veryHidden"
  openxlsx::sheetVisibility(wk)[2] <- "veryHidden"
  openxlsx::sheetVisibility(wk)[3] <- TRUE
  openxlsx::sheetVisibility(wk)[4] <- "veryHidden"
  openxlsx::sheetVisibility(wk)[5] <- "veryHidden"
  openxlsx::sheetVisibility(wk)[6] <- "Hidden"
  openxlsx::sheetVisibility(wk)[7] <- "Hidden"
  openxlsx::sheetVisibility(wk)[8] <- "veryHidden"
  openxlsx::sheetVisibility(wk)[9] <- TRUE

  openxlsx::saveWorkbook(wk, file = paste0(file_path_to_save,eval_prop_name,'.xlsx'))

}
