#' Generates excel file outputs for upload into passport for the 3kpk 1395 rfp
#'
#' @param df which dataset/readscores to use
#' @param template which excel template/export from passport to use
#' @param file_path_to_save where do you want the files saved. Character file path
#'
#' @return saves filled workbook to whichever directory you choose
#' @export
#'
#' @examples
#'
#'
generate_passport_exports <- function(df, template, file_path_to_save) {

  if (is.na(df$rfp_type)) {
    errorCondition(message = "rfp_type is missing")
  } else if (df$rfp_type == "3K/PK (1395)") {

    eval_prop_name <- df$user_proposal

    q1 <- df[[5]]
    q2 <- df[[9]]
    q3 <- df[[10]]
    q4a <- df[[11]]
    q4b <- df[[12]]
    q5 <- df[[13]]
    q6a <- df[[14]]
    q6b <- df[[15]]
    q7 <- df[[16]]
    q8 <- df[[17]]
    q9 <- df[[18]]
    q10a <- df[[19]]
    q10b <- df[[20]]
    q11 <- df[[21]]
    q12 <- df[[22]]
    comments <- df[[23]]
    blank_comment <- tibble::tibble(x = 1:26) %>% dplyr::mutate_at('x', as.character) %>% dplyr::mutate_at('x', ~ str_replace_all(., '\\d+', " "))

    wk <- openxlsx::copyWorkbook(template)

    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q1, xy = c(5,3))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q2, xy = c(5,5))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q3, xy = c(5,6))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q4a, xy = c(5,8))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q4b, xy = c(5,9))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q5, xy = c(5,11))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q6a, xy = c(5,13))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q6b, xy = c(5,14))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q7, xy = c(5,16))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q8, xy = c(5,18))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q9, xy = c(5,20))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q10a, xy = c(5,22))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q10b, xy = c(5,23))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q11, xy = c(5,25))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q12, xy = c(5,27))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = comments, xy = c(6,27))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = blank_comment, xy = c(3,2), colNames = FALSE)

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
  } else if (df$rfp_type == "COL (1396)") {
    eval_prop_name <- df$user_proposal

    q1a <- df[[6]]
    q1b <- df[[7]]
    q1c <- df[[8]]
    q2 <- df[[9]]
    q3 <- df[[10]]
    q4a <- df[[11]]
    q4b <- df[[12]]
    q5 <- df[[13]]
    q6a <- df[[14]]
    q6b <- df[[15]]
    q7 <- df[[16]]
    q8 <- df[[17]]
    q9 <- df[[18]]
    q10a <- df[[19]]
    q10b <- df[[20]]
    q11 <- df[[21]]
    q12 <- df[[22]]
    comments <- df[[23]]
    blank_comment <- tibble::tibble(x = 1:26) %>% dplyr::mutate_at('x', as.character) %>% dplyr::mutate_at('x', ~ str_replace_all(., '\\d+', " "))

    wk <- openxlsx::copyWorkbook(template)

    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q1a, xy = c(5,3))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q1b, xy = c(5,4))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q1c, xy = c(5,5))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q2, xy = c(5,7))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q3, xy = c(5,8))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q4a, xy = c(5,10))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q4b, xy = c(5,11))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q5, xy = c(5,13))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q6a, xy = c(5,15))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q6b, xy = c(5,16))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q7, xy = c(5,18))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q8, xy = c(5,20))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q9, xy = c(5,22))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q10a, xy = c(5,24))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q10b, xy = c(5,25))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q11, xy = c(5,27))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = q12, xy = c(5,29))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = comments, xy = c(6,29))
    openxlsx::writeData(wk, 'Scored Narrative Program Vision', x = blank_comment, xy = c(3,2), colNames = FALSE)

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

  } else if (!any(df$rfp_type == '3K/PK (1395)', df$rfp_type == 'COL (1396)')) {
    errorCondition(message = paste0(df$Id,'is of wrong rfp type. rfp is', df$rfp_type))
  }


}
