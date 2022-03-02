#' Not in infix operator
#'
#' @description negates the %in% infix op, looks for things that are not in your list
#'
#' @export
#' @examples
#' c(1,2,3) %notin% c(3,4,5)

`%notin%` <- Negate(`%in%`)


