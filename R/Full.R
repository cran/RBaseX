#' Title Full
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Executes a query and returns a vector with all resulting items as strings,
#'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
#'
#' @examples
#' \dontrun{
#' query_txt <- "for $i in 1 to 2 return <xml>Text { $i }</xml>"
#' query_obj <- Query(Session, query_txt)
#'
#' print(Full(query_obj))  ## Return "0b" "<xml>Text 1</xml>" "0b" "<xml>Text 2</xml>"
#'
#' }
#'
#' @export
Full <- function(query_obj) {
  return(query_obj$query$Full())
}
