#' More
#'
#' @param query_obj QueryClass instance-ID
#'
#' @return Boolean
#'
#' @description Indicates if there are any other results in the query-result.
#'
#' @examples
#' \dontrun{
#' query2 <- "for $i in 1 to 3 return <xml>Text { $i }</xml>"
#' query_iterate <- Query(Session, query2)
#' while (More(query_iterate)) {
#'   cat(Next(query_iterate), "\n")
#'   }
#' }
#'
#' @export
More <- function(query_obj) {
  return(query_obj$query$More())
}
