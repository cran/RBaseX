#' Next
#'
#' @param query_obj QueryClass instance-ID
#'
#' @description Returns the next result when iterating over a query
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
Next <- function(query_obj) {
  return(query_obj$query$Next())
}
