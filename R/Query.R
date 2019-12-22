#' Query
#'
#' @param session BasexClient instance-ID
#' @param query_string query string
#'
#' @return Query_ID
#'
#' @description Creates a new query instance and returns its id.
#'
#' @examples
#' \dontrun{
#' query_txt <- "for $i in 1 to 2 return <xml>Text { $i }</xml>"
#' query_obj <- Query(Session, query_txt)
#' print(Execute(query_obj))
#' }
#'
#' @export
Query <- function(session, query_string) {
  return(list(queryObject = QueryClass$new(query_string, session$getSocket()),
              success     = session$bool_test_sock())
              # success = session$success
              # success = session$get_success())
  )
}
