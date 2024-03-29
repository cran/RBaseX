#' Query
#'
#' @param session BasexClient instance-ID
#' @param query_string query string
#'
#' @return Query_ID
#'
#' @description Creates a new query instance and returns it's id.
#'
#' @details
#'
#'     If paste0() is used to create a multi-line statement, the lines must be separeted by a space or a newline \\n-character.
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
  if (missing(query_string)) {
    session$set_success(FALSE)
    if (session$get_intercept()) {
      return(list(queryObject = NULL, success = session$get_success()))
    } else stop("No query-string provided")
  }
  tryCatch(
    { queryObject <- QueryClass$new(query_string, session)
    success <- session$get_success()
      return(list(queryObject = queryObject, success = success))
    },
    error = function(e) {
      session$set_success(success)
      if (session$get_intercept()) {
        return(list(queryObject = NULL, success = session$get_success()))
      } else {
        message("Error creating the query-object")
        stop()}
    }
  )
}
