#' @title QueryClass
#'
#' @description The client can be used in 'standard' mode and in 'query' mode.
#'     Query mode is used to define queries, binding variables and for iterative evaluation.
#'
#' @export
QueryClass <- R6Class("QueryClass",
  inherit = BasexClient,

  public = list(
    #' @description Initialize a new instance from QueryClass
    #' @details QueryClass-instances can only be created by calling the 'Query'-method from the 'BasexClient'-class
    #' @param query Query-string
    #' @param sock Session-socket
    initialize = function(query, sock) {
      private$sock <- sock
      out_stream <- super$get_sock()
      writeBin(as.raw(0x00), out_stream)
      super$void_send(query)
      private$raw_id <- private$term_string(super$str_receive())},

    #' @description Returns the next result when iterating over a query
    Next = function() {
      if (self$More()) {
        private$pos <- private$pos + 1
        result <- private$cache[private$pos]
      }
      return(result)},
    #' @description     Closes and unregisters the query with the specified ID
    Close = function() {
      private$default_query_pattern(match.call()[[1]], private$raw_id)
    },
    #' @description Executes a query.
    ExecuteQuery = function() {
      private$default_query_pattern(match.call()[[1]], private$raw_id)
    },
    #' @description Returns a string with query compilation and profiling info.
    Info = function() {
      private$default_query_pattern(match.call()[[1]], private$raw_id)
      },
    #' @description Returns a string with all query serialization parameters, which
    #'     can e.g. be assigned to the serializer option.
    Options = function() {
      private$default_query_pattern(match.call()[[1]], private$raw_id)
      return(ifelse(!private$req_result == "",
               private$req_result %>% private$clean(), "No options set")
            )
      },
    #' @description Check if the query contains updating expressions.
    Updating = function() {
      private$default_query_pattern(match.call()[[1]], private$raw_id)
      private$req_result %<>% as.logical()
      },

    #' @description Binds a value to a variable.
    #' @param query_obj QueryClass instance-ID
    #' @param ... Binding Information
    Bind = function(...) {
      socket <- super$get_sock()
      private$write_code_ID(0x03, private$raw_id)
      writeBin(Binding(list(...)), socket)
      private$req_result <- super$str_receive()
      test_byte <- private$get_response_byte()
      if ( test_byte == 0x00) {
        private$req_success <- TRUE
        return(private$req_success)
      } else {
        error_msg <- super$str_receive()
        stop(error_msg)
      }
    },
    #' @description Binds a value to the context. The type will be ignored if the string is empty.
    #' @param value Value that should be boud to the context
    #' @param type The type will be ignored when the string is empty
    Context = function(value, type) {
      if (missing(type)) type <- ""
      socket <- super$get_sock()
      private$write_code_ID(0x0E, private$raw_id)
      cont_info <- charToRaw(value) %>% append(0) %>% as.raw()
      if (type == "") {
        cont_info %<>% append(0x00) %>% as.raw()
      } else {
        cont_info %<>% append(charToRaw(type)) %>% append(0x00) %>% as.raw()
      }
      writeBin(cont_info, socket)
      private$req_result <- super$str_receive()
      test_byte <- private$get_response_byte()
      if ( test_byte == 0x00) {
        private$req_success <- TRUE
        return(private$req_success)
      } else {
        error_msg <- super$str_receive()
        stop(error_msg)
      }
    },
    #' @description Indicates if there are any other results in the query-result.
    More = function() {
      if (is.null(private$cache)) {
        in_stream <- super$get_sock()
        private$write_code_ID(0x04, private$raw_id)
        cache <- c()
        while ((rd <- readBin(in_stream, what = "raw", n =1)) > 0) {
          cache <- c(cache, as.character(rd))
          cache <- c(cache, super$str_receive())
        }
        private$req_success <- ifelse(private$get_response_byte() == 0x00, TRUE, FALSE)
        private$cache <- cache
        private$pos <- 0
      }
      if ( length(private$cache) > private$pos) return(TRUE)
      else {
        private$cache <- NULL
        return(FALSE)
      }},
    #' @description Executes a query and returns a vector with all resulting items as strings,
    #'     prefixed by the 'XDM' (Xpath Data Model) Meta Data <https://www.xdm.org/>.
    Full = function() {
      in_stream <- out_stream <- super$get_sock()
      private$write_code_ID(0x1F, private$raw_id)
      cache <- c()
      while ((rd <- readBin(in_stream, what = "raw", n =1)) > 0) {
        cache <- c(cache, as.character(rd))
        cache <- c(cache, super$str_receive())
      }
      private$req_success <- ifelse(private$get_response_byte() == 0x00, TRUE, FALSE)
      result <- cache
      return(result)
      }
    # Print = function(...) {
    #   cat("Query-ID: ", self$str_id, "\n", sep = "")
    #   invisible(self)}
  ),

  private = list(
    raw_id = NULL,
    cache = NULL,
    pos = NULL,
    req_result = NULL,
    req_success = NULL,
    term_string = function(string) {
      return(charToRaw(string) %>% append(0) %>% as.raw())},
    get_response_byte = function(socket) {
      if (missing(socket)) socket <- super$get_sock()
      test <- readBin(socket, what = "raw", n =1)
    },
    write_code_ID = function(id_code, arg) {
      out_stream <- super$get_sock()
      writeBin(as.raw(id_code), out_stream)
      writeBin(arg, out_stream)},
    receive_more = function(input, output) {
      if (missing(input)) input   <- private$get_sock()
      if (missing(output)) output <- raw(0)
      while ((rd <- readBin(input, what = "raw", n =1)) > 0) {
        if (rd == 0xff) next
        output <- c(output, rd)
      }
      ret <- rawToChar(output)
      return(ret)},
    clean = function(input) {
      if (input == "") return(input)
      else {
        result <- input %>% strsplit("\n", fixed = TRUE)
        if ((result[[1]][1]  == "")) result <- result[[1]][2]
      }
      return(result)
    },
    default_query_pattern = function(Caller, Query_ID) {
      switch(as.character(Caller[[3]]),
             "Close"        =  private$write_code_ID(0x02, private$raw_id),
             "ExecuteQuery" =  private$write_code_ID(0x05, private$raw_id),
             "Info"         =  private$write_code_ID(0x06, private$raw_id),
             "Options"      =  private$write_code_ID(0x07, private$raw_id),
             "Updating"     =  private$write_code_ID(0x1E, private$raw_id)
      )
      private$req_result <- super$str_receive()
      test_byte <- private$get_response_byte()
      if (test_byte == 0x00)
        result <- private$req_result %>% private$clean()
      else {
        error_msg <- super$str_receive()
        self$Close()
        stop(error_msg) }
    }
  )
)

Binding <- function(...) {
  arguments <- list(...)[[1]]
  name <- arguments[[1]]
  vals <- arguments[[2]]
  argCnt <- length(arguments)

  retour <- charToRaw(name) %>% append(0) %>% as.raw()
  if (argCnt == 3) {                                        # Name, value and type
    retour %<>% append(charToRaw(vals)) %>% append(0x00) %>%
      append(charToRaw(arguments[[3]])) %>% as.raw()
  } else if (is.vector(vals)){                                # Name, list of values
    valLength <- length(vals)
    for (i in 1:length(vals)) {
      retour %<>% append(charToRaw(vals[[i]][[1]]))
      if (length(vals[[i]]) ==2) {
        retour %<>% append(0x02) %<>% as.raw() %<>%
          append(charToRaw(vals[[i]][[2]])) %<>% as.raw()
      }
      if (i < valLength) retour %<>% append(0x01) %<>% as.raw() else
        retour %<>% append(0x00) %<>% as.raw()
    }
  } else {                                                  # Name, value
    retour %<>% append(charToRaw(vals)) %>% append(0x00) %>%
      append(charToRaw("")) %>% as.raw()
  }
  retour %<>% append(0x00) %>% as.raw()
  return(retour)
}
