# R client for 'BaseX'.
# Works with BaseX 8.0 and later
#
# Documentation: http://docs.basex.org/wiki/Clients
#
# (C) Ben Engbers

#' @title BasexClient
# #' @docType package
# #' @name RBaseX
#'
#' @description The client can be used in 'standard' mode and in 'query' mode.
#'     Standard Mode is used for connecting to a server and sending commands.
#'
#' @export
BasexClient <- R6Class("BasexClient",
  public = list(
    #' @description Initialize a new client-session
    #' @param host,port,username,password Host-information and user-credentials
    initialize = function(host, port = 1984L, username, password) {
      tryCatch(
        {private$sock <- socketConnection(host = "localhost", port,
                                          open = "w+b", server = FALSE, blocking = TRUE, encoding = "utf-8")
        },
        error = function(e) {
          message("Cannot open the connection")
          stop()}
      )
      private$response <- self$str_receive()
      splitted <-strsplit(private$response, "\\:")
      ifelse(length(splitted[[1]]) > 1,
             { code  <- paste(username, splitted[[1]][1],password, sep=":")
             nonce <- splitted[[1]][2]},
             { code  <- password
             nonce <- splitted[[1]][1]
             }
      )
      code <- md5(paste(md5(code), nonce, sep = ""))
      class(code) <- "character"
      private$void_send(username)
      private$void_send(code)
      tryCatch(
        {if (!self$bool_test_sock()) stop("Access denied")},
        error = function(e) {
          message(e)
          stop()
        }
      )
    },

    #' @description Execute a command
    #' @param command Command
    #' @details For a list of database commands see \url{http://docs.basex.org/wiki/Commands}
    Execute = function(command) {
      bin <- if (grepl("retrieve\\s+", command, ignore.case = TRUE)) TRUE
      else FALSE
      private$void_send(command)
      private$result <- self$str_receive(bin = bin)
      private$info <-   self$str_receive()
      if (class(private$result) == "character") {result <- private$result %>% strsplit("\n")}
      else result <- private$result
      if (length(private$info) > 0) cat(private$info, "\n")
      return(list(result = result, info = private$info, success = self$bool_test_sock()))
    },

    #' @description Create a new query-object
    #' @details A query-object has two fields. 'queryObject' is an ID for the new created 'QueryClass'-instance.
    #'     'success' holds the status from the last executed operation on the queryObject.
    #' @param query Query-string
    #' @return ID for the created query-object
    Query = function(query) {
      return(list(queryObject = QueryClass$new(query, private$get_sock()), success = self$bool_test_sock()))
    },

    #' @description Add a new resouce at the specified path
    #' @param path Path
    #' @param input File, directory or XML-string
    Add = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
    },

    #' @description Create a new database
    #' @details Initial content can be offered as string, URL or file.
    #' @param name Name
    #' @param input Initial content, Optional
    Create = function(name, input) {
      if (missing(input)) input <- ""
      private$default_pattern(match.call()[[1]], name, input)
    },

    #' @description Replace resource, adressed by path
    #' @param path Path
    #' @param input File, directory or XML-string
    Replace = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
    },

    #' @description Store binary content
    #' @details Binary content can be retrieved by executing a retrieve-command
    #' @param path Path
    #' @param input File, directory or XML-string
    Store = function(path, input) {
      private$default_pattern(match.call()[[1]], path, input)
    },

    #' @description Return a boolean that indicates the result from the last action on the socket
    #' @param socket Socket-ID
    bool_test_sock = function(socket) {
      if (missing(socket)) socket <- private$get_sock()
      test <- readBin(socket, what = "raw", n =1)
      return(test == 0x00)
    },

    #' @description Read a string from a stream
    #' @details This method is not intented to be called direct. Due to the lack of a 'protected' classifier for R6, this is a 'public' method
    #' @param input,output Input- and output-stream
    #' @param bin Boolean; TRUE when str_receive has to retrieve binary data
    str_receive = function(input, output, bin = FALSE) {
      if (missing(input)) input   <- private$get_sock()
      if (missing(output)) output <- raw(0)
      while ((rd <- readBin(input, what = "raw", n =1)) > 0) {
        if (rd == 0xff) rd <- readBin(input, what = "raw", n =1)
        output <- c(output, rd)
      }
      if (!bin) ret <- rawToChar(output)
      else ret <- output
      return(ret)},

    #' @description Get socket-ID
    getSocket = function() {private$sock}
  ),

  private = list(
    result = NULL,
    info = NULL,
    partial = NULL,
    errorMsg = NULL,
    sock = NULL,
    response = NULL,
    get_sock = function() { private$sock },
    close_sock = function() { close(self$sock)},
    void_send = function(input) {
      if (class(input) == "character") {
        streamOut <- charToRaw(input)
      } else {
        rd_id <- 1
        end <- length(input)
        streamOut <- raw()
        while (rd_id <= end) {
          rd <- c(input[rd_id])
          if (rd == 255 || rd == 0) streamOut <- c(streamOut, c(0xFF))
          rd_id <- rd_id + 1
          streamOut <- c(streamOut, rd)
        }
      }
      streamOut <- c(streamOut, c(0x00)) %>% as.raw()
      writeBin(streamOut, private$get_sock())
    },
    default_pattern = function(Caller, path, input) {
      if (missing(path) || missing(input)) {
        stop("'path' and/or 'input' are missing")
      } else {
        switch(as.character(Caller[[3]]),
               "Create"  =  writeBin(as.raw(0x08), private$sock),
               "Add"     =  writeBin(as.raw(0x09), private$sock),
               "Replace" =  writeBin(as.raw(0x0C), private$sock),
               "Store"   =  writeBin(as.raw(0x0D), private$sock)
        )
        private$void_send(path)
        input <- input_to_raw(input)
        private$void_send(input)

        private$partial <- self$str_receive()
        test <- readBin(private$get_sock(), what = "raw", n = 1)
        if (test == 0x00) {
          return(list(info = private$partial, success = TRUE))}
      else {
          errorMsg <- self$str_receive()
          stop(errorMsg)
        }
      }
    }
  )
)
