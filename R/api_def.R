get_api_func <- function() {

  api_def <- system.file("api-def", package = "tswbench")
  api_list <- list.files(api_def, pattern = "^arg.+csv$")
  api_list <- stringr::str_sub(api_list, start = 5L, end = -5L)

  api_list
}

get_api_field <- function(func) {

  if (endsWith(func, "_vip")) {
    func <- substr(func, 1L, nchar(func) - 4L)
  }

  fname <- paste0(func, ".csv")
  def_file <- system.file("api-def", fname, package = "tswbench")
  if (!nzchar(def_file)) {
    stop("Cannot find API definition for ", func, call. = FALSE)
  }

  dt <- data.table::fread(def_file)
  if (length(dt) == 3L) {
    dt[, default := TRUE]
    data.table::setnames(dt, c("field", "dtype", "desc", "default"))
  } else {
    data.table::setnames(dt, c("field", "dtype", "default", "desc"))
    dt[, default := as_logical(default)]
  }
  data.table::setcolorder(dt, c("field", "dtype", "default", "desc"))

  dt
}

get_api_arg <- function(func) {

  if (endsWith(func, "_vip")) {
    func <- substr(func, 1L, nchar(func) - 4L)
  }

  fname <- paste0("arg_", func, ".csv")
  def_file <- system.file("api-def", fname, package = "tswbench")
  if (!nzchar(def_file)) {
    stop("Cannot find API definition for ", func, call. = FALSE)
  }

  dt <- data.table::fread(def_file)
  if (length(dt) == 3L) {
    dt[, mandatory := TRUE]
    data.table::setnames(dt, c("arg", "dtype", "desc", "mandatory"))
  } else {
    data.table::setnames(dt, c("arg", "dtype", "mandatory", "desc"))
    dt[, mandatory := as_logical(mandatory)]
  }
  data.table::setcolorder(dt, c("arg", "dtype", "mandatory", "desc"))

  dt
}

#' List available Tushare API functions
#'
#' @return a vector of character
#' @export
#'
tsapi_func <- function() {

  get_api_func()
}

#' Return usage for a Tushare API function
#'
#' @param func function name
#' @param what field: show returned fields, arg: show API arguments
#'
#' @return data.table
#' @export
#'
tsapi_usage <- function(func, what = c("field", "arg")) {

  what <- match.arg(what)
  switch(what,
         field = get_api_field(func)[],
         arg   = get_api_arg(func)[])
}
