norm_srt_data <- function(dt, api) {

  norm_rt_dt(dt, code_col = "sina_code", time_col = "Time", api = api)
}

#' Return default Sina realtime database file path
#'
#' @param wd a working directory
#' @param filename database filename
#'
#' @return a string, file path to database
#' @export
#'
get_srt_db <- function(wd = getwd(), filename = paste0(Sys.Date(), ".db")) {

  get_rt_db(wd = wd, filename = filename)
}

#' Create a Sina realtime database
#'
#' @param db file path to database
#' @param api a tsapi object
#'
#' @return TRUE
#' @export
#'
create_srt_db <- function(db = get_srt_db(), api = TushareApi()) {

  create_rt_db(query_func = sina_realtime_quote,
               ref_code = "sz000001",
               code_col = "sina_code",
               time_col = "Time",
               db = db,
               tbl_name = "sina_realtime",
               idx_unique = "sina_realtime_index",
               idx_dttm = "sina_realtime_index_dttm",
               idx_recv = "sina_realtime_index_recv",
               api = api)
}

#' Query default Sina realtime codes
#'
#' @param stock whether to include stocks
#' @param fund whether to include funds
#' @param index whether to include indices
#'
#' @return a character vector
#' @export
#'
default_srt_codes <- function(stock = TRUE, fund = TRUE, index = TRUE) {

  idx <- shdjt_ashare_code()
  # Remove unrecognised types, a.k.a TypeCN "板块"
  idx <- idx[!is.na(Type)]
  if (!stock) {
    idx <- idx[Type != "stock"]
  }
  if (!fund) {
    idx <- idx[Type != "fund"]
  }
  if (!index) {
    idx <- idx[Type != "index"]
  }
  if (!nrow(idx)) {
    stop("Failed to fetch codes.", call. = FALSE)
  }

  # Convert to Sina code format
  sina_ashare_code(idx$Code)
}

#' Run Sina realtime data loop
#'
#' This function simply query newest quotes from Sina and write to an sqlite
#' database as provided by db argument. Since WAL is used for the database,
#' please make sure that the file is not on a network location.
#'
#' @param codes codes to query
#' @param db file path to database
#' @param today date of today
#' @param api a tsapi object
#'
#' @return this function loops indefinately and does not return
#' @export
#'
sina_realtime_loop <- function(codes = default_srt_codes(), db = get_srt_db(), today = idate_now(), api = TushareApi()) {

  rtlp <- create_rt_loop(query_func = sina_realtime_quote,
                         ref_code = "sz000001",
                         code_col = "sina_code",
                         time_col = "Time",
                         db = db,
                         tbl_name = "sina_realtime",
                         idx_unique = "sina_realtime_index",
                         idx_dttm = "sina_realtime_index_dttm",
                         idx_recv = "sina_realtime_index_recv",
                         api = api)

  rtlp(codes = codes, today = today)
}

#' Generate a Sina realtime data loader function
#'
#' @param db path to database file
#' @param today date of today
#' @param api a tsapi object
#'
#' @return a load function
#' @export
#'
sina_realtime_loader <- function(db = get_srt_db(), today = idate_now(), api = TushareApi()) {

  create_rt_loader(db = db,
                   tbl_name = "sina_realtime",
                   code_col = "Code",
                   time_col = "Time",
                   today = today,
                   api = api)
}
