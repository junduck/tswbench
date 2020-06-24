# Not parsed:
#
# Bond
#   120*** SH
#   11**** SZ
# Gov bond spot mkt
#   001*** SH
#   10**** SZ
# Repo
#   201*** SH
#   13**** SZ

#' Parse A-share codes
#'
#' @param code a vector of A-share codes
#' @param type type of security
#' @param SSE symbol for Shanghai Stock Exchange
#' @param SZSE symbol for Shenzhen Stock Exchange
#'
#' @return a list of code and exchange
#' @export
#'
parse_ashare_code <- function(code,
                              type = c("stock", "index", "fund", "conv_bond"),
                              SSE = "SH", SZSE = "SZ") {

  type <- match.arg(type)

  co_parsed <- vector(mode = "character", length = length(code))
  se_parsed <- vector(mode = "character", length = length(code))

  #parse numeric code
  num_idx  <- stringr::str_detect(code, pattern = stringr::regex("^[0-9]+$"))
  if (any(num_idx)) {
    #extract numeric code
    num_co <- code[num_idx]
    num_se <- vector(mode = "character", length = length(num_co))
    #parse numeric code base on type
    switch(type,
           stock = {
             sh_idx <- stringr::str_detect(num_co, "^[679]")
             sz_idx <- stringr::str_detect(num_co, "^[023]")
             #sz_idx <- !sh_idx
           },
           index = {
             sh_idx <- stringr::str_starts(num_co, stringr::fixed("0"))
             sz_idx <- stringr::str_starts(num_co, stringr::fixed("3"))
             #sz_idx <- !sh_idx
           },
           fund = {
             sh_idx <- stringr::str_starts(num_co, stringr::fixed("5"))
             sz_idx <- stringr::str_starts(num_co, stringr::fixed("1"))
             #sz_idx <- !sh_idx
           },
           conv_bond = {
             sh_idx <- stringr::str_starts(num_co, stringr::fixed("10"))
             sz_idx <- stringr::str_starts(num_co, stringr::fixed("12"))
             #sz_idx <- !sh_idx
           })
    if (any(sh_idx)) {
      num_se[sh_idx] <- SSE
    }
    if (any(sz_idx)) {
      num_se[sz_idx] <- SZSE
    }
    co_parsed[num_idx] <- num_co
    se_parsed[num_idx] <- num_se
  }

  #parse code with SE attached already
  if (!all(num_idx)) {
    cha_idx <- !num_idx
    cha_co <- code[cha_idx]
    cha_se <- stringr::str_extract(cha_co, "[a-zA-Z]+")
    if (anyNA(cha_se)) {
      cha_se[is.na(cha_se)] <- ""
    }
    co_parsed[cha_idx] <- stringr::str_extract(cha_co, "[0-9]+")
    se_parsed[cha_idx] <- cha_se
  }

  list(
    code     = co_parsed,
    exchange = se_parsed
  )
}

#' Normalise A-share codes
#'
#' Normalise codes to CODES.EX format
#'
#' @param code a vector of A-share codes
#' @param type type of security
#'
#' @return normalised codes
#' @export
#'
norm_ashare_code <- function(code, type = c("stock", "index", "fund", "conv_bond")) {

  parsed <- parse_ashare_code(code = code, type = type, SSE = "SH", SZSE = "SZ")
  parsed_idx <- nzchar(parsed$exchange)

  ans <- parsed$code
  ans[parsed_idx] <- paste0(ans[parsed_idx], ".", toupper(parsed$exchange[parsed_idx]))
  ans
}

#' Normalise A-share codes to Sina format
#'
#' @param code a vector of A-share codes
#' @param type type of security
#'
#' @return normalised codes
#' @export
#'
sina_ashare_code <- function(code, type = c("stock", "index", "fund", "conv_bond")) {

  parsed <- parse_ashare_code(code = code, type = type, SSE = "sh", SZSE = "sz")
  parsed_idx <- nzchar(parsed$exchange)

  ans <- parsed$code
  ans[parsed_idx] <- paste0(tolower(parsed$exchange[parsed_idx]), ans[parsed_idx])
  ans
}
