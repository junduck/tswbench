quote_id <- function(x) {

  if (length(x) == 1 && x == "*") {
    x
  } else {
    x <- stringr::str_replace(x, stringr::fixed('"'), '""')
    paste0('"', x, '"')
  }
}

quote_val <- function(x) {

  if (is.character(x)) {
    stringr::str_replace(x, stringr::fixed("'"), "''")
    paste0("'", x, "'")
  } else {
    x
  }
}

quote_where <- function(..., op. = c("AND", "OR")) {

  op. <- match.arg(op.)
  op. <- paste0(" ", op., " ")

  cond_val <- list(...)
  if (!length(cond_val)) {
    return("")
  }

  cond_var <- names(cond_val)
  if (is.null(cond_var)) {
    stop("Where variables must be named.", call. = FALSE)
  } else {
    cond_var <- quote_id(cond_var)
  }

  n <- length(cond_val)
  q <- vector(mode = "character", length = n)
  for (i in seq_along(cond_val)) {
    if (length(cond_val[[i]]) == 1L) {
      q[i] <- sprintf("%s = %s", cond_var[i], quote_val(cond_val[[i]]))
    } else {
      q[i] <- sprintf("%s IN (%s)", cond_var[i], paste0(quote_val(cond_val[[i]]), collapse = ","))
    }
  }

  paste("WHERE",
        paste0(q, collapse = op.))
}

quote_sql_id <- function(con, what) {

  if (length(what) == 1L && what == "*") {
    what
  } else {
    DBI::dbQuoteIdentifier(con, what)
  }
}

#' Select from an SQL connection
#'
#' NOTICE: there is no safety check of argument where in select_from_where()
#'
#' @param con an SQL connection
#' @param tbl name of table to query
#' @param what what variables/columns to query
#' @param where a string of the where clause (without "WHERE")
#' @param ... named arguments, parsed as WHERE AND clause
#'
#' @return data.table
#' @export
#'
select_from <- function(con, tbl, what = "*", ...) {

  what <- quote_sql_id(con, what)
  tbl  <- quote_sql_id(con, tbl)
  q <- sprintf('SELECT %s FROM %s', paste0(what, collapse = ','), tbl)

  if (...length()) {
    where <- data.table::CJ(..., unique = TRUE)
    nwhere <- nrow(where)
  } else {
    nwhere <- 0L
  }

  if (nwhere) {
    #construct where clause
    var <- seq_along(where)
    var_quoted <- quote_sql_id(con, names(where))
    where_clause <- sprintf("%s = $%d", var_quoted, var)
    q <- paste(q, "WHERE", paste0(where_clause, collapse = " AND "))
  }

  if (nwhere) {
    r <- DBI::dbSendQuery(con, q)
    on.exit(DBI::dbClearResult(r))
    DBI::dbBind(r, unname(as.list(where)))
    ans <- DBI::dbFetch(r)
  } else {
    ans <- DBI::dbGetQuery(con, q)
  }

  data.table::setDT(ans)
}

#' @rdname select_from
#' @export
#'
select_from_where <- function(con, tbl, what = "*", where = "") {

  what <- quote_sql_id(con, what)
  tbl  <- quote_sql_id(con, tbl)
  q <- sprintf('SELECT %s FROM %s', paste0(what, collapse = ','), tbl)

  if (nzchar(where)) {
    q <- paste(q, "WHERE", where)
  }

  ans <- DBI::dbGetQuery(con, q)

  data.table::setDT(ans)
}

#' Insert data to an SQL connection
#'
#' @param con an SQL connection
#' @param tbl name of table to insert to
#' @param dt data to insert
#' @param conflict how to deal with conflict
#' @param constraint constrained columns, only used when conflict == "replace" and connection is to a PostgreSQL database
#'
#' @return number of rows inserted
#' @export
#'
insert_to <- function(con, tbl, dt, conflict = c("default", "replace", "ignore"), constraint = data.table::key(dt)) {

  conflict <- match.arg(conflict)
  tbl_temp <- paste0(tbl, "_tmp_", digest::digest(dt, algo = "xxhash32"))
  if (methods::is(con, "SQLiteConnection")) {
    # SQLite
    q_template <- switch(conflict,
                         default = "INSERT INTO %s SELECT * FROM %s",
                         replace = "INSERT OR REPLACE INTO %s SELECT * FROM %s",
                         ignore  = "INSERT OR IGNORE INTO %s SELECT * FROM %s")
  } else if (methods::is(con, "PqConnection")) {
    # PostgreSQL
    q_template <- switch(conflict,
                         default = "INSERT INTO %s SELECT * FROM %s",
                         replace = {
                           if (is.null(constraint)) {
                             stop("Unique/exclusion constraint is required to perform update.", call. = FALSE)
                           }
                           cols <- quote_sql_id(con, names(dt))
                           replacement <- paste0(sprintf("%s = EXCLUDED.%s", cols, cols), collapse = ", ")
                           constraint <- quote_sql_id(con, constraint)
                           clause <- sprintf("ON CONFLICT (%s) DO UPDATE SET %s", paste0(constraint, collapse = ", "), replacement)
                           paste("INSERT INTO %s SELECT * FROM %s", clause)
                         },
                         ignore  = "INSERT INTO %s SELECT * FROM %s ON CONFLICT DO NOTHING")
  } else if (methods::is(con, "MariaDBConnection")) {
    # MariaDB
    q_template <- switch(conflict,
                         default = "INSERT INTO %s SELECT * FROM %s",
                         replace = "REPLACE INTO %s SELECT * FROM %s",
                         ignore  = "INSERT IGNORE INTO %s SELECT * FROM %s")
  } else {
    # Other
    cls <- class(con)
    q_template <- switch(conflict,
                         default = "INSERT INTO %s SELECT * FROM %s",
                         replace = stop("Don't know how to insert replace for ", cls, call. = FALSE),
                         ignore  = stop("Don't know how to insert ignore for ", cls, call. = FALSE))
  }
  q <- sprintf(q_template, tbl, tbl_temp)

  # Write to temp table
  DBI::dbWriteTable(conn = con, name = tbl_temp, value = dt,
                    overwrite = TRUE, temporary = TRUE, copy = TRUE)

  # Insert from temp table
  r <- tryCatch({
    DBI::dbBegin(con)
    r <- DBI::dbExecute(con, q)
    DBI::dbCommit(con)
    r
  }, error = function(e) {
    DBI::dbRollback(con)
    warning(toString(e), call. = FALSE)
    0L
  })

  r
}

#' Create index on a table
#'
#' @param con an SQL connection
#' @param name name of index
#' @param tbl name of table
#' @param var variables/columns to create index
#' @param ASC NULL if not specified, TRUE for ascending and FALSE for descending order
#' @param unique whether to create a unique index
#'
#' @return TRUE/FALSE
#' @export
#'
create_index <- function(con, name, tbl, var, ASC = NULL, unique = FALSE) {

  tbl <- quote_sql_id(con, tbl)
  var <- quote_sql_id(con, var)
  name <- quote_sql_id(con, name)
  if (unique) {
    unique = "UNIQUE"
  } else {
    unique <- ""
  }

  n_order <- length(ASC)
  if (n_order) {
    if (n_order == 1L) {
      if (ASC) {
        order <- "ASC"
      } else {
        order <- "DESC"
      }
    } else if (n_order == length(var)) {
      order <- ifelse(ASC, "ASC", "DESC")
    } else {
      warning("ASC should be of length 1 or same as var.", call. = FALSE)
      return(FALSE)
    }
  } else {
    order <- ""
  }
  var <- paste(var, order)

  q_template <- "CREATE %s INDEX %s ON %s (%s)"
  q <- sprintf(q_template, unique, name, tbl, paste0(var, collapse = ", "))

  r <- tryCatch({
    DBI::dbBegin(con)
    DBI::dbExecute(con, q)
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    DBI::dbRollback(con)
    warning(toString(e), call. = FALSE)
    FALSE
  })

  r
}

#' Create an empty table from reference tabular data
#'
#' @param con an SQL connection
#' @param name name of table
#' @param dt reference tabular data
#' @param create_index create a default index if dt is data.table and keyed
#'
#' @return TRUE/FALSE
#' @export
#'
create_table <- function(con, name, dt, create_index = FALSE) {

  dt <- dt[0L, , drop = FALSE]
  r <- tryCatch({
    DBI::dbWriteTable(con, name, dt)
    TRUE
  }, error = function(e) {
    warning(toString(e), call. = FALSE)
    FALSE
  })

  if (r && create_index) {
    r <- tryCatch({
      index <- data.table::key(dt)
      if (length(index)) {
        r <- create_index(con, name = paste0(name, "_index"), tbl = name, var = index, unique = FALSE)
      }
      if (!r) {
        DBI::dbRemoveTable(con, name)
      }
      r
    }, error = function(e) {
      warning(toString(e), call. = FALSE)
      FALSE
    })
  }

  r
}
