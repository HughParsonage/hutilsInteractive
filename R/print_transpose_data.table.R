#' Print transposed data.table
#' @param DT A \code{data.table}.
#' @param sep Passed to \code{\link[base]{cat}}: the separation put horizontally between each row in \code{DT}.
#' @param file Passed to \code{\link[base]{cat}}. If \code{file = ""}, the default,
#' output is printed to the console. Otherwise, result will be sunk to a file.
#' @param append Should the result be appended to \code{file}? Not applicable if \code{file = ""}.
#' @param logical For logical columns, the characters representing \code{TRUE}, \code{FALSE}, and \code{NA}, respectively.
#' @param zero String for zero.
#' @param omit_if Only valid if \code{nrow(DT) == 1}.
#' Either a function, a single value, or a vector. If a function, \code{TRUE}
#' values will be omitted; if a single value, values equal will be omitted; if a vector, values
#' \code{\%in\%} will be dropped. \code{NULL} means no omission.
#' @details \code{NA}s will be replaced by \code{.} and backslashes will be replaced by \code{@ } (to avoid backslashes).
#' @export

print_transpose_data.table <- function(DT, sep = "", file = "", append = FALSE,
                                       logical = c("\u25a0", " ", "."),
                                       omit_if = NULL,
                                       zero = "-") {
  max_nchar <- function(v) {
    if (is.logical(v)) {
      v <- hutils::if_else(v, logical[1], logical[2], missing = logical[3])
    }
    v_na <- is.na(v)
    out <- as.character(v)
    out[v_na] <- ""
    out[v == "\\"] <- "@"
    out <- sub("\\", "@", out, fixed = TRUE)
    max(nchar(encodeString(out), type = "width"))
  }

  char_width <- max(vapply(DT, max_nchar, integer(1)))
  max_width_names <- max(nchar(names(DT)))

  if (nzchar(file) && !file.exists(file)) {
    provide.dir(dirname(file))
  }

  if (!append && nzchar(file) && file.exists(file)) {
    file.remove(file)
  }

  is_single_row <- nrow(DT) == 1L

  for (var in names(DT)) {
    v <- DT[[var]]
    if (is_single_row && !is.null(omit_if)) {
      if (is.function(omit_if)) {
        if (omit_if(v)) {
          next
        }
      } else if (length(omit_if) == 1L) {
        if (v == omit_if) {
          next
        }
      } else if (is.atomic(omit_if)) {
        if (v %in% omit_if) {
          next
        }
      }
    }
    # append = TRUE otherwise last cat will overwrite
    cat(formatC(var, width = max_width_names), " : ",
        sep = "", file = file, append = TRUE)
    v_na <- is.na(v)
    switch(typeof(v),
           "logical" = {
             v <- hutils::if_else(v, logical[1], logical[2], missing = logical[3])
           },
           "integer" = {
             v0 <- v
             v <- as.character(v)
             v[v0 == 0L] <- zero
           },
           {
             v <- as.character(v)
             v[v == "\\"] <- "@"
             v <- sub("\\", "@", v, fixed = TRUE)
           })
    v[v_na] <- "."

    v <- stringr::str_pad(v, char_width, side = "left")
    # stop(var)
    cat(v, sep = sep, file = file, append = TRUE)
    cat("\n", sep = sep, file = file, append = TRUE)
  }
}
