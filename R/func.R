# Q: Is it granted that an alignment maximizes the amount of similar
#    characters?


#' Count similar characters of two strings (only for length one vectors).
#'
#' @param x A character vector of length one.
#' @param y A character vector of length one.
#' @return A length one vector for the amount of similar characters between
#' \code{x} and \code{y}.

similar_chars_single <- function(x, y) {
  align <- cba::sdists.center.align(
      c(x, y),
      method = "ow", weight = c(1, 1, 0, 2), to.data.frame = TRUE
    )

  # factor to string
  align <- data.frame(
      lapply(align, as.character),
      stringsAsFactors = FALSE, check.names = FALSE
    )

  with(align, sum(`1` == `2`))
}


#' Calculate similar characters of two string vectors, element-by-element.
#'
#' @param x A character vector.
#' @param y A character vector.
#' @return A vector for the element-wise amount of similar characters between
#' \code{x} and \code{y}.
#' @examples
#' similar_chars(c('dianhua', 'gandong'), c('denwa', 'kando'))
#' @export

similar_chars <- function(x, y) {
  res_len <- max(length(x), length(y))
  res     <- integer(res_len)

  for (i in seq_len(res_len)) {
    if (is.na(x[i]) | is.na(y[i])) {
      res[i] <- NA
    } else {
      res[i] <- similar_chars_single(x[i], y[i])
    }
  }

  res
}


#' Calculate phonological similarity of two string vectors, element-by-element.
#'
#' @param x A character vector.
#' @param y A character vector.
#' @return A vector for the element-wise phonological similarity between
#' \code{x} and \code{y}.
#' @examples
#' similarity(c('dianhua', 'gandong'), c('denwa', 'kando'))
#' @export

similarity <- function(x, y) {
  similar_chars(x, y) * 2 / (nchar(x) + nchar(y))
}
