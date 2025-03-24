#' Lubridate operator to add time
#'
#' See \code{lubridate::\link[lubridate]{\%m+\%}} for details.
#'
#' @name %m+%
#' @rdname madd
#' @keywords internal
#' @export
#' @importFrom lubridate %m+%
#' @usage lhs \%m+\% rhs
#' @param lhs A date-time object.
#' @param rhs A period or duration object.
#' @return The result of adding \code{rhs} to \code{lhs}.
`%m+%` <- function(lhs, rhs) {
  lubridate::`%m+%`(lhs, rhs)
}

#' Lubridate operator to subtract time
#'
#' See \code{lubridate::\link[lubridate]{\%m-\%}} for details.
#'
#' @name %m-%
#' @rdname msubtract
#' @keywords internal
#' @export
#' @importFrom lubridate %m-%
#' @usage lhs \%m-\% rhs
#' @param lhs A date-time object.
#' @param rhs A period or duration object.
#' @return The result of subtracting \code{rhs} from \code{lhs}.
`%m-%` <- function(lhs, rhs) {
  lubridate::`%m-%`(lhs, rhs)
}
