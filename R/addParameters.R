#' Add parameters to bench
#'
#' @param b bench object
#' @param label name for method, must already exist
#' @param ... named parameter, value pairs to be used with func
#' @param .post 
#'
#' @return a bench
#' 
#' @export
#' @author Patrick Kimes
add_params <- function(b, label, ..., .list = NULL, overwrite = FALSE) {
    UseMethod("add_params")
}

#' @export
add_params.bench <- function(b, label, ..., .list = NULL, overwrite = FALSE) {
    stopifnot(label %in% names(b$methods))

    ## capture input
    if (quo_text(enquo(.list)) == "NULL") {
        qp <- quos(...)
    } else {
        qp <- enquo(.list)
    }

    ## add to bench
    b$methods[[label]]$params <- qp
    b
}
