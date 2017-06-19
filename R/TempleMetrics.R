#' @title helloWorld
#'
#' @description print Hello World
#'
#' @param other an alternative string to print
#'
#' @return NULL
#'
#' @export
helloWorld <- function(other=NULL) {
    if (is.null(other)) {
        print("Hello World")
    } else {
        print(other)
    }
}
