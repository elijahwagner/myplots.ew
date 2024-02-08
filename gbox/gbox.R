#' Create a box plot with optional grouping
#'
#' This function creates a box plot from a single numeric vector or
#' from a combination of one numeric and one categorical vector.
#'
#' @param x A numeric vector or a data frame with one numeric and one categorical variable.
#' @param y (Optional) A numeric vector if x is a data frame.
#' @param horizontal Logical. Should the box plot be horizontal? Default is FALSE.
#' @param ... Additional arguments passed to geom_boxplot().
#' @return A ggplot box plot object.
#' @examples
#' # Create a box plot from a numeric vector
#' gboxplot(mtcars$mpg)
#' # Create a side-by-side box plot from a numeric and a categorical vector
#' gboxplot(mtcars, cyl)
#' @export
gboxplot <- function(x, y = NULL, horizontal = FALSE, ...) {
  if (is.data.frame(x)) {
    if (is.null(y)) {
      stop("Please provide a second argument for categorical variable.")
    }
    if (horizontal) {
      ggplot(x, aes(x = factor(y), y = x[[1]])) +
        geom_boxplot() +
        coord_flip() +
        labs(x = "", y = deparse(substitute(x))) +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      ggplot(x, aes(x = factor(y), y = x[[1]])) +
        geom_boxplot() +
        labs(x = deparse(substitute(y)), y = deparse(substitute(x))) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  } else {
    ggplot() +
      geom_boxplot(aes(y = x), horizontal = horizontal) +
      labs(y = deparse(substitute(x))) +
      theme_minimal()
  }
}