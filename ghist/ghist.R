#' Create a quick histogram plot in ggplot.
#'
#' This function creates a histogram plot using ggplot for a given vector.
#'
#' @param x A numeric vector to be plotted.
#' @param color The color of the bars in the histogram.
#' @param fill The fill color of the bars in the histogram.
#' @param alpha The transparency of the bars in the histogram.
#' @param binwidth The width of the bins in the histogram.
#' @param title The title of the plot.
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @return A ggplot object representing the histogram plot.
#' @examples
#' # Create a histogram plot of a random numeric vector
#' x <- rnorm(100)
#' ghist(x)
ghist <- function(x, color = "black", fill = "skyblue", alpha = 0.5,
                  binwidth = NULL, title = NULL, xlab = "Value", ylab = "Frequency") {
  require(ggplot2)

  # Create histogram plot
  p <- ggplot(data.frame(x), aes(x)) +
    geom_histogram(color = color, fill = fill, alpha = alpha, binwidth = binwidth) +
    labs(title = title, x = xlab, y = ylab)

  return(p)
}
