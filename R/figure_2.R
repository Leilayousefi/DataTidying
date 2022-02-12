#' @title Figure 2 of the regreg RAP report.
#'
#' @description The function returns the time series ggplot2 object geom_line.
#'
#' @param x Input object of \code{year_phase_data} class.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#'
#' library(regregrap)
#' report_data <- phase_date_data(regreg)
#' figure_2(report_data)
#'
#' @export
#'

figure_2 <- function(x) {

  x$df$id <- as.numeric(row.names(x$df))
  x$df

  p <- ggplot2::ggplot(x$df, ggplot2::aes(x = date, y = rev(id))) +
    ggplot2::geom_line() +
    ggplot2::xlab("Date published") +
    ggplot2::ylab("Total registers published")

  return(p)

}
