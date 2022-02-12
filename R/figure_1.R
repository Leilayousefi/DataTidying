#' @title plots figure 1.
#'
#' @description \code{year_phase_data} is the class used for the creation of all
#'  figures and tables in the made up RAP report (it's a demo for the RAP MOOC),
#'  including this bar chart.
#'
#' @return Returns a ggplot object.
#'
#' @examples
#'
#' report_data <- regregrap::phase_date_data(regregrap::regreg)
#' regregrap::figure_1(report_data)
#'
#' @export
#' @param x Input phase_date_data class object.
#'
figure_1 <- function(x) {

    out <- tryCatch(
      expr = {


        p <- ggplot2::ggplot(x$df, ggplot2::aes(x = phase)) +
          ggplot2::geom_bar() +
          ggplot2::xlab("Phase") + ggplot2::scale_x_discrete(drop=FALSE)
          # govstyle::theme_gov(base_colour = 'black')

        return(p)

        return(p)

      },
      warning = function() {

        w <- warnings()
        warning('Warning produced running figure_1():', w)

      },
      error = function(e)  {

        stop('Error produced running figure_1():', e)

      },
      finally = {}
    )
  }
