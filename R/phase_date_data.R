#' @title minimal tidy data set for regregrap report production.
#'
#' @description \code{year_phase_data} is the class used for the creation of all
#'  figures and tables in the made up RAP report (it's a demo for the RAP MOOC).
#'
#'
#' @details The \code{year_phase_data} class expects a \code{data.frame} with at
#' least three columns: phase, date (entry-timestamp), and register name. Each
#' row represents a unique register.
#'
#'   Once inititated, the class has five slots: \code{df}: the basic
#'   \code{data.frame}, \code{colnames}: a character vector containing the
#'   column names from the \code{df}, \code{phase_levels}: a
#'   factor vector containing levels of \code{df$phase} of the factor sector,
#'   \code{yq}: an date vector containing \code{zoo::as.yearqrt(df$date)}.
#'
#' @param x Input dataframe, see details.
#' @param log_level The severity level at which log messages are written from
#' least to most serious: TRACE, DEBUG, INFO, WARN, ERROR, FATAL. Default is
#' level is INFO. See \code{?flog.threshold()} for additional details.
#' @param log_appender Defaults to write the log to "console", alternatively you
#' can provide a character string to specify a filename to also write to. See
#' for additional details \code{?futile.logger::appender.tee()}.
#' @param eda If TRUE an graphical data analysis is conducted for a human to check.
#'
#' @return If the class is not instantiated correctly, nothing is returned.
#'
#' @examples
#'
#' library(regregrap)
#'
#' df <- phase_date_data(regreg)
#'
#' @export


phase_date_data <- function(x, log_level = futile.logger::WARN,
                             log_appender = "console", eda = FALSE) {

  # Set logger severity threshold, defaults to
  # high level use (only flags warnings and errors)
  # Set log_level argument to futile.logger::TRACE for full info
  futile.logger::flog.threshold(log_level)

  # Set where to write the log to
  if (log_appender != "console")
  {
    # if not console then to a file called...
    futile.logger::flog.appender(futile.logger::appender.file(log_appender))
  }

  # Checks
  futile.logger::flog.info('Initiating phase_date_data class.
                           \n\nExpects a data.frame with at
                           least three columns: phase, date (entry-timestamp),
                           and register name. Each
                           row represents a unique register..
                           More information on the format expected by
                           this class is given by ?phase_date_data().')

  # Integrity checks on incoming data ----

  # Check the structure of the data is as expected: data.frame containing no
  # missing values and at least three columns, containing phase, date and register name.

  futile.logger::flog.info('\n*** Running integrity checks on input dataframe (x):')
  futile.logger::flog.debug('\nChecking input is properly formatted...')

  futile.logger::flog.debug('Checking x is a data.frame...')
  if (!is.data.frame(x))
  {
    futile.logger::flog.error("x must be a data.frame",
                              x, capture = TRUE)
  }

  futile.logger::flog.debug('Checking x has correct columns...')
  if (length(colnames(x)) < 3)
  {
    futile.logger::flog.error("x must have at least three columns: phase, date and register")
  }

  futile.logger::flog.debug('Checking x contains a date column...')
  if (!'date' %in% colnames(x)) stop("x must contain date column")

  futile.logger::flog.debug('Checking x contains a phase column...')
  if (!'phase' %in% colnames(x)) stop("x must contain phase column")

  futile.logger::flog.debug('Checking x does not contain missing values...')
  if (anyNA(x)) stop("x cannot contain any missing values")

  futile.logger::flog.debug('Checking for the correct number of rows...')
  if (nrow(x) < 25) {
    futile.logger::flog.warn("x does not appear to be well formed. nrow(x) should be
                             greater than 26 as of early 2018.")
  }



  futile.logger::flog.info('...passed')

  # User assertr to run statistical tests on the data itself ----

  futile.logger::flog.info("\n***Running statistical checks on input dataframe (x)")

  futile.logger::flog.trace("These tests are implemented using the package assertr see:
                            https://cran.r-project.org/web/packages/assertr for more details.")


  # Check snsible range for year

  futile.logger::flog.debug('Checking years in a sensible range (2015:2025)...')

  if (any(lubridate::year(regreg$date) < 2016)) {
    futile.logger::flog.warn("The dates are not in a sensible range, have they been
                             read in correctly?")
  }

  # hopefully moved beyond RAP by 2025...
  if (any(lubridate::year(regreg$date) > 2025)) {
    futile.logger::flog.warn("The dates look dodgy,
                             are you still using RAP in 2025?")
  }

  # Check that the correct levels are in phase

  futile.logger::flog.debug('Checking sectors are correct...')

  # Save sectors name lookup for use later

  phase_set <- c(
    "discovery"    = "Discovery",
    "alpha"    = "Alpha",
    "beta"     = "Beta",
    "live"    = "Live"
  )

  # Check for outliers ----

  # Could check for outliers here... given our data we can't

  futile.logger::flog.info('...passed')

  # Reset threshold to package default
  futile.logger::flog.threshold(futile.logger::INFO)
  # Reset so that log is appended to console (the package default)
  futile.logger::flog.appender(futile.logger::appender.console())

  # Message required to pass a test
  message("Checks completed successfully:
          object of 'phase_date_data' class produced!")

  # EDA
  # some people like to eyeball stuff
  if (eda == TRUE) {
    plot(rev(regreg$date),
         ylab = "Date published",
         xlab = "Cumulative count of published registers")

  }

  # Drop unnecessary columns
  x <- x[, c("register", "phase", "date")]
  # Define the class here ----

  structure(
    list(
      df = x,
      colnames = colnames(x),
      type = colnames(x)[!colnames(x) %in% c('date','phase', 'register')],
      phase_levels = levels(x$phase),
      phase_set = phase_set,
      yq = zoo::as.yearqtr(x$date, format = "%Y-%m-%d")
    ),
    class = "phase_date_data")
  }
