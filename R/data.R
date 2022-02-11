#' GOV.UK register register.
#'
#' A dataset containing the characteristics of all
#'  the published GOV.UK registers.
#'
#' @format A data frame
#' \describe{
#'   \item{date}{date register was published}
#'   \item{register}{name of register}
#'   \item{text}{description of register contents}
#'   \item{registry}{responsible body of register}
#'   \item{phase}{The stage of development a register is in.
#'   There are 4 phases - discovery, alpha, beta and live.}
#'   \item{copyright}{The copyright and licensing terms which
#'    may apply to the data held in a register.}
#'   \item{fields}{variables contained in the register}
#'   ...
#' }
#' @source \url{https://register.register.gov.uk/records}
"regreg"
