#' Information about the JSSP Instances
#'
#' This dataset provides information about the instances of the Job Shop
#' Scheduling Problem (JSSP) used in our experiments.
#'
#' @docType data
#'
#' @usage data(jsspInstances)
#'
#' @format A data frame with 242 rows and 6 variables:
#' \describe{
#'   \item{inst.name}{the name of the JSSP instance}
#'   \item{inst.jobs}{the number of jobs in the instance, i.e., \code{n}}
#'   \item{inst.machines}{the number of machines in the instance, i.e.,
#'   \code{m}}
#'   \item{inst.opt.bound.lower}{the lower bound for the makespan of the
#'   globally optimal solution}
#'   \item{inst.opt.bound.upper}{the upper bound for the makespan of the
#'   globally optimal solution}
#'   \item{inst.solutions.num}{the number of discovered/known gobally optimal
#'   solutions}
#'}
#'
#' @keywords Job Shop Scheduling, JSSP, instances, results, bounds
#'
#' @references Jelke J. van Hoorn. 2018. The current state of bounds on
#'   benchmark instances of the job-shop scheduling problem. Journal of
#'   Scheduling 21, 1 (February 2018), 127–128.
#'   doi:\href{https://doi.org/10.1007/s10951-017-0547-8}{10.1007/s10951-017-0547-8}
#'
#'   Jelke J. van Hoorn. 2015. Job shop instances and solutions. Retrieved from
#'   \href{http://jobshop.jjvh.nl}{http://jobshop.jjvh.nl}
#'
#' @source \href{http://jobshop.jjvh.nl/}{http://jobshop.jjvh.nl/}
#'
#' @examples
#' data(jsspInstances)
#' print(jsspInstances$inst.name)
"jsspInstances"
