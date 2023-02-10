#' Strength Training Program
#'
#' A dataset containing strength training program. Strength training program
#'     involves doing two strength training sessions, over 18 weeks (6 phases of 3 weeks each).
#'     Session A involves linear wave-loading pattern starting with 2x12/10/8 reps and reaching 2x8/6/4 reps.
#'     Session B involves constant wave-loading pattern using 2x3/2/1.
#'
#' @format A data frame with 216 rows and 7 variables:
#' \describe{
#'   \item{phase}{Phase index number. Numeric from 1 to 6}
#'   \item{week}{Week index number (within phase). Numeric from 1 to 3}
#'   \item{visit}{Day (total) index number. Numeric from 1 to 36}
#'   \item{session}{Name of the session. Can be "Session A" or "Session B"}
#'   \item{set}{Set index number. Numeric from 1 to 6}
#'   \item{target_reps}{Number of reps being prescribed}
#'   \item{perc_1RM}{Percentage of 1RM to be used}
#' }
"strength_training_program"
