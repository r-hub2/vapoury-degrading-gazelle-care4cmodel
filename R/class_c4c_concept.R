#  R package care4cmodel - Carbon-Related Assessment of Silvicultural Concepts
#  Copyright (C) 2023  Peter Biber
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.





# Implementation of the s3 class c4c_concept
# Which is a definition of a silvicultural concept that can be simulated


#' Constructor for a c4c_concept Object
#'
#' @param x a list object
#'
#' @return Returns an object of class \code{c4c_concept}
#'
#' @export
#'
#' @examples
#'   # remove the c4c_class attribute for the example's sake
#'   x <- unclass(pine_thinning_from_above_1)
#'
#'   x <- new_c4c_concept(x)
#'
new_c4c_concept <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "c4c_concept")
}



#' Check if an Object is of Class c4c_concept
#'
#' @param x object to check
#'
#' @return \code{TRUE}, if \code{x} has class c4c_concept, \code{FALSE} if not
#'
#' @export
#'
#' @examples
#'   data(pine_thinning_from_above_1)
#'   x <- unclass(pine_thinning_from_above_1)
#'
#'   is_c4c_concept(pine_thinning_from_above_1)
#'   is_c4c_concept(x)
#'
is_c4c_concept <- function(x) {
  rslt <- FALSE
  if(inherits(x, "c4c_concept")) {
    rslt <- TRUE
  }
  rslt
}




#' Validator for a c4c_concept Object
#'
#' @param x an object of class c4c_concept to be validated
#'
#' @return Returns the input object if it passes validation, stops with an
#'   error otherwise
#'
#' @export
#'
#' @examples
#'   pine_thinning_from_above_1 |> validate_c4c_concept()
#'   pine_no_thinning_and_clearcut_1 |> validate_c4c_concept()
#'
validate_c4c_concept <- function(x) {

  # Big checks

  if(!is_c4c_concept(x)) {
    stop("Not a c4c_concept object.")
  }

  if(!is.list(x)) {
    stop("Object x must be derived from a list.")
  }

  # All required elements present?
  req_elements <- c("concept_name", "units", "growth_and_yield")
  if(!all(req_elements %in% names(x))) {
    stop(
      paste(
        "Element(s)",
        paste(req_elements, collapse = ", "),
        "must all be present in an c4c_concept object."
      )
    )
  }

  # Non-allowed elements present?
  if(!all(names(x) %in% req_elements)) {
    stop("Non-allowed elements in object.")
  }


  # Detail checks

  # Check element concept_name
  if(!is.character(x$concept_name) |
     (length(x$concept_name) != 1) |
     is.na(length(x$concept_name))) {
    stop(
      "Element `concept_name` must be a non-empty character string of length 1"
    )
  }

  # Check element units
  # Currently, there is only one unit definition, and it is enforced
  enforced_unit_defs <- c(time = "year", area = "ha", volume = "m3", mass = "t",
                          stem_diameter = "cm")
  if(!identical(
    x$units[c("time", "area", "volume", "mass", "stem_diameter")],
    c(time = "year", area = "ha", volume = "m3", mass = "t",
      stem_diameter = "cm"))
  ) {
    stop("Non-supported unit definitions.")
  }


  # Check element growth_and_yield
  # - Is it a data.frame?
  if(!is.data.frame(x$growth_and_yield)) {
    stop("Element `growth_and_yield` must be a data frame.")
  }

  # - does if have two rows at least?
  if(nrow(x$growth_and_yield) < 2) {
    stop("Element `growth_and_yield` must have at least two rows.")
  }

  # - All required columns there?
  req_cols <- c("phase_no", "phase_name", "duration", "n_subphases",
                "vol_standing", "vol_remove", "vol_mort",
                "n_standing", "n_remove",
                "dbh_standing", "dbh_remove",
                "harvest_interval",
                "survival_cum", "vol_increment")
  actual_cols <- names(x$growth_and_yield)

  if(!all(req_cols %in% actual_cols)) {
    miss_cols <- req_cols[!(req_cols %in% actual_cols)]
    stop(
      paste(
        "Column(s)",
        paste(miss_cols, collapse = ", "),
        "missing in element `growth_and_yield`."
      )
    )
  }

  # - Are there non-allowed columns?
  if(!all(actual_cols %in% req_cols)) {
    non_allwd_cols <- actual_cols[!(actual_cols %in% req_cols)]
    stop(paste(
      "Columns",
      paste(non_allwd_cols, collapse = ", "),
      "not allowed in element `growth_and_yield`."
      )
    )
  }

  # - Any missing values?
  if(!all(stats::complete.cases(x$growth_and_yield))) {
    stop("No missing values allowed in data frame `growth_and_yield`.")
  }

  # - check column phase_no
  if(any(x$growth_and_yield$phase_no != 1:nrow(x$growth_and_yield))) {
    stop(paste("Column `phase_no` in element `growth_and_yield` must be the",
               "natural row numbers in ascending order.")
    )
  }

  # - check column phase_name
  if(any(duplicated(x$growth_and_yield$phase_no))) {
    stop("Column `phase_name` in element `growth_and_yield` must be unique.")
  }

  # - check volume columns
  # -- negative values
  #    note: the regexp "^vol_[^i]" excludes the column vol_increment from the
  #          test, because the increment is tested separately below
  rslt <- x$growth_and_yield |>
    dplyr::ungroup() |> # for safety's sake
    dplyr::summarise(dplyr::across(dplyr::matches("^vol_[^i]"), ~ any(. < 0))) |>
    t() |>
    as.vector()
  if(any(rslt)) {
    stop(paste(
        "No negative values allowed in the `vol_` columns of element",
        "`growth_and_yield`."
      )
    )
  }

  # - check column duration
  # -- all integer?
  if(any(x$growth_and_yield$duration != trunc(x$growth_and_yield$duration))) {
    stop("Values of `duration` in element `growth_and_yield` must be integers.")
  }

  # -- values >= 1?
  if(any(x$growth_and_yield$duration < 1)) {
    stop("Values of `duration` in element `growth_and_yield` must be >= 1.")
  }

  # - check column n_subphases
  # -- all integer?
  if(any(
    x$growth_and_yield$n_subphases != trunc(x$growth_and_yield$n_subphases))
  ) {
    stop(
      "Values of `n_subphases` in element `growth_and_yield` must be integers."
    )
  }

  # -- not greater than duration (i.e. length of a subphase smaller than one
  #    time unit)
  if(any(
    x$growth_and_yield$n_subphases > x$growth_and_yield$duration
    )
  ) {
    stop(
      paste(
        "Values of `n_subphases` in element `growth_and_yield` must not be",
        "greater than the corresponding values of `duration`."
      )
    )
  }

  # -- dbh_standing must be > 0 if vol_standing > 0.
  if(any(
    (x$growth_and_yield$vol_standing > 0) &
    !(x$growth_and_yield$dbh_standing > 0)
    )
  ) {
    stop(
      paste(
        "Value of `dbh_standing` must be > 0 for `vol_standing` > 0."
      )
    )
  }

  # -- dbh_remove must be > 0 if vol_remove > 0
  if(any(
    (x$growth_and_yield$vol_remove > 0) &
    !(x$growth_and_yield$dbh_remove > 0)
    )
  ) {
    stop(
      paste(
        "Value of `dbh_remove` must be > 0 for `vol_remove` > 0."
      )
    )
  }

  # - check column harvest_interval
  # -- must be >= 0
  if(any(x$growth_and_yield$harvest_interval < 0)) {
    stop(
      "Value of `harvest_interval` must be >= 0."
    )
  }

  # -- harvest_interval must be 0 if vol_remove == 0
  if(any(
    (x$growth_and_yield$vol_remove == 0) &
    !(x$growth_and_yield$harvest_interval == 0)
  )) {
    stop(
      "Value of `harvest_interval` must be 0 when vol_remove == 0."
    )
  }

  # -- harvest_interval must be > 0 if vol_remove > 0
  if(any(
    (x$growth_and_yield$vol_remove > 0) &
    !(x$growth_and_yield$harvest_interval > 0)
  )) {
    stop(
      "Value of `harvest_interval` must be > 0 when vol_remove > 0."
    )
  }

  # -- harvest_interval in a phase must not be greater than the phase duration
  if(any(
    x$growth_and_yield$harvest_interval > x$growth_and_yield$duration
  )) {
    stop(
      "Value of `harvest_interval` cannot be greater than `duration`."
    )
  }

  # - check column survival_cum
  # -- for allowed range
  if(any(
      (0 > x$growth_and_yield$survival_cum) |
      (1 < x$growth_and_yield$survival_cum)
    )
  ) {
    stop(
      paste(
        "Values of `survival_cum` in element `growth_and_yield` must be from",
        "the intveral [0, 1]."
      )
    )
  }

  # -- for decreasing order
  surv_2    <- x$growth_and_yield$survival_cum
  surv_1    <- dplyr::lag(surv_2, default = 1)
  surv_diff <- surv_1 - surv_2
  if(any(surv_diff < 0)) {
    stop(
      paste(
        "Values of `survival cum` in element `growth_and_yield` must be",
        "arranged in decreasing order."
      )
    )
  }

  # - check column vol_increment: Negative values indicate an ill defined
  #   removal volume (possibly also vol_mort)
  index <- x$growth_and_yield$vol_increment < 0
  if(any(index)) {
    stop(
      paste(
        "Negative volume increment in phase(s)",
        paste(x$growth_and_yield$phase_name[index], collapse = ", "),
        ". Check your values of vol_remove and vol_mort.",
        "They are probably too small for covering the reduction of",
        "vol_standing from one phase to the next."
      )
    )
  }

  # return input object
  x
}




#' User-Friendly Construction of a c4c_concept Object
#'
#' For creating a c4c_concept object under normal circumstances, you should not
#' use the constructor \code{\link{new_c4c_concept}} directly, but this
#' function.
#'
#' Special attention needs to be paid to the definition of **vol_remove** in
#' cases when the standing volume decreases from one phase to the next. If the
#' value given for \code{vol_remove} is too low, it will result in a negative
#' volume increment for the respective phase. This will not pass the validation
#' called inside this function.
#'
#' @param growth_and_yield data.frame with at least two rows and the columns
#'   "phase_no", "phase_name", "duration", "n_subphases", "vol_standing",
#'   "vol_remove", "vol_mort", "dbh_standing", "dbh_remove", "n_standing",
#'   "n_remove", "harvest_interval", and "survival_cum".
#'
#' @param concept_name Character, name of the concept defined
#'
#' @return A valid object of class c4c_concept, if it can be constructed from
#'   the input data; stops with an error otherwise. The object is basically a
#'   list. Its most important ingredient is a tibble named
#'   \code{growth_and_yield} which is a honed version of the input
#'   \code{growth_and_yield} to this function. It contains, in addition, phase
#'   wise periodical volume increments per ha (column \code{vol_increment}),
#'   which result from the given information. There is no option for
#'   user-provided volume increments in order to guarantee consistency.
#'
#' @export
#'
#' @examples
#'   # construct dummy example (without real life relevance)
#'   g_and_y <- data.frame(
#'     phase_no         = 1:2,
#'     phase_name       = c("young", "older"),
#'     duration         = c(10, 10),
#'     n_subphases      = c(3, 3),
#'     vol_standing     = c(166, 304),
#'     vol_remove       = c(0, 23.6),
#'     vol_mort         = c(0.01, 0.11),
#'     n_standing       = c(3200, 970),
#'     n_remove         = c(0, 306),
#'     dbh_standing     = c(9.4, 22.3),
#'     dbh_remove       = c(0, 12.3),
#'     harvest_interval = c(0, 5),
#'     survival_cum     = c(0.999, 0.852)
#'   )
#'
#'   dummy_concept <- c4c_concept(g_and_y, "dummy_concept")
#'   dummy_concept
#'
c4c_concept <- function(growth_and_yield, concept_name) {

  x <- list()
  x$concept_name <- concept_name

  # hardwired units
  x$units <- c(time = "year", area = "ha", volume = "m3", mass = "t",
               stem_diameter = "cm")

  # calculate phasewise volume increments
  growth_and_yield <- growth_and_yield |>
    dplyr::mutate(
      vol_std_lead  = dplyr::lead(.data$vol_standing,
                                  default = .data$vol_standing[1]),
      vol_increment =
        (.data$vol_std_lead - .data$vol_standing) / .data$duration +
        .data$vol_remove + .data$vol_mort
    )

  # Remove helper variable(s) and arrange columns
  x$growth_and_yield <- growth_and_yield |>
    dplyr::select("phase_no", "phase_name", "duration", "n_subphases",
                  "vol_standing", "vol_remove", "vol_mort", "n_standing",
                  "n_remove", "dbh_standing", "dbh_remove", "harvest_interval",
                  "survival_cum",
                  "vol_increment")

  new_c4c_concept(x) |>
    validate_c4c_concept()
}





