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




#' State Variable Setup and Initialization

#' Given a \code{c4c_concept} concept definition, and a matching vector of
#' initial areas the state variables required for simulating the concept are set
#' up and initialized.
#'
#' The state variables to be created are the areas attributed to the single
#' stand development phases defined in the concept definition of interest. More
#' precisely, each subphase has an area which is a state variable. When
#' initializing, the intial areas can be given for each phase in
#' \code{init_areas}, or for each subphase. In the former case the initial phase
#' areas are equally divided among the respective subphases.\cr In order to
#' allow post-hoc reconstruction of the area flows, the function also creates
#' the cumulative inflows and outflows to each subphase area as state variables
#' and initializes them with 0.
#'
#' @param concept_def The concept definition of interest as a \code{c4c_object}
#'
#' @param init_areas A vector providing the initial areas for the stand
#'   development (sub) phases defined in \code{concept_def} in the same order.
#'   In case \code{init_areas} relates to the phases, \code{detailed} must be
#'   \code{FALSE}. If it relates directly to the subphases, \code{detailed} must
#'   be \code{TRUE}. In the former case, the areas given for a phase are equally
#'   divided among its subphases. The areas must be given in the same area unit
#'   as named in \code{concept_def}, usually ha.
#'
#' @param detailed Logical, \code{FALSE} (default) indicates that
#'   \code{init_areas} is given for each stand development phase (and will be
#'   equally distributed among the subphases). \code{TRUE} indicates that
#'   \code{init_areas} is directly given for each subphase.
#'
#' @return A vector which is actually a sequence of three different blocks. This
#'   format is required for simulations with \code{\link[deSolve:ode]{ode}}.
#'   Each block has as many elements as the total number of subpbases defined in
#'   \code{concept_def}. Each element refers to each subphase in the order of
#'   the phase sequence. The first block, contains the initial areas attributed
#'   to all subphases in the order of the phase sequence. The second and the
#'   third block will track the cumulative in- and outflows of each area during
#'   the simulation. They are initialized with 0.
#'
#' @export
#'
#' @examples
#'   # Initialize with phase wise initial areas
#'   init_areas <- c(1000, 400, 250, 125, 125, 100)
#'   state_vars <- pine_thinning_from_above_1 |> setup_statevars(init_areas)
#'   state_vars
#'
#'   # Initialize with subphase wise initial areas
#'   # Assume, we are afforesting 1000 ha, so all area has to be initially in
#'   # the first subphase of the first stand development phase
#'   n_sub <- sum(pine_thinning_from_above_1$growth_and_yield$n_subphases)
#'   init_areas    <- rep(0, n_sub)
#'   init_areas[1] <- 1000
#'   state_vars    <- setup_statevars(pine_thinning_from_above_1,
#'                                    init_areas,
#'                                    detailed = TRUE)
#'   state_vars
#'
setup_statevars <- function(concept_def, init_areas, detailed = FALSE) {

  if(!is_c4c_concept(concept_def)) {
    stop("`concept_def` is not an c4c_concept object.")
  }

  if(!detailed & (length(init_areas) != nrow(concept_def$growth_and_yield))) {
    stop(
      paste(
        "The length of `init_areas` does not match the number of phases",
        "provided in `concept_def`. Also check setting of `detailed`."
      )
    )
  }

  n_sub <- concept_def$growth_and_yield$n_subphases

  if(detailed & (length(init_areas) != sum(n_sub))) {
    stop(
      "The length of `init_areas` does not match the number of subphases",
      "provided in `concept_def`. Also check setting of `detailed`."
    )
  }

  # actual initialization
  if(!detailed) {
    area        <- rep(init_areas / n_sub, times = n_sub)
  } else {
    area        <- init_areas
  }

  area_cum_in   <- rep(0,  sum(concept_def$growth_and_yield$n_subphases))
  area_cum_loss <- rep(0,  sum(concept_def$growth_and_yield$n_subphases))

  c(area, area_cum_in, area_cum_loss)
}



#' Parameter Setup
#'
#' Given a \code{c4c_concept} concept definition, a list of parameter elements
#' is handed back. This information is required for simulations and subsequent
#' evaluations.
#'
#' The element \code{risk} as part of the output describe as 'normal' risk as
#' assumed for the silvicultural concept defined in \code{concept_def}. This can
#' be adjusted with the parameter \code{avg_event_strength} of the function
#' \code{\link{setup_risk_events}}, which has to be called in any case after the
#' parameter setup.
#'
#' @param concept_def Concept definition as a \code{c4c_concept} object
#'
#' @return A list with three elements. The first, \code{dwell_time}, is a vector
#'   of dwell times for each subphase area, i.e. it indicates the average time a
#'   unit area is dwelling in this subphase (assuming an exponential
#'   distribution over time). The second element, \code{risk}, is a vector of
#'   the same length and order. It represents, for each subphase area, the
#'   average relative loss rate per year. It is derived from the cumulative
#'   survival probabilities (\code{survival_com}) given in the data frame
#'   \code{growth_and_yield} which is part of the concept definition
#'   (\code{concept_def}). The third element, \code{phase_indexes}, is a tibble
#'   which contains, for each stand development phase in \code{concept_def}, a
#'   vector of indexes which can be used to easier access the phase wise
#'   information in the different kinds of simulation outputs.
#'
#' @export
#'
#' @examples
#'   parms <- pine_thinning_from_above_1 |> setup_parms()
#'   parms
#'
setup_parms <- function(concept_def) {

  if(!is_c4c_concept(concept_def)) {
    stop(
      stop("`concept_def` is not an c4c_concept object.")
    )
  }

  # calculate the dwell times for each sub-stock of the phase areas
  dwell_time <- concept_def$growth_and_yield$duration /
    concept_def$growth_and_yield$n_subphases
  dwell_time <- rep(
    dwell_time, times = concept_def$growth_and_yield$n_subphases
  )


  # calculate annual loss rates for each sub-stock from the given cumulative
  # survival rates
  # risk_normal is an annual relative loss rate
  dat_ <- concept_def$growth_and_yield |>
    dplyr::mutate(t_1 = cumsum(dplyr::lag(.data$duration, default = 0)),
                  t_2 = cumsum(.data$duration),
                  y_1 = dplyr::lag(.data$survival_cum, default = 1),
                  exp_dec_rate = exp_decay_rate(
                    .data$t_1, .data$t_2, .data$y_1, .data$survival_cum
                  ),
                  risk_normal = 1 - exp(-.data$exp_dec_rate)
    )

  risk_normal <- rep(dat_$risk_normal, times = dat_$n_subphases)
  # risk_normal[1] <- 0


  # initiate the indexes
  # required because deSolve::ode allows only a vector of state variables, not
  # a more structured data structure
  phase_indexes <- concept_def$growth_and_yield |>
    dplyr::mutate(i_end = cumsum(.data$n_subphases),
                  i_start = 1 + dplyr::lag(.data$i_end, default = 0),
                  i = purrr::map2(.data$i_start, .data$i_end, .f = ~ .x:.y)) |>
    dplyr::select(.data$phase_no, .data$phase_name, .data$i)

  list(
    dwell_time    = dwell_time,
    risk          = risk_normal,
    phase_indexes = phase_indexes
  )
}






