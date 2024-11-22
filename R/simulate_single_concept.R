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




#' Run a Simulation for a Single Silvicultural Concept
#'
#' Top level function for running a simulation and obtaining all fundamental
#' results, i.e. the simulated area dynamics and all growth and yield related
#' outcomes.
#'
#' The output of this function is an object of class \code{c4c_base_result}.
#' There is no other way to generate such an object, therefore there is neither
#' a constructor nor a validator available to the user.
#'
#' @param concept_def Silvicultural Concept definition to be used in the
#'   simulation; a \code{c4c_concept} object
#'
#' @param init_areas The initial areas for each stand development phase defined
#'   in \code{concept_def} (if \code{detailed_init == FALSE}, default) or the
#'   initial areas for each stand development subphase as defined in
#'   \code{concept_def} (if \code{detailed_init == FALSE})
#'
#' @param time_span Time span to be covered by the simulation
#'
#' @param risk_level Risk level relative to the standard risk level as defined
#'   in \code{concept_def}. The default is 1 which means, the standard risk
#'   level will be applied. If \code{risk_level == 0}, no damaging events will
#'   happen; e.g. \code{risk_level == 2} will increase damage probabilities as
#'   if the standard level risk events would occur two times.
#'
#' @param detailed_init Logical; is \code{init_areas} provided for each stand
#'   development phase (default, FALSE), or for each subphase (TRUE)?
#'
#' @param detailed_out Logical; should the output also include growth and yield
#'   pre-evaluation results (which are a very detailed interim evaluation output
#'   that is usally only required for internal efficiency)? The default is
#'   FALSE.
#'
#' @param ... Additional arguments to
#'   \code{\link{sim_area_single_concept_with_risk}}
#'
#' @return An object of class \code{c4c_base_result} which is actually a named
#'   list containing all information that was used to define and set up a
#'   simulation, as well as all fundamental simulation results, i.e. the
#'   simulated area dynamics, and all growth and yield related results.
#'
#' @export
#'
#' @examples
#'   simulate_single_concept(
#'     pine_thinning_from_above_1,
#'     init_areas = c(1000, 0, 0, 0, 0, 0),
#'     time_span  = 200,
#'     risk_level = 3
#'   )
#'
simulate_single_concept <- function(concept_def,
                                    init_areas,
                                    time_span,
                                    risk_level = 1,
                                    detailed_init = FALSE,
                                    detailed_out = FALSE,
                                    ...) {

  if(!is_c4c_concept(concept_def)) {
    stop("`concept_def` is not an c4c_concept object.")
  }

  # Does the length of init_areas match with the contents of concept_def?
  if(!detailed_init) {
    if(length(init_areas) != nrow(concept_def$growth_and_yield)) {
      stop(
        paste0(
          "Length of init_areas must be the same as the number of phases ",
          "in the concept definition when detailed_init == FALSE."
        )
      )
    }
  } else {
    if(length(init_areas) != sum(concept_def$growth_and_yield$n_subphases)) {
      stop(
        paste0(
          "Length of init_areas must be the same as the total number of ",
          "subphases in the concept definition when detailed_init == TRUE."
        )
      )
    }
  }


  # Initialize
  state_vars <- setup_statevars(concept_def, init_areas, detailed_init)
  parms      <- setup_parms(concept_def)

  # -- I don't really like to push the risk matrix into parms, but for operating
  #    deSolve::ode() this is the least clumsy way
  parms$risk_mat <- setup_risk_events(time_span, risk_level, parms$risk)

  # Simulate area dynamics
  sim_areas_raw <- sim_area_single_concept_with_risk(
    state_vars,
    parms       = parms,
    event_times = 0:time_span,
    time_span   = time_span,
    ...
  )

  # Aggregate simulated areas
  sim_areas_agg <- aggregate_raw_sim_rslt(sim_areas_raw, concept_def)

  # Growth and yield evaluation
  growth_and_yield <- growth_and_yield_evaluation(
    sim_areas_agg, concept_def, detailed_out
  )

  x <- list(
    concept       = concept_def,
    time_span     = time_span,
    detailed_init = detailed_init,
    detailed_out  = detailed_out,
    init_areas    = init_areas,
    risk_level    = risk_level,
    parameters    = parms,
    sim_areas     = sim_areas_agg,
    sim_growth_and_yield = growth_and_yield
  )

  new_c4c_base_result(x)
}

