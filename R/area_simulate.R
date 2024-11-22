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



#' sim_area_single_concept_with_risk
#'
#' Low-level simulation function for area-phase dynamics, available for users
#' who want to compose simulations out of the single steps. Regular users are
#' recommended to use the function \code{\link{simulate_single_concept}}.
#'
#' @param state_vars list of state variable(s) (vectors), all initialized
#'
#' @param parms list of parameter(s) (vectors)
#'
#' @param event_times vector of integers specifying the points in time when
#'   damage events can happen. Usually all numbers between (and including) zero
#'   and the endpoint of the simulation (i.e. \code{c(0, time_span)}).
#'
#' @param time_span simulation time span, integer, in the chosen time unit
#'   (typically years)
#'
#' @param time_frac integer >= 1, defines the time step to be used for numerical
#'   integration (time step = 1 / time_frac), i.e. one time unit will be split
#'   into time_frac substeps. Too small values of \code{time_frac} may cause the
#'   model showing chaotic dynamics, too large values increase computation time
#'   without meaningful gains in precision. The default value of 4 is mostly a
#'   good choice; in doubt, increase \code{time_frac} until the results do not
#'   change meaningfully anymore (risk events should be turned off during that
#'   procedure).
#'
#' @param integ_method integration method, passed to \code{\link[deSolve]{ode}}
#'
#' @return an object of class \code{deSolve}
#'
#' @export
#'
#' @examples
#'   # Work with the example data pine_thinning_from_above_1
#'   # Initialize state variables (areas per stand development phase)
#'   state_vars <- setup_statevars(pine_thinning_from_above_1,
#'                                 c(1000, 0, 0, 0, 0, 0))
#'
#'   # Set time frame
#'   time_span          <- 200
#'
#'   # Initialize parameters
#'   parms     <- setup_parms(pine_thinning_from_above_1)
#'
#'   # Build risk matrix and add it to parms
#'   parms$risk_mat <- setup_risk_events(
#'     time_span, avg_event_strength = 1, parms$risk
#'   )
#'
#'   # Simulate
#'   sim_area_single_concept_with_risk(
#'     state_vars,
#'     parms       = parms,
#'     event_times = c(0:time_span),
#'     time_span   = time_span
#'   )
#'
sim_area_single_concept_with_risk <- function(state_vars,
                                              parms,
                                              event_times,
                                              time_span = 100L,
                                              time_frac =   4L,
                                              integ_method = "lsoda") {

  if (time_frac != round(time_frac))
    stop("time_frac is not integer")

  if (time_frac < 1)
    stop("time_frac must be at least equal to 1")

  if (time_span != round(time_span))
    stop("time_span is not integer")


  # setup simulation times
  times <- seq(0, time_span, by = 1 / time_frac)

  # conveniently define the "preceding area index"
  n         <- length(parms$dwell_time) # number of (sub-) phases
  i_act     <- 1:n
  i_prec    <- 1:n - 1
  i_prec[1] <- n

  # actual simulation
  deSolve::ode(
    y      = state_vars,
    times  = times,
    func   = single_concept_area_dynamics,
    parms  = parms,
    method = integ_method,
    event  = list(func = apply_risk_events, time = event_times),
    n_phases = n,
    i_act    = i_act,
    i_prec   = i_prec
  )
}













