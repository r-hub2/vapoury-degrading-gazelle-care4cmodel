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



#' Equations for a Single Concept Area Dynamics Model
#'
#' Outdated low-level function. Use
#' \code{\link{sim_area_single_concept_with_risk}} instead.
#'
#' @param t time parameter required by \code{deSolve::ode} but not inside this
#'   function
#'
#' @param y *** Revise *** vector of initialized areas
#'
#' @param parms parameters (list)
#'
#' @param i_prec index vector of the same length as \code{area}, but pointing to
#'   the previous element in the area vector. Passed from outside in order to
#'   avoid recalculation in each simulation step
#'
#' @return Returns the change rates of the state variables
#'
#' @noRd
#'
single_concept_area_dynamics <- function(t, y, parms, n_phases,
                                         i_act, i_prec) {

  # Initialize area change vector
  # 1:n_phases area change through in- and outgrowth
  # (n_phases + 1):(2 * n_phases) ingrowth only
  # (2 * nphases + 1):(3 n_phases) area losses (risk events)
  d_area <- rep(0, 3 * n_phases)

  # Outgrowth from each phase (incl. final harvesst)
  area_out <- y[i_act] / parms$dwell_time
  # Incoming is for both, actual and cumulated areas
  d_area[1:(2 * n_phases)]   <- rep(area_out[i_prec], 2)
  # Outgoing only substracted from actual areas
  d_area[i_act] <- d_area[i_act] - area_out

  list(d_area)
}


