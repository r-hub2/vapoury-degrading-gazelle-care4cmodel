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




#' Aggregate Raw Area Simulation Results
#'
#' Internal. Generates the 'area' component of the output of
#' \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param sim_areas_raw Raw area simulation results as obtained from
#'   \code{link{sim_area_single_concept_with_risk}}
#'
#' @param concept_def Concept definition matching \code{sim_areas_raw}, a
#'   \code{c4c_concept} object
#'
#' @return A matrix with named columns, where each row represents an integer
#'   time step, and all columns are the areas attributed to each phase as
#'   defined in \code{concept_def}.
#'
#' @noRd
#'
aggregate_sim_areas <- function(sim_areas_raw, concept_def) {

  # Keep only integer times
  agg_dat <- sim_areas_raw[(sim_areas_raw[, 1] %% 1) == 0, ]
  time    <- agg_dat[, 1]

  # Horizontally sum up areas of the phases
  # column index offsets are chosen dependent from type
  n_sub            <- concept_def$growth_and_yield$n_subphases
  horiz_index_end  <- cumsum(n_sub) + 1
  horiz_index_strt <- horiz_index_end - n_sub + 1

  agg_dat <- mapply(
    FUN = function(a, b, mat) rowSums(mat[, a:b, drop = FALSE]),
    a   = horiz_index_strt,
    b   = horiz_index_end,
    MoreArgs = list(mat = agg_dat)
  )

  colnames(agg_dat) <- concept_def$growth_and_yield$phase_name
  cbind(time, agg_dat)
}





#' Calculate Regular Annual Area Inflows to Phases
#'
#' Internal. Generates the 'area_inflows_regular' component of the output of
#' \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param sim_areas_raw Raw area simulation results as obtained from
#'   \code{link{sim_area_single_concept_with_risk}}
#'
#' @param concept_def Concept definition matching \code{sim_areas_raw}, a
#'   \code{c4c_concept} object
#'
#' @return A matrix with named columns, where each row represents an integer
#'   time step, and all columns are the areas attributed to each phase as
#'   defined in \code{concept_def}. The entries (except time) represent
#'   the area inflows into each stand development phase as caused by regular
#'   development, not by damage events. An entry at a given point in time
#'   represents the inflow between (and up to) this and the previous point in
#'   time. Therefore, the entries at time 0 are NA.
#'
#' @noRd
#'
aggregate_sim_areaflows_regular <- function(sim_areas_raw, concept_def) {

  # Keep only integer times
  agg_dat <- sim_areas_raw[(sim_areas_raw[, 1] %% 1) == 0, ]
  time    <- agg_dat[, 1]

  # column index offsets are chosen dependent from type
  n_sub            <- concept_def$growth_and_yield$n_subphases
  horiz_index_end  <- sum(n_sub) + cumsum(n_sub) + 1
  horiz_index_strt <- horiz_index_end - n_sub + 1

  # Inflows to each aggregated phase (means we require the first index
  # per aggregated phase only)
  agg_dat <- agg_dat[2:nrow(agg_dat), horiz_index_strt] -
               agg_dat[(1:nrow(agg_dat) - 1), horiz_index_strt]

  # First line values (t = 0) are NA
  agg_dat <- rbind(matrix(NA, ncol = ncol(agg_dat), nrow = 1), agg_dat)

  colnames(agg_dat) <- concept_def$growth_and_yield$phase_name

  cbind(time, agg_dat)
}



#' Calculate Annual Phase Area Outflows Triggered by Events
#'
#' Internal. Generates the 'area_outflows_events' component of the output of
#' \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param sim_areas_raw Raw area simulation results as obtained from
#'   \code{link{sim_area_single_concept_with_risk}}
#'
#' @param concept_def Concept definition matching \code{sim_areas_raw}, a
#'   \code{c4c_concept} object
#'
#' @return A matrix with named columns, where each row represents an integer
#'   time step, and all columns are the areas attributed to each phase as
#'   defined in \code{concept_def}. The entries represent area outflows of each
#'   stand development phase as caused by damage events. An entry at a given
#'   point in time represents the inflow between (and up to) this and the
#'   previous point in time. Therefore, the entries at time 0 are NA.
#'
#' @noRd
#'
aggregate_sim_areaflows_events <- function(sim_areas_raw, concept_def) {

  # Keep only integer times
  agg_dat <- sim_areas_raw[(sim_areas_raw[, 1] %% 1) == 0, ]
  time    <- agg_dat[, 1]

  # column index offsets are chosen dependent from type
  n_sub            <- concept_def$growth_and_yield$n_subphases
  horiz_index_end  <- 2 * sum(n_sub) + cumsum(n_sub) + 1
  horiz_index_strt <- horiz_index_end - n_sub + 1

  # First, we require horizontal phase-wise summations of the cumulated areas
  agg_dat <- mapply(
    FUN = function(a, b, mat) rowSums(mat[, a:b, drop = FALSE]),
    a   = horiz_index_strt,
    b   = horiz_index_end,
    MoreArgs = list(mat = agg_dat)
  )

  # Second, vertical substraction in order to obtain the flows
  agg_dat <- agg_dat[2:nrow(agg_dat), ] -
    agg_dat[(1:nrow(agg_dat) - 1), ]

  # First line values (t = 0) are NA
  agg_dat <- rbind(matrix(NA, ncol = ncol(agg_dat), nrow = 1), agg_dat)

  colnames(agg_dat) <- concept_def$growth_and_yield$phase_name

  cbind(time, agg_dat)
}



#' Comprehensively Aggregate Raw Simulation Results
#'
#' Aggregate and prepare raw simulation output as obtained from
#' \code{\link{sim_area_single_concept_with_risk}} in a way that makes them
#' readable and appropriate for further processing.
#'
#' @param sim_areas_raw Raw simulation results as obtained from
#'   \code{\link{sim_area_single_concept_with_risk}}
#'
#' @param concept_def The concept definition (a \code{c4c_concept} object) used
#'   for the simulation which generated \code{sim_areas_raw}
#'
#' @return A list of three matrices with named columns. Each row of these
#'   matrices represents a point in simulation time, but only integer times. The
#'   time distance from one row to the next one is one time unit, typically one
#'   year. The first column of the matrices is time, the other columns represent
#'   the stand development phases as defined in \code{concept_def}. The three
#'   list elements (matrices) are:\cr
#'   \itemize{
#'     \item{*areas*: Contains the simulated areas of each stand development
#'       phase in the area units defined in \code{concept_def}, usually ha.}
#'     \item{*area_inflows_regular*: The area inflows into each stand
#'       development phase as caused by regular development, not by damage
#'       events. An entry at a given point in time represents the inflow between
#'       (and up to) this and the previous point in time. Therefore, the entries
#'       at time 0 are NA.}
#'     \item{*area_outflows_events*: Area outflows of each stand development
#'       phase as caused by damage events. An entry at a given point in time
#'       represents the inflow between (and up to) this and the previous point
#'       in time. Therefore, the entries at time 0 are NA.}
#'   }
#'
#' @export
#'
#' @examples
#'   # Make a simulation
#'   state_vars <- setup_statevars(
#'     pine_thinning_from_above_1, c(1000, 0, 0, 0, 0, 0)
#'   )
#'   time_span  <- 50
#'   parms      <- setup_parms(pine_thinning_from_above_1)
#'   parms$risk_mat <- setup_risk_events(
#'     time_span, avg_event_strength = 1, parms$risk
#'   )
#'
#'   # Simulate
#'   sim_rslt_raw <- sim_area_single_concept_with_risk(
#'     state_vars,
#'     parms       = parms,
#'     event_times = c(0:time_span),
#'     time_span   = time_span
#'   )
#'
#'   aggregate_raw_sim_rslt(sim_rslt_raw, pine_thinning_from_above_1)
#'
#'
aggregate_raw_sim_rslt <- function(sim_areas_raw, concept_def) {

  a <- aggregate_sim_areas(sim_areas_raw, concept_def)
  b <- aggregate_sim_areaflows_regular(sim_areas_raw, concept_def)
  c <- aggregate_sim_areaflows_events(sim_areas_raw, concept_def)

  list(areas = a, area_inflows_regular = b, area_outflows_events = c)
}














