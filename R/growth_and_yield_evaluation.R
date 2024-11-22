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




#' Calculate Volumes and Volume Flows Related to Phase Areas
#'
#' Given aggregated area simulation output and the corresponding concept
#' definition, the function calculates standing volumes, volume increment,
#' removal volumes, and mortality volumes.
#'
#' @param area_agg Aggregated simulated area dynamics, more precisely, the list
#'   element \code{areas} of the output of \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param concept_def Concept definition matching \code{area_agg}, a
#'   \code{c4c_concept} object
#'
#' @param type one of "standing", "removal", "mort", "inc" (default "standing").
#'   These settings allow to choose between calculations of standing volumes,
#'   removal volumes, mortality volumes, and volume increment, respectively. The
#'   increment is calculated here by multiplying the phase wise increments given
#'   in the concept definition with the area. There is also another way to
#'   calculate volume increment on the estate level from the simulated volumes
#'   and volume flows (see \code{\link{growth_and_yield_summary}}), but this is
#'   not done here.
#'
#' @return A matrix where each row represents a point in time, and the first
#'   column represents the points in time. All other columns stand for the stand
#'   development phases as defined in \code{concept_def}. Depending on the
#'   choice made with \code{type}, they contain either standing volumes, or
#'   removal volumes, or mortality volumes.
#'
#' @noRd
#'
volume_from_area <- function(area_agg, concept_def,
                             type = c("standing", "removal", "mort", "inc")) {

  type <- match.arg(type)
  v_def_name <- switch(type,
                       "standing" = "vol_standing",
                       "removal"  = "vol_remove",
                       "mort"     = "vol_mort",
                       "inc"      = "vol_increment")
  vol_unitarea <- concept_def$growth_and_yield[[v_def_name]]
  n_phases <- nrow(concept_def$growth_and_yield)
  areas <- area_agg[, -1] # remove time column

  vol <- t(t(areas) * vol_unitarea)

  cbind(time = area_agg[, 1], vol) # add time column
}


#' Calculate Volume Flows at Phase Transitions Parallel to Regular Area Flows
#'
#' The calculation is based on the volume differences between the subsequent
#' stand development phases of the concept of interest. The differences can be >
#' 0 (volume increase at phase transition) or < 0 (volume decrease). Hereby, it
#' is important to take into account that the last phase is connected to the
#' first one. Here, only regular area flows, no damage events are included.
#'
#'
#' @param areaflws_reg_agg Aggregated simulated area dynamics, more precisely,
#'   the list element \code{area_inflows_regular} of the output of
#'   \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param concept_def Concept definition matching \code{areaflws_reg_agg}, a
#'   \code{c4c_concept} object
#'
#' @return A matrix where each row is a point in time, and time is stored in the
#'   first column. All other columns stand for the stand development phases as
#'   defined in \code{concept_def}. They contain the volume flows since the
#'   previous point in time. Negative volume flows are volume reductions *from*
#'   the phase of interest at area transitions from this to the subsequent phase
#'   (which has a lower standing volume). Positive volume flows are volume
#'   increments that occur at area transitions *from* the phase of interest to
#'   the subsequent phase (which has a higher standing volume). The first row
#'   of the matrix has \code{NA} entries, because there is no previous point
#'   in time since when a flow could have occurred.
#'
#' @noRd
#'
volume_coflows_regular <- function(areaflws_reg_agg, concept_def) {

  n_phases  <- nrow(concept_def$growth_and_yield)
  areafflws <- areaflws_reg_agg[, -1] # Remove time column

  # volume differences per unit area from phase to phase
  vol_std   <- concept_def$growth_and_yield$vol_standing
  vol_diff  <- vol_std - vol_std[c(n_phases, 1:(n_phases - 1))]

  vol_flws  <- t(t(areafflws) * vol_diff)

  # For subsequent calculations it is more convenient to attribute the volume
  # flows not to the phase where the corresponding area flows got to, but where
  # they come from. This way, volume reductions are connected to the phase whose
  # volume is reduced.
  # (volume increases are, analogously, the volume increases that occur when
  # an area leaves its phase)
  cl_names     <- colnames(vol_flws)
  cl_index_new <- dplyr::lead(1:n_phases, default = 1)
  vol_flws     <- vol_flws[, cl_index_new]
  colnames(vol_flws) <- cl_names # set original order of column names

  cbind(time = areaflws_reg_agg[, 1], vol_flws) # add time column
}



#' Calculate Volume Losses due to Damage Events
#'
#' Volume loss is the standing volume difference between the initial phase where
#' a damaged area goes to to and the volume of the losing phase per unit area.
#'
#'
#' @param areaflws_events_agg Aggregated simulated area dynamics, more
#'   precisely, the list element \code{area_outflows_events} of the output of
#'   \code{\link{aggregate_raw_sim_rslt}}

#' @param concept_def Concept definition matching \code{areaflws_events_agg}, a
#'   \code{c4c_concept} object
#'
#' @return A matrix where each row is a point in time, and time is stored in the
#'   first column. All other columns stand for the stand development phases as
#'   defined in \code{concept_def}. They contain the volume losses due to
#'   damaging events since the previous point in time. The first row of the
#'   matrix has \code{NA} entries, because there is no previous point in time
#'   since when a flow could have occurred.
#'
#' @noRd
#'
volume_coflows_events <- function(areaflws_evts_agg, concept_def) {

  areafflws <- areaflws_evts_agg[, -1] # Remove time column

  # volume differences per unit area to the initial phase
  vol_diff <- concept_def$growth_and_yield$vol_standing[1] -
    concept_def$growth_and_yield$vol_standing

  vol_flws  <- t(t(areafflws) * vol_diff)

  cbind(time = areaflws_evts_agg[, 1], vol_flws) # add time column
}





#' Produce a List of Growth and Yield Pre-Evaluation Matrices for Use in Higher-
#' Level Evaluations
#'
#' @param sim_agg Aggregated simulated area dynamics, i.e. the output of
#'   \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param concept_def Concept definition matching \code{sim_agg}, a
#'   \code{c4c_concept} object
#'
#' @return A list of matrices, each with the same structure. Each row represents
#'   a point in time, and the first column represents the points in time. All
#'   other columns stand for the stand development phases as defined in
#'   \code{concept_def}. The matrices are named 1) *vol_standing*, 2)
#'   *vol_rmv_cont*, 3) *vol_mrt_cont*, 4) *vol_ofl_reg*, and 5) *vol_ofl_evt*.
#'   They contain 1) the standing wood volume, 2) the continously removed
#'   volume, 3) volume losses from continuous mortality (according to the
#'   phase-wise mortality definition, not from damaging events), 4) regular
#'   volume outflows at phase transitions (i.e. transitions from a phase with a
#'   higher standing volume to a phase with lower volume), and 5) volume
#'   outflows caused by damaging events (which have \code{NA} entries in the
#'   first row).
#'
#' @noRd
#'
growth_and_yield_pre_eval <- function(sim_agg, concept_def) {

  areas_agg <- sim_agg$areas
  area_flws <- sim_agg$area_inflows_regular
  area_evts <- sim_agg$area_outflows_events

  # Standing colume
  v_standing   <- volume_from_area(areas_agg, concept_def, type = "standing")
  # Continously removed volume from phases
  vol_rmv_cont <- volume_from_area(areas_agg, concept_def, "removal")
  # Volume losses due to continuous mortality
  vol_mrt_cont <- volume_from_area(areas_agg, concept_def, "mort")
  # Volume increment (as upscaled from the ha values in the concept definition)
  vol_inc_ups  <- volume_from_area(areas_agg, concept_def, "inc")

  # Regular volume outflows from phases (at phase transitions), taken as harvest
  # (can in theory also contain mortality)
  vol_ofl_reg <- volume_coflows_regular(area_flws, concept_def)
  n_cl        <- ncol(vol_ofl_reg)
  vol_ofl_reg[, 2:n_cl] <- abs(
    ifelse(vol_ofl_reg[, 2:n_cl] > 0, 0, vol_ofl_reg[, 2:n_cl])
  )

  # Event-caused volume outflows from phases (come as negative values)
  vol_ofl_evt <- abs(volume_coflows_events(area_evts, concept_def))

  list(vol_standing = v_standing,
       vol_rmv_cont = vol_rmv_cont,
       vol_mrt_cont = vol_mrt_cont,
       vol_ofl_reg  = vol_ofl_reg,
       vol_ofl_evt  = vol_ofl_evt,
       vol_inc_ups  = vol_inc_ups
  )
}



#' Overall Growth and Yield Evaluation
#'
#' Provides a phase-overarching growth and yield summary
#'
#'
#' @param gy_pre Output of \code{\link{growth_and_yield_pre_eval}}
#'
#' @return A tibble, where each row is a point in time. The columns (in addition
#'   to time) are: *vol_standing*, the standing volume on the total area of
#'   interest; *vol_rmv_cont*,the continuous removals that take place, as long
#'   as an area is in a given phase; *vol_rmv_damage*, the volume losses due to
#'   damage events; *vol_rmv_total*, all removed volume, the sum of vol_rmv_cont
#'   and vol_rmv_damage; *vol_mort*, the mortality volume (normal, not
#'   event-triggered); *vol_inc_ups*, the volume increment as upscaled from the
#'   phase-wise increments given in the concept definition;
#'   *vol_inc*, the volume increment on the whole area resulting from
#'   vol_standing, vol_rmv_total, and vol_mort (i.e. from the simulated volume
#'   history on the estate level).
#'
#' @noRd
#'
growth_and_yield_summary <- function(gy_pre) {

  gy <- tibble::tibble(
    time            = gy_pre$vol_standing[, 1],
    vol_standing    = rowSums(gy_pre$vol_standing[, -1]),
    # continuously removed volume
    vol_rmv_cont    = rowSums(gy_pre$vol_rmv_cont[, -1]),
    # Not required anymore because now contained in vol_rmv_cont:
    # removals at phase transitions
    # vol_rmv_trans   = rowSums(gy_pre$vol_ofl_reg[, -1]),
    # removals due to damage events
    vol_rmv_damage  = abs(rowSums(gy_pre$vol_ofl_evt[, -1])),
    # Not required anymore because now contained in vol_rmv_cont:
    # removals due to regular harvest
    # vol_rmv_harvest = .data$vol_rmv_cont +
    #   c(0, .data$vol_rmv_trans[2:length(.data$vol_rmv_trans)]), # first is NA
    # regular and irregular harvest
    vol_rmv_total   = .data$vol_rmv_cont + # earlier: .data$vol_rmv_harvest
      c(0, .data$vol_rmv_damage[2:length(.data$vol_rmv_damage)]), # first is NA
    # losses due to continuous mortality
    vol_mort        = rowSums(gy_pre$vol_mrt_cont[, -1]),
    # volume increment as upscaled from the phase wise increments
    vol_inc_ups     = rowSums(gy_pre$vol_inc_ups[, -1])
  )

  # volume increment calculated from the simulated volume history on estate
  # level
  gy |> dplyr::mutate(vol_inc = .data$vol_standing -
    dplyr::lag(.data$vol_standing) +
    .data$vol_rmv_total + .data$vol_mort)
}



#' Phasewise Growth and Yield Evaluation
#'
#' Calculate the same variables as does \code{\link{growth_and_yield_summary}},
#' but separately for each stand development phase.
#'
#' @param gy_pre Output of \code{\link{growth_and_yield_pre_eval}}
#'
#' @return List of tibbles, one for each variable as calculated also by
#'   \code{\link{growth_and_yield_summary}} (except the version of the volume
#'   increment that results from the simulated history, because it makes no
#'   sense in a phase-wise context), but in phase-wise resolution. In each
#'   tibble, each row is a point in time, the columns represent the stand
#'   development phases.
#'
#' @noRd
#'
growth_and_yield_phasewise <- function(gy_pre) {

  n_c <- ncol(gy_pre$vol_rmv_cont)
  n_r <- nrow(gy_pre$vol_rmv_cont)

  # Not required anymore due to inclusive definition of vol_rmv_cont
  # vol_rmv_harvest <- gy_pre$vol_rmv_cont
  # vol_rmv_harvest[2:n_r, 2:n_c] <- vol_rmv_harvest[2:n_r, 2:n_c] +
  #   gy_pre$vol_ofl_reg[2:n_r, 2:n_c]
  #   # 2:n_r above, because first row of vol_ofl_reg is NA

  # vol_rmv_total <- vol_rmv_harvest
  # vol_rmv_total[2:n_r, 2:n_c] <- vol_rmv_harvest[2:n_r, 2:n_c] +
  #   gy_pre$vol_ofl_evt[2:n_r, 2:n_c]
  #   # 2:n_r above, because first row of vol_ofl_evt is NA

  # The previous three lines had to be changed due to the new definition into:
  vol_rmv_total <- gy_pre$vol_rmv_cont
  vol_rmv_total[2:n_r, 2:n_c] <- vol_rmv_total[2:n_r, 2:n_c] +
    gy_pre$vol_ofl_evt[2:n_r, 2:n_c]
  # 2:n_r above, because first row of vol_ofl_evt is NA, i.e. nothing needs to
  # be added to the first row

  list(vol_standing    = gy_pre$vol_standing,
       vol_rmv_cont    = gy_pre$vol_rmv_cont,
       # vol_rmv_trans   = gy_pre$vol_ofl_reg,
       vol_rmv_damage  = gy_pre$vol_ofl_evt,
       # vol_rmv_harvest = vol_rmv_harvest,
       vol_rmv_total   = vol_rmv_total,
       vol_mort        = gy_pre$vol_mrt_cont,
       vol_inc_ups     = gy_pre$vol_inc_ups) |>
    lapply(FUN = tibble::as_tibble)
}



#' Detailed Harvest Evaluation
#'
#' @param gy_phasewise An object returned from
#'   \code{\link{growth_and_yield_phasewise}}
#'
#' @param concept_def Concept definition matching \code{gy_phasewise}, a
#'   \code{c4c_concept} object
#'
#' @return A tibble that contains for each point in time the harvest volume
#'   structured by regular and damage-induced harvest, and by mean dbh of the
#'   harvested trees
#'
#' @noRd
#'
growth_and_yield_harvest_details <- function(gy_phasewise, concept_def) {

  # Evaluate for regular harvest
  eval_regular <- gy_phasewise$vol_rmv_cont |>
    tidyr::pivot_longer(
      -.data$time,
      names_to = "phase_name", values_to = "volume"
    ) |>
    dplyr::inner_join(
      concept_def$growth_and_yield |>
        dplyr::select(
          .data$phase_no, .data$phase_name, .data$dbh_remove, .data$vol_remove,
          .data$n_remove, .data$harvest_interval
        ),
      by = "phase_name"
    ) |>
    dplyr::mutate(
      dbh_cm      = round(.data$dbh_remove, 1),
      vol_tree_m3 = ifelse(
        .data$n_remove > 0, .data$vol_remove / .data$n_remove, 0
      ) |>
        round(digits = 2),
      tree_dist_m = ifelse(
        .data$n_remove > 0,
        sqrt(10000 / (.data$n_remove * .data$harvest_interval)),
        0
      ) |>
        round(digits = 2)
    ) |>
    dplyr::select(-.data$dbh_remove, -.data$vol_remove, -.data$n_remove) |>
    dplyr::mutate(harvest_type = "regular")

  # Evaluate for damage-induced harvest
  # looks very similar to above, but some details are different
  eval_damage <- gy_phasewise$vol_rmv_damage |>
   tidyr::pivot_longer(
     -.data$time,
     names_to = "phase_name", values_to = "volume"
   ) |>
   dplyr::mutate(volume = ifelse(is.na(.data$volume), 0, .data$volume)) |>
   dplyr::inner_join(
     concept_def$growth_and_yield |>
       dplyr::select(
         .data$phase_no, .data$phase_name, .data$dbh_standing,
         .data$vol_standing, .data$n_standing
       ),
     by = "phase_name"
   ) |>
   dplyr::mutate(
     dbh_cm = round(.data$dbh_standing, 1),
     vol_tree_m3 = ifelse(
       .data$n_standing > 0, .data$vol_standing / .data$n_standing, 0
     ) |>
       round(digits = 2),
     tree_dist_m = ifelse(
       .data$n_standing > 0, sqrt(10000 / .data$n_standing), 0
     ) |>
       round(digits = 2)
   ) |>
   dplyr::select(-.data$dbh_standing, -.data$vol_standing, -.data$n_standing) |>
   dplyr::mutate(harvest_type = "damage")

  # Combine both, summarise, and hand the result back
  dplyr::bind_rows(eval_regular, eval_damage) |>
    dplyr::group_by(
      .data$time, .data$phase_no, .data$phase_name, .data$harvest_type,
      .data$dbh_cm, .data$vol_tree_m3, .data$tree_dist_m
    ) |>
    dplyr::summarise(volume = sum(.data$volume)) |>
  dplyr::select(.data$time, .data$harvest_type, .data$phase_no,
                .data$phase_name, dplyr::everything())
}



#' Extensive Growth and Yield Evaluation
#'
#' Provide an extensive compilation of growth and yield related results.
#'
#' The result object, a list, contains the following elements:
#' \itemize{
#'   \item{*gyield_summary*: A tibble that contains phase overarching growth
#'     and yield information. In this tibble, each row is a point in time. The
#'     columns (in addition to time) are: *vol_standing*, the standing volume on
#'     the total area of interest; *vol_rmv_cont*,the continuous removals that
#'     take place, as long as an area is in a given phase; *vol_rmv_trans*, the
#'     volume removals occurring at phase transitions from phases with more to
#'     such with less standing volume; *vol_rmv_damage*, the volume losses due
#'     to damage events; *vol_rmv_harvest*, regular harvest volume, the sum of
#'     vol_rmv_cont and vol_rmv_trans; *vol_rmv_total*, all removed volume, the
#'     sum of vol_rmv_harvest and vol_rmv_damage; *vol_mort*, the mortality
#'     volume (normal, not event-triggered); *vol_inc*, the volume increment on
#'     the whole area resulting from the vol_standing, vol_rmv_total,
#'     and vol_mort.}
#'   \item{*gyield_phases*: A list of tibbles, one for each variable as
#'     contained in gyield_summary (except the volume increment) which makes no
#'     sense in a phase-wise context), but in phase-wise resolution. In each
#'     tibble, each row is a point in time, the columns represent the stand
#'     development phases.}
#'   \item{*gyield_pre* (in case the user has chosen
#'     \code{detailed_out = TRUE}): A tibble, where each row is a point in
#'     time. The columns (in addition to time) are: *vol_standing*, the standing
#'     volume on the total area of interest; *vol_rmv_cont*,the continuous
#'     removals that take place, as long as an area is in a given phase;
#'     *vol_rmv_trans*, the volume removals occurring at phase transitions from
#'     phases with more to such with less standing volume;*vol_rmv_damage*, the
#'     volume losses due to damage events; *vol_rmv_harvest*, regular harvest
#'     volume, the sum of vol_rmv_cont and vol_rmv_trans; *vol_rmv_total*, all
#'     removed volume, the sum of vol_rmv_harvest and vol_rmv_damage;
#'     *vol_mort*, the mortality volume (normal, not event-triggered);
#'     *vol_inc*, the volume increment on the whole area resulting from the
#'     vol_standing, vol_rmv_total, and vol_mort.}
#' }
#'
#' @param sim_agg Aggregated simulated area dynamics, more precisely, the output
#'   of \code{\link{aggregate_raw_sim_rslt}}
#'
#' @param concept_def Concept definition matching \code{sim_agg}, a
#'   \code{c4c_concept} object
#'
#' @param detailed_out Boolean, if TRUE, also pre-evaluation output (calculated
#'   by the internal function growth_and_yield_pre_eval) will be part of the
#'   result list (default = FALSE)
#'
#' @return A list with two elements (see also details),
#' \itemize{
#'   \item{*gyield_summary*: A tibble that contains phase overarching growth
#'     and yield information.}
#'   \item{*gyield_phases*: A list of tibbles, each one representing one of the
#'     growth and yield variables also found in gyield_summary, but here on the
#'     level of the single stand development phases.}
#' }
#'   If the user has selected \code{detailed_out = TRUE}, there will also be
#'   another list element, *gyield_pre*, which contains the interim information
#'   from which gyield_summary and gyield_phases are calculated.
#'
#' @export
#'
#' @examples
#'   # Run a simulation and store the aggregated outcome
#'   state_vars <- setup_statevars(
#'     pine_thinning_from_above_1, c(1000, 0, 0, 0, 0, 0)
#'   )
#'   time_span <- 200
#'   parms     <- setup_parms(pine_thinning_from_above_1)
#'   parms$risk_mat <- setup_risk_events(
#'     time_span, avg_event_strength = 1, parms$risk
#'   )
#'
#'   sim_areas_agg <- sim_area_single_concept_with_risk(
#'     state_vars,
#'     parms       = parms,
#'     event_times = c(0:time_span),
#'     time_span   = time_span
#'   ) |>
#'     aggregate_raw_sim_rslt(pine_thinning_from_above_1)
#'
#'   # Growth and yield evaluation
#'   growth_and_yield_evaluation(sim_areas_agg, pine_thinning_from_above_1)
#'
growth_and_yield_evaluation <- function(sim_agg,
                                        concept_def,
                                        detailed_out = FALSE) {

  if(!is_c4c_concept(concept_def)) {
    stop("`concept_def` is not an c4c_concept object.")
  }

  gyield_pre <- growth_and_yield_pre_eval(sim_agg, concept_def)
  gyield_summary <- growth_and_yield_summary(gyield_pre)
  gyield_phases  <- growth_and_yield_phasewise(gyield_pre)
  gyield_harvest_detail <- growth_and_yield_harvest_details(
    gyield_phases, concept_def
  )

  gyield_rslt <- list(
    gyield_summary = gyield_summary,
    gyield_phases  = gyield_phases,
    gyield_harvest_detail = gyield_harvest_detail
  )

  if(detailed_out) gyield_rslt$gyield_pre <- gyield_pre

  gyield_rslt
}








