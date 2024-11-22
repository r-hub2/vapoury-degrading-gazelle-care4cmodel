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




#' Fuel Consumption and CO2 Emissions for Cutting
#'
#' Given the output of a simulation run (i.e. an object of class
#' \code{c4c_base_result}) as created with the function
#' \code{\link{simulate_single_concept}}, the fuel consumption and CO2 emissions
#' for cutting (i.e. felling, limbing, cutting the trees into logs) are
#' calculated. Currently, this function assumes only harvester operations.
#'
#' @param x An object of class \code{c4c_base_result}
#'
#' @param mode Character string to choose between "standard" (default) and
#'   "nordic". For "standard", the function \code{\link{fuel_cons_harvester_1}}
#'   is used. Hoewever, as this function does only provide values for operations
#'   with an average harvest tree diameter of 15 cm and more, the "nordic"
#'   function \code{\link{fuel_cons_harvester_2}} with the option \code{thinning
#'   = TRUE} is used for smaller average tree sizes.\cr
#'   With the choice "nordic" only the function
#'   \code{\link{fuel_cons_harvester_2}} is used. However, if the harvested
#'   volume per ha per operation amounts to 90% and more of the standing volume
#'   per ha of the respective stand development phase, which is virutally a
#'   clearcut, the option \code{thinning = FALSE} is used. In case of damage-
#'   induced harvest, only the option \code{thinning = TRUE} is in effect in
#'   order to account for the more difficult conditions of such harvest
#'   operations.
#'
#' @return A data frame (tibble) with the columns time, harvest_type (damage or
#'   regular), phase_no, phase_name (numbers and names of the stand development
#'   phases), fuel_cutting_l_per_m3 (liters of fuel consumed per m3 of
#'   harvested wood), fuel_cutting_total_l (liters of fuel consumed in total),
#'   co2_cutting_total_kg (kg CO2 emitted).
#'
#' @export
#'
#' @examples
#'   base_out <- simulate_single_concept(
#'    pine_thinning_from_above_1,
#'    init_areas = c(50, 100, 10, 50, 150, 600),
#'    time_span  = 50,
#'    risk_level = 3
#'   )
#'
#'   co2_eval_cutting(base_out, "standard")
#'   co2_eval_cutting(base_out, "nordic")
#'
#'
co2_eval_cutting <- function(x, mode = c("standard", "nordic")) {

  stopifnot(is_c4c_base_result(x))
  mode <- match.arg(mode)

  # Join harvest details from x with the concept definition, use this for
  # calculating the actual removal volume per ha per operation
  work_dat <- x$sim_growth_and_yield$gyield_harvest_detail |>
    dplyr::inner_join(x$concept$growth_and_yield |> dplyr::select(
      .data$phase_no, .data$phase_name, .data$vol_standing, .data$vol_remove,
      .data$harvest_interval
    )) |>
    dplyr::mutate(
      vol_remove_ha_per_op = dplyr::case_when(
        harvest_type == "damage"  ~ vol_standing,
        harvest_type == "regular" ~ ifelse(vol_remove > 0,
                                           vol_remove * harvest_interval,
                                           0)
      )
    )

  # In mode "standard" cutting with mean tree dbh >= 15 cm is done with the
  # function 'fuel_cons_harvester_1'; for smaller trees, the "nordic" function
  # `fuel_cons_harvester_2` is used with the option thinning == TRUE
  if(mode == "standard") {
    work_dat <- work_dat |> dplyr::mutate(
      fuel_cutting_l_per_m3 = purrr::pmap_dbl(

        .l = list(dbh       = .data$dbh_cm,
                  vt        = .data$vol_tree_m3,
                  vrm_ha    = .data$vol_remove_ha_per_op),

        .f = function(dbh, vt, vrm_ha) {
          ifelse(
            .data$vol_remove_ha_per_op > 0,
            ifelse(dbh >= 15,
                   fuel_cons_harvester_1(vt, dbh),
                   fuel_cons_harvester_2(vt, vrm_ha, thinning = TRUE)
            ),
            0
          )
        } # .f
      ) # pmap_dbl
    ) # mutate
  }

  # Mode "nordic" simply uses the function `fuel_cons_harvester_2`.
  # This function requires information whether the harvest operation of interest
  # is a thinning or some "other" type of operation, typically a final harvest.
  # As this is equivalent to a clearcut in the context of the function,
  # "other" will be considered only if 90% or more of the standing volume are
  # harvested during an operation. Damage-induced harvesting will always be
  # treated as a thinning, in order to be on the pessimistic side (higher
  # fuel consumption)
  if(mode == "nordic") {
    work_dat <- work_dat |> dplyr::mutate(
      thinning_flag = ifelse(
        (.data$harvest_type == "damage") | (.data$vol_standing == 0),
        TRUE,
        ifelse(.data$vol_remove_ha_per_op / .data$vol_standing < 0.9,
               TRUE,
               FALSE
        )
      )
    ) |>
    dplyr::mutate(
      fuel_cutting_l_per_m3 = ifelse(
        (.data$vol_tree_m3 == 0) | (.data$vol_remove_ha_per_op == 0),
        0,
        fuel_cons_harvester_2(
          .data$vol_tree_m3, .data$vol_remove_ha_per_op, .data$thinning_flag
        )
      )
    ) |>
    dplyr::select(-.data$thinning_flag)
  }

  # Calculate total fuel consumption, convert it into co2 emissions
  work_dat |>
    dplyr::ungroup() |>
    dplyr::mutate(
      fuel_cutting_total_l = .data$fuel_cutting_l_per_m3 * .data$volume,
      co2_cutting_total_kg = fuel_to_co2(.data$fuel_cutting_total_l, "diesel")
    ) |>
    dplyr::select(
      .data$time, .data$harvest_type,
      tidyselect::starts_with("phase_"), tidyselect::starts_with("fuel_"),
      tidyselect::starts_with("co2_")
    )
}





#' Fuel Consumption and CO2 Emissions for Moving Wood From the Felling Spot to
#' the Forest Road
#'
#' Given the output of a simulation run (i.e. an object of class
#' \code{c4c_base_result}) as created with the function
#' \code{\link{simulate_single_concept}}, the fuel consumption and CO2 emissions
#' for moving the wood to a truck road are calculated. Currently, this function
#' assumes only forwarder operations.
#'
#'
#' @param x An object of class \code{c4c_base_result}
#'
#' @param road_density Forest road density (m/ha), relating to truck-accessible
#'   roads
#'
#' @param rel_loss Relative amount of the standing tree volume that is lost
#'   during harvesting (default 0.1). Note that the harvested amount is reduced
#'   with the factor 1 - rel_loss before upscaling from the fuel consumption per
#'   m³, because only the wood remaining after the harvest loss (mainly the
#'   stumps) is actually moved.
#'
#' @param mode Character string to choose between "standard" (default) and
#'   "nordic". With the option "standard", the function
#'   \code{\link{fuel_cons_forwarder_1}} is used, while "nordic" makes use of
#'   \code{\link{fuel_cons_forwarder_2}}
#'
#' @return A data frame (tibble) with the columns time, harvest_type (damage or
#'   regular), phase_no, phase_name (numbers and names of the stand development
#'   phases), fuel_moving_l_per_m3 (liters of fuel consumed per m3 of moved
#'   wood), fuel_moving_total_l (liters of fuel consumed in total),
#'   co2_moving_total_kg (kg CO2 emitted).
#'
#' @export
#'
#' @examples
#'   base_out <- simulate_single_concept(
#'    pine_thinning_from_above_1,
#'    init_areas = c(50, 100, 10, 50, 150, 600),
#'    time_span  = 50,
#'    risk_level = 3
#'   )
#'
#'   co2_eval_moving(base_out, road_density = 35, mode = "standard")
#'   co2_eval_moving(base_out, road_density = 35, mode = "nordic")
#'
co2_eval_moving <-function(x, road_density, rel_loss = 0.1,
                           mode = c("standard", "nordic")) {

  stopifnot(is_c4c_base_result(x))
  mode <- match.arg(mode)

  aed <- avg_extraction_distance(road_density)
  work_dat <- x$sim_growth_and_yield$gyield_harvest_detail

  if(mode == "standard") {
    work_dat <- work_dat |>
      dplyr::mutate(
        fuel_moving_l_per_m3 = fuel_cons_forwarder_1(aed)
      )
  }

  # For "nordic" the calculation requires the removal volume per ha per
  # operation, which requires to merge in the growth and yield part of the
  # concept definition. For fuel consumption we allow mineral_soil = TRUE only
  if(mode == "nordic") {
    work_dat <- work_dat |>
      dplyr::inner_join(x$concept$growth_and_yield |> dplyr::select(
        .data$phase_no, .data$phase_name, .data$vol_standing, .data$vol_remove,
        .data$harvest_interval)
      ) |>
      dplyr::mutate(
        vol_remove_ha_per_op = dplyr::case_when(
          harvest_type == "damage"  ~ vol_standing,
          harvest_type == "regular" ~ ifelse(vol_remove > 0,
                                             vol_remove * harvest_interval,
                                             0)
        ),
        fuel_moving_l_per_m3 = ifelse(
          .data$vol_remove_ha_per_op != 0,
          fuel_cons_forwarder_2(
            aed, .data$vol_remove_ha_per_op, mineral_soil = TRUE
          ),
          0
        )
      )
  }

  # Calculate total fuel consumption, convert it into co2 emissions
  work_dat |>
    dplyr::ungroup() |>
    dplyr::mutate(
      fuel_moving_total_l = .data$fuel_moving_l_per_m3 *
        .data$volume * (1 - rel_loss), # only this vol. is acutally transported
      co2_moving_total_kg = fuel_to_co2(.data$fuel_moving_total_l, "diesel")
    ) |>
    dplyr::select(
      .data$time, .data$harvest_type,
      tidyselect::starts_with("phase_"), tidyselect::starts_with("fuel_"),
      tidyselect::starts_with("co2_")
    )
}



#' Overarching Evaluation of Fuel Consumption and CO2 Emissions
#'
#' Given the output of a simulation run generated with
#' \code{\link{simulate_single_concept}}, i.e. an object of class
#' \code{c4c_base_result}, a set of information related to CO2 emissions and
#' storage is generated on different levels of aggregation.
#'
#' @param x An object of class \code{c4c_base_result}
#'
#' @param road_density_m_ha The forest road density on the whole area in m/ha
#'
#' @param raw_density_kg_m3 The raw wood density (kg/m³) to be used for wood
#'   volume conversions (i.e. density of air-dry wood (12% water content)).
#'   Default is 520 kg/m³ (typical for Scots pine). Internally, wood volume is
#'   converted into CO2 equivalents with \code{\link{wood_to_co2}}.
#'
#' @param harvest_loss Relative loss fraction of wood volume during harvest,
#'   mainly comprising the stumps (default = 0.1). Does not include bark losses.
#'
#' @param bark_share Relative wood volume share of the bark. Required, as the
#'   CO2 equivalents of harvested wood are calculated for wood volume under
#'   bark.
#'
#' @param mode Character string indicating the mode of calculating fuel
#'   consumption due to harvest operations. This relates to the functions
#'   \code{\link{co2_eval_cutting}} and \code{\link{co2_eval_moving}}, see the
#'   documentation of these functions for details.
#'
#' @return An object of class \code{c4c_co2_result} which is, in essence, a list
#'   of three result data frames (and metadata about the underlying simulation),
#'   providing information about co2 emissions, storage, and fuel consumption on
#'   different levels of aggregation.
#'
#' @export
#'
#' @examples
#'   # Make a simulation run first
#'   # The resulting object base_output (class c4c_base_result) comprises
#'   # the simulated phase area dynamics as well as extended growth and yield
#'   # information
#'   base_output <- simulate_single_concept(
#'     pine_thinning_from_above_1,
#'     init_areas = c(1000, 0, 0, 0, 0, 0),
#'     time_span  = 200,
#'     risk_level = 3
#'   )
#'
#'   # Generate fuel and CO2 related information
#'   fuel_and_co2_evaluation(
#'     base_output,
#'     road_density_m_ha = 35,
#'     mode = "nordic"
#'   )
#'
fuel_and_co2_evaluation <- function(x,
                                    road_density_m_ha,
                                    raw_density_kg_m3 = 520,
                                    harvest_loss = 0.1,
                                    bark_share = 0.12,
                                    mode = c("standard", "nordic")) {

  stopifnot(is_c4c_base_result(x))
  mode <- match.arg(mode)

  cut  <- co2_eval_cutting(x, mode)
  move <- co2_eval_moving(x, road_density_m_ha, harvest_loss, mode)

  # Detailed view by phases
  co2_phases <- cut |>
    dplyr::inner_join(
      move, by = c("time", "harvest_type", "phase_no", "phase_name")
    ) |>
    dplyr::inner_join(x$sim_growth_and_yield$gyield_harvest_detail |>
                        dplyr::ungroup() |>
                        dplyr::select(
                          .data$time, .data$harvest_type, .data$phase_no,
                          .data$phase_name, .data$volume
                        )
    ) |>
    dplyr::mutate(
      # what is actually harvested - substract harvest loss and bark
      co2_equiv_harvest_kg = wood_to_co2(
        .data$volume * (1 - harvest_loss) * (1 - bark_share),
        raw_density_kg_m3
      )
    ) |>
    dplyr::select(-.data$volume)

  # Intermediate aggregation level
  co2_agg_med <- co2_phases |>
    dplyr::group_by(.data$time, .data$harvest_type) |>
    dplyr::summarise(fuel_cutting_total_l = sum(.data$fuel_cutting_total_l),
                     fuel_moving_total_l  = sum(.data$fuel_moving_total_l),
                     co2_cutting_total_kg = sum(.data$co2_cutting_total_kg),
                     co2_moving_total_kg  = sum(.data$co2_moving_total_kg),
                     co2_equiv_harvest_kg = sum(.data$co2_equiv_harvest_kg))

  # High aggregation level
  co2_agg_hi <- co2_agg_med |>
    dplyr::group_by(.data$time) |>
    dplyr::summarise(fuel_cutting_total_l = sum(.data$fuel_cutting_total_l),
                     fuel_moving_total_l  = sum(.data$fuel_moving_total_l),
                     co2_cutting_total_kg = sum(.data$co2_cutting_total_kg),
                     co2_moving_total_kg  = sum(.data$co2_moving_total_kg),
                     co2_equiv_harvest_kg = sum(.data$co2_equiv_harvest_kg)) |>
    dplyr::mutate(fuel_road_maint_total_l = rowSums(
      x$sim_areas$areas[, 2:ncol(x$sim_areas$areas)] *
        fuel_cons_road_maintenance(road_density_m_ha)
      ),
      co2_road_maint_total_kg = fuel_to_co2(.data$fuel_road_maint_total_l,
                                            "diesel")
    ) |>
    dplyr::inner_join(x$sim_growth_and_yield$gyield_summary |>
                        dplyr::select(.data$time, .data$vol_standing,
                                      .data$vol_inc_ups)) |>
    dplyr::mutate(
      co2_equiv_inc_kg      = wood_to_co2(.data$vol_inc_ups, raw_density_kg_m3),
      co2_equiv_standing_kg = wood_to_co2(.data$vol_standing, raw_density_kg_m3)
    ) |>
    dplyr::select(-.data$vol_standing, -.data$vol_inc_ups) |>
    dplyr::relocate(.data$fuel_road_maint_total_l,
                    .after = .data$fuel_moving_total_l) |>
    dplyr::relocate(.data$co2_road_maint_total_kg,
                    .after = .data$co2_moving_total_kg)

  # Output as a list
  rslt <- list(
    concept        = x$concept,
    time_span      = x$time_span,
    detailed_init  = x$detailed_init,
    init_areas     = x$init_areas,
    risk_level     = x$risk_level,

    parameters_co2 = list(
      road_density_m_ha = road_density_m_ha,
      raw_density_kg_m3 = raw_density_kg_m3,
      harvest_loss      = harvest_loss,
      bark_share        = bark_share,
      mode              = mode
    ),

    co2_agg_high   = co2_agg_hi,
    co2_agg_medium = co2_agg_med,
    co2_by_phases  = co2_phases
  )

  # Make it an object of class c4c_co2_result
  new_c4c_co2_result(rslt)
}





