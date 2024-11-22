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




#' Plot Function for c4c_co2_result Objects
#'
#' @param x Object of class \code{c4c_base_result}
#'
#' @param plot_type Character string, specifies the kind of diagram to be
#'   plotted. The options are:
#'   \itemize{
#'     \item{"em_by_type": The CO2 emissions by operation type (cutting,
#'       moving, forest road maintenance) over time}
#'     \item{"fl_by_type": The fuel consumption by operation type (cutting,
#'       moving, forest road maintenance) over time}
#'     \item{"em_by_phase": The CO2 emissions by stand development phase
#'       over time, not including forest road maintenance}
#'     \item{"fl_by_phase": The fuel consumption by stand development phase
#'       over time, not including forest road maintenance}
#'     \item{"em_vs_inc": The CO2 emissions plotted against the wood increment
#'       (in CO2 equivalents)}
#'     \item{"em_vs_hrv": The CO2 emissions plotted against the harvest (in
#'       CO2 equivalents)}
#'     \item{"em_inc_ratio": The ratio of all CO2 emissions and the wood
#'       increment (in CO2 equivalents) over time}
#'   }
#'
#' @param ... Other parameters; currently not used
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#'
#'   sim_co2_out <- simulate_single_concept(
#'     pine_thinning_from_above_1,
#'     init_areas = c(1000, 0, 0, 0, 0, 0),
#'     time_span  = 200,
#'     risk_level = 3
#'   ) |>
#'     fuel_and_co2_evaluation(road_density_m_ha = 35, mode = "nordic")
#'
#'   # Make a plot
#'   plot(sim_co2_out,  plot_type = "em_by_type")
#'   # Also try the plot types "fl_by_type", "em_by_phase", "fl_by_phase",
#'   # "em_vs_inc", "em_vs_hrv", "em_inc_ratio"
#'
plot.c4c_co2_result <- function(x,
                                plot_type = c("em_by_type",
                                              "fl_by_type",
                                              "em_by_phase",
                                              "fl_by_phase",
                                              "em_vs_inc",
                                              "em_vs_hrv",
                                              "em_inc_ratio"),
                                ...) {

  stopifnot(is_c4c_co2_result(x))
  plot_type <- match.arg(plot_type)

  # sub-function for building the phase levels required for some area plots
  make_phase_levels <- function(x) {
    n <- nrow(x$concept$growth_and_yield)
    x$concept$growth_and_yield$phase_name[n:1]
  }

  # CO2 emissions for cutting and moving by stand development phase
  if(plot_type == "em_by_phase") {
    gg_out <- x$co2_by_phases |>
      dplyr::mutate(
        co2_cut_move = .data$co2_cutting_total_kg + .data$co2_moving_total_kg,
        phase = ordered(.data$phase_name, levels = make_phase_levels(x))
      ) |>
      dplyr::group_by(.data$time, .data$phase) |>
      dplyr::summarise(co2_cut_move = sum(.data$co2_cut_move)) |>
      ggplot() +
        geom_area(aes(x = .data$time, y = .data$co2_cut_move,
                      fill = .data$phase)) +
      scale_fill_viridis_d(name = "stand development phase") +
      xlab(paste0("time (", x$concept$units[["time"]], ")")) +
      ylab("CO2 emissions cutting and moving (kg/year)")
  }


  # Fuel consumption for cutting and moving by stand development phase
  if(plot_type == "fl_by_phase") {
    gg_out <- x$co2_by_phases |>
      dplyr::mutate(
        fuel_cut_move = .data$fuel_cutting_total_l + .data$fuel_moving_total_l,
        phase = ordered(.data$phase_name, levels = make_phase_levels(x))
      ) |>
      dplyr::group_by(.data$time, .data$phase) |>
      dplyr::summarise(fuel_cut_move = sum(.data$fuel_cut_move)) |>
      ggplot() +
      geom_area(aes(x = .data$time, y = .data$fuel_cut_move,
                    fill = .data$phase)) +
      scale_fill_viridis_d(name = "stand development phase") +
      xlab(paste0("time (", x$concept$units[["time"]], ")")) +
      ylab("fuel consumption cutting and moving (l/year)")
  }


  # CO2 emissions by emission type
  if(plot_type == "em_by_type") {
    gg_out <- x$co2_agg_high |>
      dplyr::select(.data$time, .data$co2_cutting_total_kg,
                    .data$co2_moving_total_kg, .data$co2_road_maint_total_kg) |>
      tidyr::pivot_longer(cols      = tidyr::starts_with("co2_"),
                          names_to  = "emission_type",
                          values_to = "co2_equiv_kg") |>
      dplyr::mutate(
        emission_type = dplyr::case_when(
          .data$emission_type == "co2_cutting_total_kg"     ~ "cutting",
          .data$emission_type == "co2_moving_total_kg"      ~ "moving",
          .data$emission_type == "co2_road_maint_total_kg"  ~ "road maintenance"
        ) |> ordered(levels = c("cutting", "moving", "road maintenance"))
      ) |>
      ggplot() +
      geom_area(aes(x = .data$time, y = .data$co2_equiv_kg,
                    fill = .data$emission_type)) +
      scale_fill_viridis_d("emission type", option = "mako") +
      xlab(paste0("time (", x$concept$units[["time"]], ")")) +
      ylab("CO2 emissions (kg/year)")
  }

  # Fuel consumption by emission type
  if(plot_type == "fl_by_type") {

    gg_out <- x$co2_agg_high |>
      dplyr::select(.data$time, .data$fuel_cutting_total_l,
                    .data$fuel_moving_total_l, .data$fuel_road_maint_total_l) |>
      tidyr::pivot_longer(cols      = tidyr::starts_with("fuel_"),
                          names_to  = "operation_type",
                          values_to = "fuel_cons_l") |>
      dplyr::mutate(
        emission_type = dplyr::case_when(
          .data$operation_type == "fuel_cutting_total_kg"   ~ "cutting",
          .data$operation_type == "fuel_moving_total_kg"    ~ "moving",
          .data$operation_type == "fuel_road_maint_total_l" ~ "road maintenance"
        ) |> ordered(levels = c("cutting", "moving", "road maintenance"))
      ) |>
      ggplot() +
      geom_area(aes(x = .data$time, y = .data$fuel_cons_l,
                    fill = .data$operation_type)) +
      scale_fill_viridis_d("emission type", option = "mako") +
      xlab(paste0("time (", x$concept$units[["time"]], ")")) +
      ylab("fuel consumption (l/year)")
  }


  # Emissions versus increment (CO2-equivalents)
  if(plot_type == "em_vs_inc") {
    gg_out <- x$co2_agg_high |>
      dplyr::mutate(
        co2_emiss = .data$co2_cutting_total_kg + .data$co2_moving_total_kg +
          .data$co2_road_maint_total_kg
      ) |>
      ggplot() +
      geom_path(
        aes(x = .data$co2_equiv_inc_kg , y = .data$co2_emiss, col = .data$time),
        lineend = "round") +
      scale_color_viridis_c(paste0("time (", x$concept$units[["time"]], ")")) +
      xlab("increment (kg CO2 equiv/year)") +
      ylab("CO2 emissions (kg/year)")
  }


  # Emissions versus increment (CO2-equivalents)
  if(plot_type == "em_vs_hrv") {
    gg_out <- x$co2_agg_high |>
      dplyr::mutate(
        co2_emiss =
          .data$co2_cutting_total_kg + .data$co2_moving_total_kg +
          .data$co2_road_maint_total_kg
      ) |>
      ggplot() +
      geom_path(
        aes(x = .data$co2_equiv_harvest_kg, y = .data$co2_emiss,
            col = .data$time),
        lineend = "round") +
      scale_color_viridis_c(paste0("time (", x$concept$units[["time"]], ")")) +
      xlab("harvest (kg CO2 equiv/year)") +
      ylab("CO2 emissions (kg/year)")
  }


  # Emission - increment ratio (both in CO2 equivalents)
  if(plot_type == "em_inc_ratio") {
    gg_out <- x$co2_agg_high |>
      dplyr::mutate(
        em_inc_ratio =
          (.data$co2_cutting_total_kg +
             .data$co2_moving_total_kg +
             .data$co2_road_maint_total_kg) /
          .data$co2_equiv_inc_kg
      ) |>
      ggplot() +
      geom_path(aes(x = .data$time , y = .data$em_inc_ratio),
                lineend = "round") +
      scale_color_viridis_c(x$units$time) +
      xlab(paste0("time (", x$concept$units["time"], ")")) +
      ylab("CO2 emissions/CO2 equiv. increment")
  }


  gg_out
}

