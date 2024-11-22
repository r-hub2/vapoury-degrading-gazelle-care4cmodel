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




#' Plot Function for c4c_base_result Objects
#'
#' @param x Object of class \code{c4c_base_result}
#'
#' @param variable Character string, specifies the variable to be plotted. The
#'   options are:
#'   \itemize{
#'     \item{"area": The areas covered by the different stand development
#'       phases (SDPH) over time}
#'     \item{"vol_standing": The standing volume by SDPH over time}
#'     \item{"vol_inc_ups": The volume increment by SDPH (upscaled from the
#'       concept definition) over time}
#'     \item{"vol_rmv_total": The total removed volume (comprising regular and
#'       damage-induced harvest) by SDPH over time}
#'     \item{"vol_rmv_cont": The volume removed due to regular harvest by SDPH
#'       over time}
#'     \item{"vol_rmv_damage": The volume removed due to damage-induced
#'       harvest by SDPH over time}
#'     \item{"vol_mort": The volume losses due to mortality by SDPH over time}
#'     \item{"hrvst_det_reg": The regular harvest volume over time by tree
#'       size classes (mean harvest diameter)}
#'     \item{"hrvst_det_dam": The damage-induced harvest volume over time by
#'       tree size classes (mean harvest diameter)}
#'   }
#'
#' @param ... Other parameters, currently not used
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#'
#'   sim_base_out <- simulate_single_concept(
#'     pine_thinning_from_above_1,
#'     init_areas = c(1000, 0, 0, 0, 0, 0),
#'     time_span  = 200,
#'     risk_level = 3
#'   )
#'
#'   # Make a plot
#'   plot(sim_base_out, variable = "area")
#'   # Also try the following options for the parameter "variable":
#'   # "vol_standing", "vol_inc_ups", "vol_rmv_total", "vol_rmv_cont",
#'   # "vol_rmv_damage", "vol_mort", "hrvst_det_reg", "hrvst_det_dam"
#'
plot.c4c_base_result <- function(x,
                                 variable = c("area",
                                              "vol_standing",
                                              "vol_inc_ups",
                                              "vol_rmv_total",
                                              "vol_rmv_cont",
                                              "vol_rmv_damage",
                                              "vol_mort",
                                              "hrvst_det_reg",
                                              "hrvst_det_dam"),
                                 ...) {

  if(!is_c4c_base_result(x)) {
    stop("Input object does not have class c4c_base_result.")
  }

  variable = match.arg(variable)

  # sub-function for building the phase levels required for some area plots
  make_phase_levels <- function(x) {
    n <- nrow(x$concept$growth_and_yield)
    x$concept$growth_and_yield$phase_name[n:1]
  }

  # sub-function for building the harvest diameter levels required for some
  # area plots
  make_dbh_levels <- function(x, variable) {
    h_type <- ifelse(variable == "hrvst_det_reg", "regular", "damage")
    x$sim_growth_and_yield$gyield_harvest_detail |>
      dplyr::filter(.data$harvest_type == h_type, .data$dbh_cm > 0) |>
      purrr::pluck("dbh_cm") |>
      unique() |>
      sort(decreasing = TRUE)
  }

  # sub-function which is used to generate the plot info required below for
  # plots with phase levels
  make_plot_info_1 <- function(dat_, phase_levels, variable, xlab, ylab) {
    list(
      dat = dat_ |>
        tibble::as_tibble() |>
        tidyr::pivot_longer(
          cols = -.data$time, values_to = variable, names_to = "phase"
        ) |>
        dplyr::mutate(phase = ordered(.data$phase, levels = phase_levels)),
      xlab = xlab,
      ylab = ylab,
      col_scale_name = "stand development phase",
      var_to_plot = variable
    )
  }

  # sub-function which is used to generate the plot info required below for
  # plots with dbh levels
  make_plot_info_2 <- function(dat_, dbh_levels, xlab, ylab) {
    list(
      dat = dat_ |>
        dplyr::filter(.data$dbh_cm > 0) |>
        dplyr::mutate(phase = ordered(.data$dbh_cm, levels = dbh_levels)),
      xlab = xlab,
      ylab = ylab,
      col_scale_name = "mean harvest diameter (cm)",
      var_to_plot = "volume"
    )
  }

  # Transform the data and combine the information required for a plot given
  # the user's choice of variable
  plot_info <- dplyr::case_when(
    variable == "area" ~ make_plot_info_1(
      dat_ = x$sim_areas$areas,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("area (", x$concept$units["area"], ")")
    ),

    variable == "vol_standing" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_standing,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("standing volume (", x$concept$units["volume"], ")")
    ),

    variable == "vol_inc_ups" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_inc_ups,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("volume increment (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "vol_rmv_total" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_rmv_total,
      make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("total removal volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "vol_rmv_cont" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_rmv_cont,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("regular harvest volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "vol_rmv_damage" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_rmv_damage,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("damage harvest volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "vol_mort" ~ make_plot_info_1(
      dat_ = x$sim_growth_and_yield$gyield_phases$vol_mort,
      phase_levels = make_phase_levels(x),
      variable = variable,
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("mortality volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "hrvst_det_reg" ~ make_plot_info_2(
      dat_ = x$sim_growth_and_yield$gyield_harvest_detail |>
        dplyr::filter(harvest_type == "regular"),
      dbh_levels = make_dbh_levels(x, variable),
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("regular harvest volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    ),

    variable == "hrvst_det_dam" ~ make_plot_info_2(
      dat_ = x$sim_growth_and_yield$gyield_harvest_detail |>
        dplyr::filter(harvest_type == "damage"),
      dbh_levels = make_dbh_levels(x, variable),
      xlab = paste0("time (", x$concept$units["time"], ")"),
      ylab = paste0("damage induced harvest volume (",
                    x$concept$units["volume"], "/", x$concept$units["time"],
                    ")")
    )
  ) # case_when()


  # The actual plot
  plot_info$dat |>
    ggplot() +
      geom_area(aes(x = .data$time, y = .data[[plot_info$var_to_plot]],
                    fill = .data$phase)) +
      scale_fill_viridis_d(name = plot_info$col_scale_name) +
      xlab(plot_info$xlab) +
      ylab(plot_info$ylab)

}



