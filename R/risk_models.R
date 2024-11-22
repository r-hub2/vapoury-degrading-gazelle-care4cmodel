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




#' Weibull-Based Estimates of Stand Survival
#'
#' Estimates the probability of a forest stand to survive a period t after its
#' establishment, based on the Weibull-Method published by Staupendahl (2011)
#' Forstarchiv 82, 10-19.
#'
#' The parameter \code{s_100} represents the survival probability after t = 100
#' years, and \code{alpha} is the shape parameter, indicating the risk profile
#' of the stand (type) of interest.
#'
#' @param t Time in years after stand establishment
#'
#' @param alpha Shape parameter. According to Staupendahl, Values < 1 indicate a
#'   high risk at young ages, alpha = 1 indicates an indifference of the risk to
#'   t (exponential distribution), values > 1 indicate high risk at old ages,
#'   where 1 < alpha < 2 means a degressively increasing, alpha = 2 a constantly
#'   increasing, and alpha > 2 a progressively increasing risk.
#'
#' @param s_100 Survival probability up to t = 100
#'
#' @return The probability to survive up to t = 100
#'
#' @export
#'
#' @examples
#'   # Calculations for Common oak, European beech, Norway spruce, Douglas fir,
#'   # and Scots pine with parameters after Staupendahl and Zucchini (AFJZ 2011)
#'   t <- seq(0, 120, 5)
#'   survival_weibull(t, alpha = 2.75, s_100 = 0.971) # oak
#'   survival_weibull(t, alpha = 1.76, s_100 = 0.967) # beech
#'   survival_weibull(t, alpha = 2.78, s_100 = 0.726) # spruce
#'   survival_weibull(t, alpha = 3.11, s_100 = 0.916) # Douglas
#'   survival_weibull(t, alpha = 2.45, s_100 = 0.923) # pine
#'
#'
survival_weibull <- function(t, alpha, s_100) {

  exp((t / 100)^alpha * log(s_100))
}




#' Calculate an Exponential Decay Rate From Two Appropriate Pairs of Values
#'
#' Assuming an exponential decay process y = exp(-r * t), this function
#' calculates r if the following informaion is given:\cr y_1 = exp(-r \* t_1),
#' y_2 = exp(-r \* t_2)\cr Hereby, t_1 is the earlier, t_2 the later point in
#' time. This implies the following conditions: t_2 > t_1, y_2 <= y_1\cr If
#' these conditions are not given, the function will terminate with an error.
#'
#' @param t_1 Earlier point in time, coupled to \code{y_1}
#'
#' @param t_2 Later point in time, coupled to \code{y_2}
#'
#' @param y_1 Earlier value, coupled to \code{t_1}
#'
#' @param y_2 Later value, coupled to \code{t_2}
#'
#' @return The exponential decay rate \code{r}, relating to the time unit of
#'   \code{t_1} and \code{t_2}
#'
#' @export
#'
#' @examples
#'   # Up to an age of t_1 = 30, a forest stand of interest has a survival
#'   # probability of 0.95. Up to an age of t_2 = 80, it has a survival
#'   # probability of 0.83. If we assume an exponential decay process for the
#'   # 50-year period, what is the exponential decay rate r?
#'   r <- exp_decay_rate(30, 80, 0.95, 0.83)
#'   print(r)
#'
#'   # Check it
#'   0.95 * exp(-r * (80 - 30)) # 0.83
#'
exp_decay_rate <- function(t_1, t_2, y_1, y_2) {

  stopifnot(t_2 > t_1, y_2 <= y_1)

  log(y_1 / y_2) / (t_2 - t_1)
}



#' setup_risk_events
#'
#' Low-level function for setting up a risk matrix for a simulation run.
#' Available for users who want to build simulation runs out of single elements.
#' Regular users are recommended to use the function
#' \code{\link{simulate_single_concept}} for running a simulation with one
#' single command (where this function is internally used).
#'
#' The function uses exponentially distributed random numbers (with expectation
#' = 1) for simulating the strenghth of damaging events. Such kind of
#' distribution where small events are much more frequent than strong ones is a
#' realistic assumption for forest damages. Such a random number is drawn for
#' each simulation point in time. The actual damage strength (i.e. relative area
#' loss) for a given subphase is then calculated as follows:\cr
#' \code{rel_area_loss = 1 - ((1 - x) ^ avg_event_strength) ^ event_strength},
#' \cr
#' where\cr
#' \itemize{
#'   \item{x: The baseline area loss risk of a given stand development
#'     subphase as resulting from the silvicultural concept definition of
#'     interest}
#'   \item{avg_event_strength: The user defined overall average event
#'     strenghth}
#'   \item{event_strength: Exponentially distributed random number with
#'     expectation 1, indicating the damage event strength in a given year}
#' }
#'
#' @param time_span Simulation time span to be covered (integer)
#'
#' @param avg_event_strength Number which indicates the average strength of a
#'   damage event in the simulation. Default is 1 which means that the survival
#'   probabilities as defined in the silvicultural concept of interest are
#'   applied exactly as they are. A value of 2 would mean that one damage event
#'   would have the same effect as would two subsequent events with normal
#'   strength. A value of 0 would trigger no damage events at all.
#'
#' @param area_risks Vector of subphase-wise baseline damage risks, contained in
#'   the list made with \code{\link{setup_parms}} under the name \code{risk}.
#'
#' @return A matrix where each row is a point in simulation time, and each
#'   column represents a subphase of the silvicultural concept of interest (in
#'   increasing order). Each matrix element describes the relative area loss
#'   that will happen at a given time to a given subphase.
#'
#' @export
#'
#' @examples
#'   parms <- setup_parms(pine_no_thinning_and_clearcut_1)
#'   setup_risk_events(time_span = 200,
#'                     avg_event_strength = 3,
#'                     area_risks = parms$risk)
#'
setup_risk_events <- function(time_span, avg_event_strength = 1, area_risks) {

  times <- 0:time_span
  event_strength <- stats::rexp(length(times), rate = 1)

  mat <- matrix(
    rep(area_risks, times = length(times)),
    ncol = length(area_risks),
    byrow = TRUE
  )

  help_fun <- function(x, estrngth, avg_estrngth) {
    1 - ((1 - x) ^ avg_estrngth) ^ estrngth
  }

  do.call(cbind,
          apply(
            mat,
            MARGIN = 2,
            FUN = help_fun,
            estrngth = event_strength,
            avg_estrngth = avg_event_strength,
            simplify = FALSE
          )
  )
}


#' apply_risk_events
#'
#' Internal function for calculating damage-induced area losses during
#' simulation
#'
#' @param t A point in time
#'
#' @param y Vector containing the current areas per subphase
#'
#' @param parms List of parameters as built with \code{\link{setup_parms}},
#'   importantly also including a matching risk matrix (list element
#'   \code{risk_mat}) as made with \code{\link{setup_risk_events}}.
#'
#' @param n_phases Number of (sub-) phases defined in the silvicultural concept
#'   to be simulated.
#'
#' @param i_act Index vector, must be \code{1:n_phases}, not built inside the
#'   functions for reasons of efficiency.
#'
#' @param ... Not used, required for the function to work with
#'   \code{link[deSolve]{ode}}
#'
#' @return Vector of the areas after the risk events at a given time
#'
#' @noRd
#'
apply_risk_events <- function(t, y, parms, n_phases, i_act, ...) {

  # risk_mat is the output of setup_risk_events
  # t + 1 because counting begins at 0
  area_losses <- y[i_act] * parms$risk_mat[t + 1, ]
  y[i_act]    <- y[i_act] - area_losses
  y[1]        <- y[1]     + sum(area_losses)

  # cumlulate risk induced outflows from each class
  y[(2 * n_phases + 1):(3 * n_phases)] <-
    y[(2 * n_phases + 1):(3 * n_phases)] + area_losses

  y
}





