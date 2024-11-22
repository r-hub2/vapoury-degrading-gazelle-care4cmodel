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




#' Constructor for the Class c4c_co2_result
#'
#' For internal use only. Objects of class c4c_co2_result are only constructed
#' by functions that evaluate simulation runs. There is no validator function,
#' for such objects, because their consistency is guaranteed by the functions
#' that build them.
#'
#' @param x An object to become a \code{c4c_co2_result}
#'
#' @return \code{x}, but with the class attribute \code{c4c_co2_result}
#'
#' @keywords internal
#'
new_c4c_co2_result <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "c4c_co2_result")
}




#' Check if an Object is of Class c4c_co2_result
#'
#' @param x Object to check
#'
#' @return \code{TRUE}, if \code{x} has class c4c_co2_result, \code{FALSE} if
#'   not
#'
#' @keywords internal
#'
is_c4c_co2_result <- function(x) {
  rslt <- FALSE
  if(inherits(x, "c4c_co2_result")) {
    rslt <- TRUE
  }
  rslt
}




