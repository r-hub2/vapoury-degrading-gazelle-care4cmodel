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



#' Convert Wood Volume to CO2 Equivalents
#'
#' @param volume_m3 Wood volume to be converted in cubic meters
#'
#' @param raw_density_kg_m3 The raw density of the wood in kg/m³
#'
#' @param water_perc The water content of the wood at raw density in percent
#'   (usually defined as 12 %, i.e. air-dry)
#'
#' @return The CO2 equivalent of the input wood volume in kg
#'
#' @export
#'
#' @examples
#'   # Conversion of 1 m³ wood with typical values for important tree species
#'   wood_to_co2(1, raw_density_kg_m3 = 520) # Scots pine
#'   wood_to_co2(1, raw_density_kg_m3 = 470) # Norway spruce
#'   wood_to_co2(1, raw_density_kg_m3 = 600) # European larch
#'   wood_to_co2(1, raw_density_kg_m3 = 700) # sessile/pedunculate oak
#'   wood_to_co2(1, raw_density_kg_m3 = 730) # European beech
#'
#'
wood_to_co2 <- function(volume_m3, raw_density_kg_m3, water_perc = 12) {

  volume_m3 * raw_density_kg_m3 * (1 - water_perc / 100) / 2 * 3.67
}



#' Convert Fuel Consumption into CO2 Emission
#'
#' Simple conversion assuming a factor of 2.61 kg CO2 / l diesel fuel
#'
#' @param fuel_cons_ltrs Fuel amount consumed by a harvester in liters.
#'
#' @param fuel_type Fuel type (character string), required to find the correct
#'   conversion factor. Currently, only "diesel" is accepted.
#'
#' @return The emitted amount of CO2 in kg coming form burning
#'   \code{fuel_cons_ltrs}
#'
#' @export
#'
#' @examples
#'   dbh  <- seq(20, 70, 10) # Vector of tree dbh in cm
#'   vol  <- dbh ^ 2 / 1000  # Simple Volume estimate (m³) with Denzin's formula
#'
#'   fuel_cons_harvester_1(vol, dbh) |>
#'     fuel_to_co2()
#'
fuel_to_co2 <- function(fuel_cons_ltrs, fuel_type = c("diesel")) {

  fuel_type <- match.arg(fuel_type)

  ft_vec <- c(diesel = 2.61)
  unname(ft_vec[fuel_type] * fuel_cons_ltrs) # unname for removing fuel_type
}



#' Fuel Consumption of a Harvester per Cubic Meter Harvested Wood (Version #1)
#'
#' Fuel consumption depends on the average tree volume. For tree diameters at
#' breast height < 15 cm the function gives back \code{NA}, because the assumed
#' machine does not work with such small trees. Estimated after
#' \insertCite{Bacescu_et_al_2022;textual}{care4cmodel}.
#'
#' @param tree_vol Average standing merchandable wood volume over bark (m³) per
#'   harvested tree
#'
#' @param tree_dbh Average diameter at breast height (cm) per harvested tree
#'
#' @return Fuel consumption of a harvester in liters diesel fuel per m³
#'   harvested wood
#'
#' @references
#'   \insertRef{Bacescu_et_al_2022}{care4cmodel}
#'
#' @export
#'
#' @examples
#'   dbh <- seq(10, 70, 10) # Vector of tree dbh in cm
#'   vol <- dbh ^ 2 / 1000  # Simple Volume estimate (m³) with Denzin's formula
#'
#'   fuel_cons_harvester_1(vol, dbh)
#'
fuel_cons_harvester_1 <- function(tree_vol, tree_dbh) {

  purrr::map2_dbl(
    .x = tree_vol,
    .y = tree_dbh,

    .f = function(.x, .y) {
      if(.x > 0 & .y >= 15) {
        rslt <- 1 / (1.83382 + 0.641961 * log(.x))
      } else
        rslt <- NA
      rslt
    }
  )
}



#' Fuel Consumption of a Harvester per Cubic Meter Harvested Wood (Version #2)
#'
#' Fuel consumption of a harvester in liters diesel per m³ havested wood after
#' \insertCite{karha_et_al_2023;textual}{care4cmodel}.
#'
#' @param tree_vol Average standing merchandable wood volume over bark (m³) per
#'   harvested tree
#'
#' @param harvest_vol_ha Harvested merchandable wood volume over bark per ha
#'   (m³/ha)
#'
#' @param thinning Logical, TRUE (default) if the harvest is a thinning, or
#'   another kind of felling operation (FALSE)
#'
#' @return Fuel consumption of a harvester in liters diesel fuel per m³
#'   harvested wood
#'
#' @references
#'   \insertRef{karha_et_al_2023}{care4cmodel}
#'
#' @export
#'
#' @examples
#'   tree_vol    <- c(0.03,  0.10,  1.00,  2.00,  5.00)
#'   harvest_vol <- c(5.00, 10.00, 50.00, 25.00, 10.00)
#'
#'   fuel_cons_harvester_2(tree_vol, harvest_vol, TRUE)
#'   fuel_cons_harvester_2(tree_vol, harvest_vol, FALSE)
#'
fuel_cons_harvester_2 <- function(tree_vol,
                                  harvest_vol_ha,
                                  thinning = TRUE) {

  l <- list(tv = tree_vol,
            hv = harvest_vol_ha,
            th = thinning)

  purrr::pmap_dbl(
    .l = l,
    .f = function(tv, hv, th) {
      0.494 + 0.105 / tv + 9.501 / hv + 0.149 * th
    }
  )
}



#' Estimate Average Wood Extraction Distance From Forest Road Density
#'
#' @param frd Forest road density (truck roads) in m/ha
#'
#' @return The average extraction distance from the felling spot to the nearest
#'   landing at a truck road.
#'
#' @export
#'
#' @examples
#'   frd <- c(15, 30, 60, 100) # Forest road densities m/ha
#'   avg_extraction_distance(frd)
#'
avg_extraction_distance <- function(frd) {

  10000 / frd / 4
}



#' Fuel Consumption of a Forwarder per Cubic Meter Harvested Wood (Version #1)
#'
#' Fuel consumption per m³ harvested wood derived from the data provided by
#' \insertCite{grigolato_et_al_2022;textual}{care4cmodel}. Includes loading,
#' transportation, and unloading.
#'
#' @param aed Average extraction distance to the nearest truck road
#'
#' @return Fuel consumption of a forwarder in liters diesel fuel per m³ wood to
#'   be handled
#'
#' @references \insertRef{grigolato_et_al_2022}{care4cmodel}
#'
#' @export
#'
#' @examples
#'   frd <- c(15, 30, 60, 100) # Forest road densities m/ha
#'   avg_extraction_distance(frd) |>
#'     fuel_cons_forwarder_1()
#'
fuel_cons_forwarder_1 <- function(aed) {

  0.000324 * aed +  0.469384
}



#' Fuel Consumption of a Forwarder per Cubic Meter Harvested Wood (Version #2)
#'
#' Fuel consumption per m³ harvested wood after
#' \insertCite{karha_et_al_2023;textual}{care4cmodel}. Includes loading,
#' transportation, and unloading.
#'
#' @param aed Average extraction distance to the nearest truck road
#'
#' @param harvest_vol_ha Harvested merchandable wood volume over bark per ha
#'   (m³/ha)
#'
#' @param mineral_soil Logical, TRUE (default) if the operation takes place on
#'   mineral soil, FALSE if not
#'
#' @return Fuel consumption of a forwarder in liters diesel fuel per m³ wood to
#'   be handled
#'
#' @references \insertRef{karha_et_al_2023}{care4cmodel}
#'
#' @export
#'
#' @examples
#'   frd <- c(15, 30, 60, 100) # Forest road densities m/ha
#'   aed <- avg_extraction_distance(frd)
#'
#'   fuel_cons_forwarder_2(aed, 100, TRUE)
#'   fuel_cons_forwarder_2(aed, 100, FALSE)
#'
fuel_cons_forwarder_2 <- function(aed, harvest_vol_ha, mineral_soil = TRUE) {

  l <- list(aed = aed,
            hv  = harvest_vol_ha,
            min = mineral_soil)

  purrr::pmap_dbl(
    .l = l,
    .f = function(aed, hv, min) {
      0.516 + 0.049 * aed / 100 + 17.033 / hv - 0.106 * min
    }
  )
}



#' Annual Fuel Consumption for Truck Road Network Maintenance
#'
#' Estimate the annual diesel fuel consumption per ha for maintaining an
#' existing truck road network in the forest. Estimate based on
#' \insertCite{enache_stampfer_2015;textual}{care4cmodel}.
#'
#' @param frd Forest road density in m/ha
#'
#' @return Diesel fuel consumption for truck road network maintenance in
#'   l/ha/year
#'
#' @references
#'   \insertRef{enache_stampfer_2015}{care4cmodel}
#'
#' @export
#'
#' @examples
#'   frd <- c(15, 30, 60, 100)
#'   fuel_cons_road_maintenance(frd)
#'
fuel_cons_road_maintenance <- function(frd) {

  0.25 * frd
}








