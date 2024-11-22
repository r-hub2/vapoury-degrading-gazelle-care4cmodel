## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7.15,
  fig.height = 4.5
)

set.seed(12) # Initialize random generator for constant output 

## ----quickstart, message = FALSE----------------------------------------------
library(care4cmodel) # Attach the package

# Run a simulation and store its base results in a variable sim_base_out
# call ?simulate_single_concept for details
sim_base_out <- simulate_single_concept(
    concept_def = pine_thinning_from_above_1, # use pre-defined concept
    init_areas  = c(800, 0, 0, 0, 0, 200),
    time_span   = 200,
    risk_level  = 3
)

# Evaluate the base results for carbon related information
# call ?fuel_and_co2_evaluation for details
carbon_out <- fuel_and_co2_evaluation(sim_base_out, road_density_m_ha = 35)

## ----quickstart_plot_base-----------------------------------------------------
# Plot base results. Without further specifications, this will generate a plot
# of the areas covered by the stand development phases over time

# Check the documentation with ?plot.c4c_base_result in order to see all 
# options for plotting, especially growth and yield variables
plot(sim_base_out) 


## ----quickstart_plot_carbon---------------------------------------------------
# Plot carbon related results. The selected option for plot_type generates a
# phase diagram where the total CO2 emissions due to forest operations are
# plotted over the CO2 sequestered by wood growth.

# Check the documentation with ?plot.c4c_co2_result in order to see all options
# for plotting
plot(carbon_out, plot_type = "em_vs_inc")


## ----setup--------------------------------------------------------------------
library(care4cmodel)

## ----example_concept----------------------------------------------------------
pine_thinning_from_above_1

## ----base_simulation----------------------------------------------------------
sim_base_out <- simulate_single_concept(
    concept_def = pine_thinning_from_above_1,
    init_areas  = c(1000, 0, 0, 0, 0, 0),
    time_span   = 200,
    risk_level  = 3
)

## ----show_base_output---------------------------------------------------------
names(sim_base_out)

## ----base_access_init_areas---------------------------------------------------
sim_base_out$init_areas

## ----names_sim_areas----------------------------------------------------------
names(sim_base_out$sim_areas)


## ----area_matrices------------------------------------------------------------
head(sim_base_out$sim_areas$areas)
head(sim_base_out$sim_areas$area_inflows_regular)
head(sim_base_out$sim_areas$area_outflows_events)

## ----sim_growth_and_yield-----------------------------------------------------
sim_base_out$sim_growth_and_yield

## ----plot_standing_volume-----------------------------------------------------
plot(sim_base_out, variable = "vol_standing") # standing volume


## ----plot_vol_rmv_total-------------------------------------------------------
plot(sim_base_out, variable = "vol_rmv_total") # total harvested volume


## ----fuel_and_co2, message = FALSE--------------------------------------------
# Calculate information about fuel consumption
# call documentation with ?fuel_and_co2_evaluation for detail information
carbon_out <- fuel_and_co2_evaluation(
  sim_base_out,             # object obtained from simulate_single_concept
  road_density_m_ha = 35,   # forest road density
  raw_density_kg_m3 = 520,  # default setting, wood density
  harvest_loss      = 0.1,  # default, share of volume lost at harvest
  bark_share        = 0.12, # default, bark share of volume
  mode  = "standard" # default, choice is between "standard" and "nordic"
)

## ----names_carbon_results-----------------------------------------------------
names(carbon_out)

## ----dataframes_carbon_results------------------------------------------------
carbon_out$co2_agg_high
carbon_out$co2_agg_medium
carbon_out$co2_by_phases

## ----plot_em_by_type----------------------------------------------------------
plot(carbon_out, plot_type = "em_by_type")

## ----plot_em_inc_ratio--------------------------------------------------------
plot(carbon_out, plot_type = "em_inc_ratio")

