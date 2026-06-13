# Amusement Park Simulation

This repository contains the code and configuration files used to simulate an amusement park as a stochastic service system. The model includes visitor arrivals, finite queues, ride capacities, balking, reneging, ride choice behaviour and disruption scenarios.

## Files

- `escenarios.yaml`  
  Configuration file with the simulation parameters and scenarios. It defines the rides, arrival settings, visitor behaviour, queue rules and operational scenarios.

- `simulation_park.R`  
  Main simulation script. It reads the YAML configuration file, builds the amusement park model, runs the simulations and generates the main output tables.

- `plots_analysis.R`  
  Analysis and plotting script. It uses the results from the simulation to create summary tables, compare scenarios and generate the figures used in the written document.

## How to run

First, make sure the three files are in the same folder. Then run:

```r
source("simulation_park.R")
source("simulation_park.R")
