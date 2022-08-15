# Summary
This repository includes code, data, and configuration files for running a 1-D aquatic ecosystem model for the purpose of studying organic carbon dynamics in north-temperate lakes. The model was run for six lakes on a daily timestep from 1995-2014, over two layers (surface layer and bottom layer). 

The output for the model contains layer-specific predictions for state variables including recalcitrant and labile dissolved organic carbon (DOC_R, DOC_L), recalcitrant and labile particulate organic carbon (POC_R, POC_L), and dissolved oxygen (DO). Physical and metabolism fluxes, which are used to calculate state variables, are generated and tracked for each time step as well. All state variables and fluxes are in mass units (grams).

All data were obtained through open-access online repositories (NTL-LTER, USGS).

# Contents
The six lakes are organized by their respective identifiers from the high resolution National Hydrography Dataset (NHD):
* nhdhr_143249470 = Lake Mendota
* nhdhr_143249640 = Lake Monona
* nhdhr_69886156 = Allequash Lake
* nhdhr_69886228 = Trout Lake
* nhdhr_69886284 = Big Muskellunge Lake
* nhdhr_69886444 = Sparkling Lake

More information on each lake can be found here: https://lter.limnology.wisc.edu/about/lakes

### R_code
The R_code folder contains all scripts needed to run the model, calculate uncertainty, and visualize the model results. To run the model, 
* `run_model_script.R`: 

### config_files
The config_files folder contains model configuration information that is specific for each lake. These files include:
* `*_config_fil.csv` = file for lake parameter configuration

* `*.nml` = nml file for lake specific information such as morphometry and coordinate information

* `hydro_inputs.csv` / `hydro_outputs.csv` = derived hydrology information that is specific for each lake (not included for Lake Monona)

* `me_outflows.csv` = model output from Lake Mendota (used as hydrology input only for Lake Monona) 


### data
The data folder contains input data used for running the model and observational data for each lake. The derived temperature profiles and the meteorology data was taken from the USGS data product: Process-based predictions of lake water temperature in the Midwest US. These files include:
 * `pball_*_temperatures.csv`: derived temperature profiles from the USGS data product
 
 * `pball_*_ice_flag.csv` : flags for the presence or absence of ice on lake surface 

 * `NLDAS_*.csv`: meteorology data used to drive the model

 * `wq_data.csv`: water quality data from NTL-LTER used for model calibration and validation
 
 
