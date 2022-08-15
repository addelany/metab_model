#### model for carbon and do dynamics ###
##author: Austin Delany


#packages
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(tidyverse)
library(RPostgreSQL)
library(pracma)
library(ggplot2)

library(stats)
library(zoo)
library(lubridate)
library(solrad)
library(imputeTS)
library(ggmcmc)
remotes::install_github("jsta/nml")
library(nml)

install.packages('FME')
library(FME)

################################################################
# Open the database connection
drv <- dbDriver("PostgreSQL") ##this is where you call the name of your ODBC connection

##create connection (I use the server id here but the url name works too)
con <- dbConnect(drv,
                 dbname = db_name,
                 host = host_name,
                 port = port_number,
                 user = user_name,
                 password = pswrd)
#################################################################

current_dir <- getwd()

########## GRAB DATA #####################

##grab data from pgdl output and wqp data 
nhdid = 'nhdhr_143249470' #mendota
#nhdid = 'nhdhr_143249640' #monona
#nhdid = 'nhdhr_69886228' #trout
#nhdid = 'nhdhr_69886156' #allequash
#nhdid = 'nhdhr_69886284' #big muskie
#nhdid = 'nhdhr_69886444' #sparkling
#nhdid = 'nhdhr_69886510' #crystal

min_date <- '1995-01-01'
max_date <- '2014-12-31'

#temperature data query
data_query <- paste('select * from data.predicted_temps_calibrated where "nhd_lake_id" = \'',nhdid,'\' and date >= \'',min_date,'\'',' and date <= \'',max_date,'\'', sep = '')

#lake metric data query
shape_query <- paste('select * from data.lake_metrics where "nhd_lake_id" = \'',nhdid,"'", sep = '')

#ice flag data query
ice_query <- paste('select * from data.ice_flags_calibrated where "nhd_lake_id" = \'',nhdid,'\' and date >= \'',min_date,'\'',' and date <= \'',max_date,'\'', sep = '')

lake_data <- dbGetQuery(con,data_query, stringsAsFactors = FALSE)
lake_morphology <- dbGetQuery(con,shape_query, stringsAsFactors = FALSE)

lake_ice_flags <- dbGetQuery(con,ice_query, stringsAsFactors = FALSE)
lake_ice_flags <- lake_ice_flags %>% rename(sampledate = date) %>% select(-nhd_lake_id)
lake_ice_flags$sampledate <- as.Date(lake_ice_flags$sampledate)

#### Grab meteorolgy data
#find met file first
met_nhd_match_query <- paste('select "meteo_filename" from data.nhd_met_relation where "nhdid" = \'', nhdid,"\'",sep = '')
metfile <- dbGetQuery(con,met_nhd_match_query, stringsAsFactors = FALSE)
metfile <- metfile$meteo_filename

met_query <- paste('select * from data.met_input_data where "meteofile" = \'', metfile,"\' and \"time\" >= \'",min_date,"\'"," and \"time\" <= \'",max_date,"\'",sep = '')
met_data <- dbGetQuery(con,met_query, stringsAsFactors = FALSE)

dbDisconnect(con)

# GRAB .nml FILE 
lake_nml <- read_nml(paste0(current_dir,'/configuration_files/',nhdid,'/',nhdid,'.nml'))

##################### USER DEFINED PARAMETERS ####################

lake_config_file <- read.csv(paste0(current_dir,'/configuration_files/',nhdid,'/',nhdid,'_config_file','.csv'),stringsAsFactors = FALSE)

########################################################################################################################################################################################################
########################################################################################################################################################################################################
########################################################################################################################################################################################################


input_param_df <- lake_config_file
 
########################DATA FORMATTING####################


##FORMAT DATES
lake_date_fix <- lake_data %>%
  mutate(sampledate = date) %>% 
  separate(date, sep= "-", into = c("year", "month", "day"))

lake_date_fix$year <- as.numeric(lake_date_fix$year)
lake_date_fix$month <- as.numeric(lake_date_fix$month)
lake_date_fix$day <- as.numeric(lake_date_fix$day)
lake_date_fix$sampledate <- as.Date(lake_date_fix$sampledate)
lake_date_fix <- lake_date_fix %>% arrange(sampledate)


#####READ IN THERMOCLINE DEPTH ASSIGNMENT CODE#########
source(paste0(current_dir,"/td_assign_model_script.R"), echo = FALSE)

data = lake_data ##need original data format
p.data <- c()

for (ki in sort(unique(data$date))){
  yi <- which(data$date == ki)
  p.data <- rbind(p.data, rev(data$Value[yi]))
}

data2 <- as.data.frame(cbind(rep(1, length(unique(data$date))), p.data))
colnames(data2) <- c('date',paste0('temp_',sort(unique(data$Depth))))
data2$date = as.Date(sort(unique(data$date)))


td_df <- data.frame(calc_td_depth(data2)) ##list of td values
td_df$sampledate <- as.Date(sort(unique(data$date)))


td_assign <- lake_date_fix %>% right_join(td_df, by = c('sampledate')) %>% rename(thermocline_depth = calc_td_depth.data2.)  ## rejoin onto data with corrected dates


dates_store <- td_assign %>% 
  distinct(sampledate, .keep_all = TRUE) %>% 
  select(sampledate,nhd_lake_id,thermocline_depth)

avg_wtrcol_temp <- td_assign %>% 
  group_by(sampledate) %>% 
  drop_na(Value) %>% 
  mutate(watercol_avg_temp = mean(Value)) %>% 
  distinct(sampledate, watercol_avg_temp) %>% 
  ungroup()

epi_temp_avg <- td_assign %>% 
  group_by(sampledate) %>% 
  filter(Depth < thermocline_depth) %>% 
  drop_na(Value) %>% 
  mutate(epi_temp = mean(Value)) %>% 
  distinct(sampledate,epi_temp) %>% 
  ungroup()


hypo_temp_avg <- td_assign %>% 
  group_by(sampledate) %>% 
  filter(Depth > thermocline_depth) %>% 
  drop_na(Value) %>% 
  mutate(hypo_temp = mean(Value)) %>% 
  distinct(sampledate,hypo_temp) %>% 
  ungroup()

temp_driver_epi <- dates_store %>% full_join(epi_temp_avg, by=c('sampledate'))

temp_driver_hypo <- temp_driver_epi %>% 
  full_join(hypo_temp_avg, by=c('sampledate'))

temp_driver_full <- temp_driver_hypo %>% 
  full_join(avg_wtrcol_temp, by=c('sampledate')) %>% 
  arrange(sampledate)

#add layer temps for non-stratified periods
temp_driver_full$epi_temp <- ifelse(is.na(temp_driver_full$thermocline_depth), temp_driver_full$watercol_avg_temp, temp_driver_full$epi_temp)
temp_driver_full$hypo_temp <- ifelse(is.na(temp_driver_full$thermocline_depth), temp_driver_full$watercol_avg_temp, temp_driver_full$hypo_temp)
temp_driver_full <- temp_driver_full %>% select(-watercol_avg_temp)

######CALCUALTE VOLUMES AND AREAS

#area and volume functions
interp_area <- function(td_x){
  idx_find <- findInterval(td_x, lake_depth)
  pair1 <- morph_df[idx_find,]
  pair2 <- morph_df[idx_find+1,]
  
  y1 = as.numeric(pair1[,'lake_area'])
  y2 = as.numeric(pair2[,'lake_area'])
  
  x1 = as.numeric(pair1[,'lake_depth'])
  x2 = as.numeric(pair2[,'lake_depth'])
  
  y_interp = y1 + ((y2-y1)/(x2-x1))*(td_x - x1)
  
  return(y_interp)
}  

interp_volume <- function(td_x){
  idx_find <- findInterval(td_x, lake_depth)
  pair1 <- morph_df[idx_find,]
  pair2 <- morph_df[idx_find+1,]
  
  y1 = as.numeric(pair1[,'lake_area'])
  y2 = as.numeric(pair2[,'lake_area'])
  
  x1 = as.numeric(pair1[,'lake_depth'])
  x2 = as.numeric(pair2[,'lake_depth'])
  
  y_interp = y1 + ((y2-y1)/(x2-x1))*(td_x - x1)
  
  volume_calc_df <- morph_df[1:idx_find,]
  volume_calc_df <- rbind(volume_calc_df,c(td_x,y_interp))
  #print(volume_calc_df)
  
  volume_layer <- abs(trapz(volume_calc_df[,'lake_depth'],volume_calc_df[,'lake_area']))
  return(volume_layer)
}  


lake_area <- rev(lake_nml$morphometry$A) # AREA
lake_depth <- rev(abs(lake_nml$morphometry$H - max(lake_nml$morphometry$H))) # DEPTH

morph_df <- data.frame(lake_depth,lake_area)

total_area <- lake_area[1]
total_volume <- abs(trapz(morph_df[,'lake_depth'],morph_df[,'lake_area']))
extra_morph_data <- data.frame(area_total = total_area, volume_total = total_volume)
lake_morphology <- cbind(lake_morphology,extra_morph_data) #to be used as an input df for the model
lake_morphology$area <- total_area

areas <- c()
volumes <- c()

#interp volumes and areas for each point
dat_driving <- temp_driver_full
for (row in 1:nrow(dat_driving)){
  td_input <- as.numeric(dat_driving[row,"thermocline_depth"])

  if(!is.na(td_input)){
    a = interp_area(td_input)
    areas <- append(areas,a)
    
    v = interp_volume(td_input)
    volumes <- append(volumes,v)
  }
  else{
    areas <- append(areas,total_area)
    volumes <- append(volumes,total_volume)
    
  }
  
}

lake_data_full <- cbind(temp_driver_full,areas,volumes)

lake_data_full <- lake_data_full %>% rename(area_td = areas, volume_epi = volumes)

##FILL IN HYPO VOLUME VALUES 
lake_data_full$volume_hypo <- (total_volume - lake_data_full$volume_epi)


######## BUILD FINAL DRIVING DF ########
source(paste0(current_dir,"/obs_data_create_model_script.R"), echo = FALSE)
obs_df <- get_obs(lake_data_full,lake_morphology,nhdid,min_date,max_date)
interp_obs <- obs_df[[6]]

## noramlize tp obs
interp_obs$tp_epi_approx <- (interp_obs$tp_epi_approx - min(interp_obs$tp_epi_approx))/(max(interp_obs$tp_epi_approx) - min(interp_obs$tp_epi_approx))
interp_obs$tp_hypo_approx <- (interp_obs$tp_hypo_approx - min(interp_obs$tp_hypo_approx))/(max(interp_obs$tp_hypo_approx) - min(interp_obs$tp_hypo_approx))

#Join phosphorus values onto driving data and set correct units
lake_data_full <- right_join(lake_data_full, interp_obs, by = "sampledate")

elevation_value <- max(lake_nml$morphometry$H)
lake_data_full$do_sat_value <- (o2.at.sat.base(lake_data_full$epi_temp,altitude = elevation_value))*lake_data_full$volume_epi #should have value for each day (even winter)

#join ice flags
lake_data_full <- right_join(lake_data_full,lake_ice_flags, by = 'sampledate')

##add gas exchange values that incorporate windspeed
lake_data_full$gas_exchange_depth <- k.cole.base(met_data$WindSpeed)
lake_data_full$gas_exchange_depth <- k600.2.kGAS.base(lake_data_full$gas_exchange_depth,lake_data_full$epi_temp,gas="O2") #*0.5

##add solar radiation values 
lake_data_full$doy <- as.numeric(strftime(lake_data_full$sampledate, format = "%j"))

lake_data_full$i_rad <- met_data$ShortWave #taken from Jordan's data
lake_data_full <- lake_data_full %>% select(-doy)


###calculate inflow/outflow values

if(nhdid %in% c('nhdhr_143249470')){ #'nhdhr_143249640'
  
  lake_inflow <- read.csv(paste0(current_dir,'/configuration_files/',nhdid,'/','hydro_inputs','.csv'),stringsAsFactors = FALSE)

  lake_inflow$Date <- as.Date(lake_inflow$time)
  
  lake_inflow <- lake_inflow %>% filter(Date >= min_date & Date <= max_date)
  
  lake_data_full$inflow_vol <- lake_inflow$total_inflow_volume * 0.55
  lake_data_full$outflow_vol <- lake_inflow$total_inflow_volume * 0.55
  
}else if (nhdid %in% c('nhdhr_143249640')){
  lake_inflow <- read.csv(paste0(current_dir,'/configuration_files/nhdhr_143249470/hydro_inputs.csv'),stringsAsFactors = FALSE)
  
  lake_inflow$Date <- as.Date(lake_inflow$time)
  lake_inflow <- lake_inflow %>% filter(Date >= min_date & Date <= max_date)
  
  lake_data_full$outflow_vol <- lake_inflow$total_inflow_volume*0.55  #assumed to be equal to the MO inflow volume, 
                                                                #which is the same as the outflow volume of ME
                                                                
  
  #Mendota outflow 
  me_outflow <- read.csv(paste0(current_dir,'/configuration_files/nhdhr_143249640/me_outflows.csv'))
  #make col for each variable's inflow concentration 
  lake_data_full$pocl_inflow <- me_outflow$poc_l_out
  lake_data_full$pocr_inflow <- me_outflow$poc_r_out
  lake_data_full$docl_inflow <- me_outflow$doc_l_out
  lake_data_full$docr_inflow <- me_outflow$doc_r_out
  
  lake_data_full$inflow_vol <- NA # comes from mendota outflow -- inflow mass is used in this case

  
}else{ #for the northern lakes
  ##new inflow/outflow method
  lake_inflow <-  read.csv(paste0(current_dir,'/configuration_files/',nhdid,'/','hydro_inputs','.csv'),stringsAsFactors = FALSE)
  lake_outflow <-  read.csv(paste0(current_dir,'/configuration_files/',nhdid,'/','hydro_outputs','.csv'),stringsAsFactors = FALSE)
  
  lake_inflow$Date <- as.Date(lake_inflow$Date)
  lake_outflow$Date <- as.Date(lake_outflow$Date)
  
  lake_inflow <- lake_inflow %>% filter(Date >= min_date & Date <= max_date)
  lake_outflow <- lake_outflow %>% filter(Date >= min_date & Date <= max_date)
  
  if (nhdid %in% c('nhdhr_69886156')){ #AL
    
    lake_data_full$inflow_vol <- lake_inflow$Total_Inputs * 0.8
    lake_data_full$outflow_vol <- lake_outflow$Total_Outputs * 0.8
    
  }else if (nhdid %in% c('nhdhr_69886228')){ #TR
    
  lake_data_full$inflow_vol <- lake_inflow$Total_Inputs* 0.92
  lake_data_full$outflow_vol <- lake_outflow$Total_Outputs*0.92

  }else if (nhdid %in% c('nhdhr_69886284')){ #BM
    
    lake_data_full$inflow_vol <- lake_inflow$Total_Inputs* 1.25
    lake_data_full$outflow_vol <- lake_outflow$Total_Outputs*1.25
    
  }else if (nhdid %in% c('nhdhr_69886444')){ #SP
    lake_data_full$inflow_vol <- lake_inflow$Total_Inputs *0.88
    lake_data_full$outflow_vol <- lake_outflow$Total_Outputs*0.88

  }else{
  print('not a valid nhdid')
}
}

## Set DO initial conditions ##
input_param_df[1,'do_init'] <- as.numeric(o2.at.sat.base(lake_data_full$epi_temp[1]))*lake_data_full[1,'volume_epi']
input_param_df[1,'doc_r_init'] <- as.numeric(input_param_df[1,'doc_r_init'])*lake_data_full[1,'volume_epi']
input_param_df[1,'doc_l_init'] <- as.numeric(input_param_df[1,'doc_l_init'])*lake_data_full[1,'volume_epi']
input_param_df[1,'poc_r_init'] <- as.numeric(input_param_df[1,'poc_r_init'])*lake_data_full[1,'volume_epi']
input_param_df[1,'poc_l_init'] <- as.numeric(input_param_df[1,'poc_l_init'])*lake_data_full[1,'volume_epi']


## RUN THE MODEL
source(paste0(current_dir,"/main_metab_function_model_script.R"), echo = FALSE)
model_check <- metab_model(lake_data_full,input_param_df,lake_morphology) #rerun model again with optimized params

#MAKE PLOTS #
source(paste0(current_dir,"/diagnostic_plots_model_script.R"), echo = FALSE)
model_plots <- make_diagnostic_plot(model_check,lake_data_full,obs_df,input_param_df)


##calculate residuals
source("~/calculate_residuals_model_script.R", echo = FALSE)
residual_store <- res_df_list

## SAVE MODEL OUTPUT
sampledate <- lake_data_full$sampledate

##save model output
write.csv(cbind(sampledate,model_check[[1]]),paste0('~/output/',nhdid,'/epi_vars.csv'),row.names = FALSE)
write.csv(cbind(sampledate,model_check[[2]]),paste0('~/output/',nhdid,'/hypo_vars.csv'),row.names = FALSE)
write.csv(cbind(sampledate,model_check[[3]]),paste0('~/output/',nhdid,'/epi_fluxes.csv'),row.names = FALSE)
write.csv(cbind(sampledate,model_check[[4]]),paste0('~/output/',nhdid,'/hypo_fluxes.csv'),row.names = FALSE)
write.csv(cbind(sampledate,model_check[[5]]),paste0('~/output/',nhdid,'/conc_vars.csv', sep = ''),row.names = FALSE)
write.csv(lake_data_full,paste0('~/output/',nhdid,'/driving_info.csv', sep = ''),row.names = FALSE)

##save observational data output
write.csv(obs_df[[1]],paste0('~/output/',nhdid,'/obs_data/do_epi_obs.csv'),row.names = FALSE)
write.csv(obs_df[[2]],paste0('~/output/',nhdid,'/obs_data/do_hypo_obs.csv'),row.names = FALSE)
write.csv(obs_df[[3]],paste0('~/output/',nhdid,'/obs_data/doc_epi_obs.csv'),row.names = FALSE)
write.csv(obs_df[[4]],paste0('~/output/',nhdid,'/obs_data/doc_hypo_obs.csv'),row.names = FALSE)
write.csv(obs_df[[5]],paste0('~/output/',nhdid,'/obs_data/secchi_obs.csv'),row.names = FALSE)
write.csv(obs_df[[6]],paste0('~/output/',nhdid,'/obs_data/tp_interp_obs.csv'),row.names = FALSE)
