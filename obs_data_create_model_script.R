## this script is called in the "metabolism_model.R" script. The purpose of this script it to create tables that will hold observational data for model calibration and validation.


##TP 

source(paste0(current_dir,"/volume_weighted_avg_test_model_script.R"), echo = FALSE)


get_obs <- function(lake_df,lake_morph,nhd_id,obs_min_date,obs_max_date){

print('weighting and interpolating observational data...')

data_path <- paste(current_dir,'/data/',nhd_id,'/wq_data.csv',sep = '')
ntl_data_git <- read.csv(data_path, stringsAsFactors = FALSE)

##check if sloh values exist....if so use those
if('Total Phosphorous, unfiltered lab' %in% unique(ntl_data_git$CharacteristicName)){
  print('using SLOH TP data')
  ntl_data_git$ResultMeasureValue <- ifelse(ntl_data_git$CharacteristicName == 'Total Phosphorous, unfiltered lab',
                                            ntl_data_git$ResultMeasureValue*1000,ntl_data_git$ResultMeasureValue)
  tp_col <- 'Total Phosphorous, unfiltered lab'
}else{
  tp_col <- 'Total Phosphorous, unfiltered'
}

tp_obs <- ntl_data_git %>%
  filter(CharacteristicName == tp_col) %>%
  filter(ActivityStartDate >= obs_min_date) %>%
  filter(ActivityStartDate <= obs_max_date) %>%
  rename(sampledate = ActivityStartDate) %>% 
  rename(obs_value = ResultMeasureValue) %>% 
  rename(sampledepth = ActivityDepthHeightMeasure.MeasureValue) %>% 
  select(-CharacteristicName) %>% 
  distinct(sampledate,sampledepth, .keep_all = TRUE) %>% 
  filter(obs_value >= 0) %>% 
  filter(obs_value < quantile(obs_value,0.975, na.rm = TRUE)) %>% 
  select(sampledate,obs_value, sampledepth)
tp_obs$sampledate <- as.Date(tp_obs$sampledate)

tp_obs_approx <- interp_observations(tp_obs,lake_df,lake_morph)

tp_obs_epi_approx <- tp_obs_approx[[1]] %>% drop_na()
tp_obs_hypo_approx <- tp_obs_approx[[2]] %>% drop_na()

##TP 
tp_interp_build <- lake_df %>% distinct(sampledate)
tp_interp_setup_epi <- tp_interp_build %>% full_join(tp_obs_epi_approx, by = c('sampledate'))
###interpolate tp epi values 
tp_interp_epi <- zoo(tp_interp_setup_epi$obs_approx,tp_interp_setup_epi$sampledate)
tp_interp_epi <- na_interpolation(tp_interp_epi, option = 'linear')
tp_epi_df <- fortify(tp_interp_epi) %>% 
  rename(sampledate = Index, tp_epi_approx = tp_interp_epi)
interp_obs_tp <- full_join(tp_interp_build,tp_epi_df, by = 'sampledate')

###interpolate tp hypo values
tp_interp_setup_hypo <- tp_interp_build %>% full_join(tp_obs_hypo_approx, by = c('sampledate'))
tp_interp_hypo <- zoo(tp_interp_setup_hypo$obs_approx,tp_interp_setup_hypo$sampledate)
tp_interp_hypo <- na_interpolation(tp_interp_hypo, option = 'linear')
tp_hypo_df <- fortify(tp_interp_hypo) %>% 
  rename(sampledate = Index, tp_hypo_approx = tp_interp_hypo)
interp_obs_tp <- right_join(interp_obs_tp,tp_hypo_df, by = 'sampledate')

interp_obs_tp$tp_epi_approx <- ifelse(interp_obs_tp$tp_epi_approx < 1, 1, interp_obs_tp$tp_epi_approx)
interp_obs_tp$tp_hypo_approx <- ifelse(interp_obs_tp$tp_hypo_approx < 1, 1, interp_obs_tp$tp_hypo_approx)


do_obs <- ntl_data_git %>%
  filter(CharacteristicName == 'Dissolved oxygen (DO)') %>%
  filter(ActivityStartDate >= obs_min_date) %>%
  filter(ActivityStartDate <= obs_max_date) %>%
  rename(sampledate = ActivityStartDate) %>% 
  rename(obs_value = ResultMeasureValue) %>% 
  rename(sampledepth = ActivityDepthHeightMeasure.MeasureValue) %>% 
  select(-CharacteristicName) %>% 
  distinct(sampledate,sampledepth, .keep_all = TRUE) %>% 
  filter(obs_value >= 0) %>% 
  filter(obs_value < quantile(obs_value,0.975, na.rm = TRUE)) %>% 
  select(sampledate,obs_value, sampledepth)
do_obs$sampledate <- as.Date(do_obs$sampledate)

do_obs_approx <- interp_observations(do_obs,lake_df,lake_morph)

do_obs_epi_approx <- do_obs_approx[[1]] %>% drop_na() %>% rename(do_epi = obs_approx)
do_obs_hypo_approx <- do_obs_approx[[2]] %>% drop_na() %>% rename(do_hypo = obs_approx)
   
doc_obs <- ntl_data_git %>%
  filter(CharacteristicName == 'Dissolved organic carbon') %>%
  filter(ActivityStartDate >= obs_min_date) %>%
  filter(ActivityStartDate <= obs_max_date) %>%
  rename(sampledate = ActivityStartDate) %>% 
  rename(obs_value = ResultMeasureValue) %>% 
  rename(sampledepth = ActivityDepthHeightMeasure.MeasureValue) %>% 
  select(-CharacteristicName) %>% 
  distinct(sampledate,sampledepth, .keep_all = TRUE) %>% 
  filter(obs_value >= 0) %>% 
  filter(obs_value < quantile(obs_value,0.975, na.rm = TRUE)) %>% 
  filter(obs_value < 10) %>% 
  select(sampledate,obs_value, sampledepth)
doc_obs$sampledate <- as.Date(doc_obs$sampledate)

doc_obs_approx <- interp_observations(doc_obs,lake_df,lake_morph)

doc_obs_epi_approx <- doc_obs_approx[[1]] %>% drop_na() %>% rename(doc_epi = obs_approx)
doc_obs_hypo_approx <- doc_obs_approx[[2]] %>% drop_na() %>% rename(doc_hypo = obs_approx)

secchi_obs_data <- ntl_data_git %>%
  filter(CharacteristicName == 'Depth, Secchi disk depth') %>%
  filter(ActivityStartDate >= obs_min_date) %>%
  filter(ActivityStartDate <= obs_max_date) %>%
  rename(sampledate = ActivityStartDate) %>% 
  rename(secchi = ResultMeasureValue) %>% 
  select(-CharacteristicName) %>% 
  distinct(sampledate, .keep_all = TRUE) %>% 
  filter(secchi < quantile(secchi,0.975, na.rm = TRUE)) %>% 
  select(sampledate,secchi)
secchi_obs_data$sampledate <- as.Date(secchi_obs_data$sampledate)

return(list(do_obs_epi_approx,do_obs_hypo_approx,doc_obs_epi_approx,doc_obs_hypo_approx,secchi_obs_data,interp_obs_tp))
}