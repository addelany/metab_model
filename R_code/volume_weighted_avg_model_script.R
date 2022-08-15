## OBSERVATION VOLUME-WEIGHTED INTERPOLATION FUNCTION

interp_observations <- function(var_obs_df,lake_df,lake_morph){
  epi_obs_df <- data.frame(sampledate = as.Date(NA), obs_approx = NA)
  hypo_obs_df <- data.frame(sampledate = as.Date(NA), obs_approx = NA)
  
  for (d in seq(1,length(unique(var_obs_df$sampledate)))){
  
  interest_day <- unique(var_obs_df$sampledate)[d]
  td_x <- lake_df %>% filter(sampledate == interest_day) %>% select(thermocline_depth)
  td_x <- td_x$thermocline_depth
  td_area <- lake_df %>% filter(sampledate == interest_day) %>% select(area_td)
  td_area <- td_area$area_td
  sfc_area <- lake_morph$area_total
  depth_max <- lake_morph$maxdepth
  daily_obs <- var_obs_df %>% filter(sampledate == interest_day)

  if(nrow(daily_obs) == 1){ #check if there is only one observation for a given sampledate
    if (is.na(td_x) && (daily_obs$sampledepth > (lake_morph$maxdepth/2))){ #check if the only observation is in the top half of a given layer
      #dont save anything -- not indicative of the most water column mass
      next
    } else if (is.na(td_x) || daily_obs$sampledepth <= td_x){ #if lake is mixed or observation depth is less than thermocline depth
      epi_obs_df[d,'sampledate'] <- interest_day
      epi_obs_df[d,'obs_approx'] <- daily_obs$obs_value
      next
    } else if (!is.na(td_x) && daily_obs$sampledepth > td_x){ ##if lake is stratified and obs depth is greater than thermocline depth
      hypo_obs_df[d,'sampledate'] <- interest_day
      hypo_obs_df[d,'obs_approx'] <- daily_obs$obs_value
      next
    } 
  }
  if(is.na(td_x)){
    
    ### EPI ###
    epi_obs <- daily_obs #set df for relevant layer (epi, hypo, or entire wc)

    td_area <- 0 # no td for mixed conditions
    n_epi_depth <- round(depth_max)  #number of meters in wc, rounded
    area_approx_epi <- approx(c(sfc_area,td_area),n=n_epi_depth+1)[[2]] #approximate area at each point. Take second vector from output 
    
    area_interp_build_epi <- data.frame(sampledepth = (0:n_epi_depth),interp_area = area_approx_epi) #save information in df 
    
    depth_build <- area_interp_build_epi %>% select(sampledepth) #create platform to build obs df
    obs_build <- epi_obs %>% select(sampledepth,obs_value) #extract obs info
    
    obs_interp_setup_epi <- depth_build %>% full_join(obs_build, by = c('sampledepth'))
    
    obs_interp_epi <- zoo(obs_interp_setup_epi$obs_value,obs_interp_setup_epi$sampledepth)
    obs_interp_epi <- na_interpolation(obs_interp_epi, option = 'linear')
    obs_epi_df <- fortify(obs_interp_epi) %>% 
      rename(sampledepth = Index, obs_approx = obs_interp_epi)
    interp_obs <- full_join(depth_build,obs_epi_df, by = 'sampledepth')
    
    area_obs_approx <- area_interp_build_epi %>% right_join(interp_obs, by=c('sampledepth')) %>% drop_na()
    area_obs_approx$weighted_value <- area_obs_approx$interp_area * area_obs_approx$obs_approx #calculate weighted average
    
    approx_obs_value <- abs(trapz(area_obs_approx$sampledepth,area_obs_approx$weighted_value)) #integrate to get weighted average mass of water column
    layer_volume <- abs(trapz(area_obs_approx$sampledepth,area_obs_approx$interp_area)) #integrate to get total volume
    epi_approx_obs_value <- approx_obs_value/layer_volume #divide weighted average mass / weighted average volume to get concentration
    
  }else{
    
    ### EPI ####
    epi_obs <- daily_obs #set df for relevant layer (epi, hypo, or entire wc)

    n_epi_depth <- round(td_x)  #number of meters in EPI, rounded
    area_approx_epi <- approx(c(sfc_area,td_area),n=n_epi_depth+1)[[2]] #approximate area at each point. Take second vector from output. Add one to index to account for zero depth observations

    area_interp_build_epi <- data.frame(sampledepth = (0:n_epi_depth),interp_area = area_approx_epi) #save information in df 
    
    depth_build <- area_interp_build_epi %>% select(sampledepth) #create platform to build obs df
    obs_build <- epi_obs %>% select(sampledepth,obs_value) #extract obs info
    
    obs_interp_setup_epi <- depth_build %>% full_join(obs_build, by = c('sampledepth'))
    
    obs_interp_epi <- zoo(obs_interp_setup_epi$obs_value,obs_interp_setup_epi$sampledepth)
    obs_interp_epi <- na_interpolation(obs_interp_epi, option = 'linear')
    obs_epi_df <- fortify(obs_interp_epi) %>% 
      rename(sampledepth = Index, obs_approx = obs_interp_epi)
    interp_obs <- full_join(depth_build,obs_epi_df, by = 'sampledepth')
    
    area_obs_approx <- area_interp_build_epi %>% right_join(interp_obs, by=c('sampledepth')) %>% drop_na()
    area_obs_approx$weighted_value <- area_obs_approx$interp_area * area_obs_approx$obs_approx #calculate weighted average
    
    approx_obs_value <- abs(trapz(area_obs_approx$sampledepth,area_obs_approx$weighted_value)) #integrate to get weighted average mass of water column
    layer_volume <- abs(trapz(area_obs_approx$sampledepth,area_obs_approx$interp_area)) #integrate to get total volume
    epi_approx_obs_value <- approx_obs_value/layer_volume #divide weighted average mass / weighted average volume to get concentration
    
    #### HYPO ###
    hypo_obs <- daily_obs #set df for relevant layer (epi, hypo, or entire wc)

    n_hypo_depth <- length(round(td_x):round(depth_max))  #number of meters in wc, rounded
    area_approx_hypo <- approx(c(round(td_x),depth_max),n=n_hypo_depth)[[2]]
    
    area_interp_build_hypo <- data.frame(sampledepth = (round(td_x):depth_max),interp_area = area_approx_hypo) #save information in df 

    depth_build_hypo <- area_interp_build_hypo %>% select(sampledepth) #create platform to build obs df
    obs_build_hypo <- hypo_obs %>% select(sampledepth,obs_value) #extract obs info
    
    obs_interp_setup_hypo <- depth_build_hypo %>% full_join(obs_build_hypo, by = c('sampledepth'))
    
    obs_interp_hypo <- zoo(obs_interp_setup_hypo$obs_value,obs_interp_setup_hypo$sampledepth)
    obs_interp_hypo <- na_interpolation(obs_interp_hypo, option = 'linear')
    obs_hypo_df <- fortify(obs_interp_hypo) %>% 
      rename(sampledepth = Index, obs_approx = obs_interp_hypo)
    interp_obs_hypo <- full_join(depth_build,obs_hypo_df, by = 'sampledepth')
    
    area_obs_approx_hypo <- area_interp_build_hypo %>% right_join(interp_obs_hypo, by=c('sampledepth')) %>% drop_na()
    area_obs_approx_hypo$weighted_value <- area_obs_approx_hypo$interp_area * area_obs_approx_hypo$obs_approx #calculate weighted average
    
    approx_obs_value_hypo <- abs(trapz(area_obs_approx_hypo$sampledepth,area_obs_approx_hypo$weighted_value)) #integrate to get weighted average mass of water column
    layer_hypo_volume <- abs(trapz(area_obs_approx_hypo$sampledepth,area_obs_approx_hypo$interp_area)) #integrate to get total volume
    hypo_approx_obs_value <- approx_obs_value_hypo/layer_hypo_volume #divide weighted average mass / weighted average volume to get concentration
    
    

  hypo_obs_df[d,'sampledate'] <- interest_day
  hypo_obs_df[d,'obs_approx'] <- hypo_approx_obs_value
  }
  #update epi values regardless of mixing status
  epi_obs_df[d,'sampledate'] <- interest_day
  epi_obs_df[d,'obs_approx'] <- epi_approx_obs_value
  
  }
  approx_tables <- list(epi_obs_df,hypo_obs_df)
  return(approx_tables)
}