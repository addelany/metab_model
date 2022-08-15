## script for running optimization (MCMC)


source(paste0(current_dir,"/main_metab_function_v3_test_model_script.R"), echo = FALSE)

get_residuals <- function(param_guess,driving_data,config,lake_morph,lake_obs){
  config$ip <- param_guess[1]
  config$resp_poc_l <- param_guess[2]
  config$resp_doc_l <- param_guess[3]

  model_output <- metab_model(driving_data,config,lake_morph)
  
  sampledate <- driving_data$sampledate
  epi_vars_output <- cbind(sampledate,(model_output[[1]]/driving_data$volume_epi))
  hypo_vars_output <- cbind(sampledate,(model_output[[2]]/driving_data$volume_hypo))
  
  #make individual vectors of residuals to be combined later
  do_obs_epi <- lake_obs[[1]]
  do_obs_hypo <- lake_obs[[2]]
  doc_obs_epi <- lake_obs[[3]]
  doc_obs_hypo <- lake_obs[[4]]
  secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
  
  do_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_epi,by='sampledate')
  do_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_hypo,by='sampledate') %>% filter(do_value != 0)
  doc_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_epi,by='sampledate')
  doc_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_hypo,by='sampledate') %>% filter(doc_value != 0)
  secchi_value <- 1.7/model_output[[5]]$lec_epi_value
  secchi_df <- cbind(sampledate,model_output[[5]]) %>% mutate(secchi = 1.7/lec_epi_value) %>% select(sampledate,secchi) %>% inner_join(secchi_obs,by='sampledate')
  
  do_epi_res <- do_epi_df$do_value - do_epi_df$do_epi
  do_hypo_res <- do_hypo_df$do_value - do_hypo_df$do_hypo
  
  doc_epi_res <- doc_epi_df$doc_value - doc_epi_df$doc_epi
  doc_hypo_res <- doc_hypo_df$doc_value - doc_hypo_df$doc_hypo
  
  #weight secchi obs differently depending on lake
  if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
    secchi_res <- (secchi_df$secchi - secchi_df$secchi_obs)
  }else{
    secchi_res <- (secchi_df$secchi - secchi_df$secchi_obs)/100
  }
  all_res <- c(do_epi_res,do_hypo_res,doc_epi_res,doc_hypo_res,secchi_res)
  return(all_res)
  }

#inital model run to calculate residuals -- find best pararameters 

if(nhdid %in% c('nhdhr_69886156')){ #AL
  param_range_lower = c(0.04,0.02,1)
  param_range_upper = c(0.05,0.03,2)
  print('using AL alternate parameter ranges')
}else if (nhdid %in% c('nhdhr_69886444','nhdhr_69886284')){ #SP, BM
  param_range_lower = c(0.04,0.02,1.5)
  param_range_upper = c(0.045,0.03,2)
  print('using SP,BM alternate parameter ranges')
}else{
  param_range_lower = c(0.01,0.02,0.005) 
  param_range_upper = c(0.1,0.2,0.05)
  print('using ME/MO/TR ranges')
}

print('starting initial fit...')
fit_model <- modFit(f=get_residuals,
                   p=param_guess,
                   driving_data = lake_data_full,
                   config = input_param_df,
                   lake_morph = lake_morphology,
                   lake_obs = obs_df,
                    method = 'Marq',
                    #lower = c(0.00005,0.2,1.04),
                    #upper = c(0.005,0.4,1.15))
                    # lower = c(0.0005,0.2,1.04,0.2,0.3),
                    # upper = c(0.01,0.4,1.15,2,1))
                    # lower = c(0.001,0.2,0.003,1),#,0.1), ## ip = c(0.0005,0.05)
                    # upper = c(0.05,0.4,0.03,2))#,1))
                    # lower = c(0.005,0.003,1),
                    # upper = c(0.05,0.03,2))#,1))
                   lower = param_range_lower,
                   upper = param_range_upper)

  print('starting mcmc run...')
  fit_time <- Sys.time()
  
  mcmc_run <- modMCMC(f=get_residuals,
                      p=fit_model$par,
                      #p = param_guess,
                      var0 = 1,#model_sp$modVariance, # revisit this parameter
                      jump = c(0.01,0.01,0.001),#NULL,#covar_out, # make IP jump 0.005 next time....bigger to run faster for now
                      niter = 1000,
                      driving_data = lake_data_full,
                      config = input_param_df,
                      lake_morph = lake_morphology,
                      lake_obs = obs_df,
                      lower = param_range_lower,
                      upper = param_range_upper)

  end_time <- Sys.time()
  
  print('Iteration Time: ', end_time - fit_time)
  print('--------------------')