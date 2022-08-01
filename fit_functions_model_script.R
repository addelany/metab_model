source(paste0(current_dir,"/main_metab_function_v3_test_model_script.R"), echo = FALSE)
# source("~/Documents/masters_work/model/metab_model/diagnostic_plots.R", echo = FALSE)
# 
# 
# 
# # ####### MODFIT TEST ####
# start_time <- Sys.time() #FIGURE OUT A WAY TO KEEP TRACK OF HOW MANY ITERATIONS IT TOOK
# ## FIGURE OUT HOW TO SET LIMIT ON ITERATIONS
# print('Fitting model...')
# 
# fit_model <- modFit(f=get_residuals,
#                    p=param_guess,
#                    driving_data = lake_data_full,
#                    config = input_param_df,
#                    lake_morph = lake_morphology,
#                    lake_obs = obs_df,
#                    method = 'Marq',
#                    lower = c(0.00001,0.01),
#                    upper = c(0.001,1))
# 
# end_time <- Sys.time()
# print(end_time - start_time)
# 
# input_param_df$ip <- fit_test$par[1]
# input_param_df$resp_sed_hypo <- fit_test$par[2]
# 
# model_check <- metab_model(lake_data_full,input_param_df,lake_morphology)
# model_plots <- make_diagnostic_plot(model_check,lake_data_full,obs_df)


get_residuals <- function(param_guess,driving_data,config,lake_morph,lake_obs){
  config$ip <- param_guess[1]
  #config$resp_sed_hypo <- param_guess[2]
  #config$theta_resp <- param_guess[3]
  #config$theta_npp <- param_guess[3]
  config$resp_poc_l <- param_guess[2]
  #config$k_pocl_epi <- param_guess[3]
  #config$resp_sed_hypo <- param_guess[3]
  config$resp_doc_l <- param_guess[3]
  #config$theta_npp <- param_guess[5]

  model_output <- metab_model(driving_data,config,lake_morph)
  
  sampledate <- driving_data$sampledate
  epi_vars_output <- cbind(sampledate,(model_output[[1]]/driving_data$volume_epi))
  hypo_vars_output <- cbind(sampledate,(model_output[[2]]/driving_data$volume_hypo))
  
  #make individual vectors of residuals to be combined later
  do_obs_epi <- lake_obs[[1]] #%>% select(sampledate,do_epi)
  do_obs_hypo <- lake_obs[[2]] #%>% select(sampledate,do_hypo)
  doc_obs_epi <- lake_obs[[3]] #%>% select(sampledate,doc_epi)
  doc_obs_hypo <- lake_obs[[4]] #%>% select(sampledate,doc_hypo)
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
  #print(length(do_epi_res))
  #print(length(do_hypo_res))
  #print(length(doc_epi_res))
  #print(length(doc_hypo_res))
  #print(length(secchi_res))
  all_res <- c(do_epi_res,do_hypo_res,doc_epi_res,doc_hypo_res,secchi_res)
  #df_list <- list(do_epi_df,do_hypo_df,doc_epi_df,doc_hypo_df,secchi_df)
  return(all_res)
  }

#inital model run to calculate residuals -- find best pararameters 
#residual_calc <- get_residuals(driving_data = lake_data_full, config = input_param_df, lake_morph = lake_morphology, lake_obs = obs_df,)

if(nhdid %in% c('nhdhr_69886156')){ #AL
  param_range_lower = c(0.04,0.02,1)
  param_range_upper = c(0.05,0.03,2)
  print('using AL alternate parameter ranges')
}else if (nhdid %in% c('nhdhr_69886444','nhdhr_69886284')){ #SP, BM
  param_range_lower = c(0.04,0.02,1.5)
  param_range_upper = c(0.045,0.03,2)
  
  #param_range_lower = c(0.04,0.02,1.5) #BM?
  #param_range_upper = c(0.05,0.03,2)
  print('using SP,BM alternate parameter ranges')
}else{
  # param_range_lower = c(0.005,0.003,1)
  # param_range_upper = c(0.05,0.03,2)
   #param_range_lower = c(0.04,0.07,0.01) #new parameter test
   #param_range_upper = c(0.05,0.15,0.03)
  #param_range_lower = c(0.04,0.05,0.005) #new parameter test
  #param_range_upper = c(0.05,0.15,0.035)
  param_range_lower = c(0.01,0.02,0.005) #new parameter test
  param_range_upper = c(0.1,0.2,0.05)
  print('using ME/MO/TR ranges')
}

# ####### RUN FITTING ####
#start_time <- Sys.time() #FIGURE OUT A WAY TO KEEP TRACK OF HOW MANY ITERATIONS IT TOOK
## FIGURE OUT HOW TO SET LIMIT ON ITERATIONS
#print('Fitting model...')
#print('Finding starting parameters...')
#p1_guess <- runif(100,0.01,0.1)[1]
#p2_guess <- runif(100,0.02,0.2)[1]
#p3_guess <- runif(100,0.005,0.05)[1]

#param_guess <- c(p1_guess,p2_guess,p3_guess)
#print(param_guess)

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

#model_sp <- summary(fit_model)
#model_sp[]
#covar_out <- model_sp$cov.scaled * 2.4^2/2
  
#print('Parameters found, running MCMC...')


#pars_df_list <- list()

#for (i in seq.int(1:1)){ #want to run three chains
  #print(paste('Starting iteration: ',i))
  
  ##randomized parameter guesses from uniform distribution
  #p1_guess <- runif(100,0.01,0.1)[1]
  #p2_guess <- runif(100,0.02,0.2)[1]
  #p3_guess <- runif(100,0.005,0.05)[1]
  
  #param_guess <- c(p1_guess,p2_guess,p3_guess)
  #print(param_guess)
  print('starting mcmc run...')
  fit_time <- Sys.time()
  #print(fit_time - start_time)
  
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
                      #lower = c(0.00005,0.2,1.04),
                      #upper = c(0.005,0.4,1.15)) #0.001
                      #upper = c(0.005,0.4,1.15))
                      # lower = c(0.0005,0.2,1.04,0.2,0.3),
                      # upper = c(0.01,0.4,1.15,2,1))
                      # lower = c(0.001,0.2,0.003,1),#,0.1),
                      # upper = c(0.05,0.4,0.03,2))#,1))
                      # lower = c(0.005,0.003,1),
                      # upper = c(0.05,0.03,2))#,1))
                      lower = param_range_lower,
                      upper = param_range_upper)
  
  ##update list with pars info
  #pars_df_list <- append(pars_df_list,list(mcmc_run$pars))
  
  end_time <- Sys.time()
  
  #print(paste(i,'Chains Done...'))
  print('Iteration Time: ', end_time - fit_time)
  #print(end_time - fit_time)
  print('--------------------')
  #print('Total time...')
  #print(end_time - start_time)
  
#}

#fit_time <- Sys.time()
#print(fit_time - start_time)

#mcmc_run <- modMCMC(f=get_residuals,
                    #p=fit_model$par,
#                    p = param_guess,
#                    var0 = 1,#model_sp$modVariance, # revisit this parameter
#                    jump = c(0.005,0.01,0.001),#NULL,#covar_out,
#                    niter = 1000,
#                    driving_data = lake_data_full,
#                    config = input_param_df,
#                    lake_morph = lake_morphology,
#                    lake_obs = obs_df,
                    #lower = c(0.00005,0.2,1.04),
                    #upper = c(0.005,0.4,1.15)) #0.001
                    #upper = c(0.005,0.4,1.15))
                    # lower = c(0.0005,0.2,1.04,0.2,0.3),
                    # upper = c(0.01,0.4,1.15,2,1))
                    # lower = c(0.001,0.2,0.003,1),#,0.1),
                    # upper = c(0.05,0.4,0.03,2))#,1))
                    # lower = c(0.005,0.003,1),
                    # upper = c(0.05,0.03,2))#,1))
#                    lower = param_range_lower,
#                    upper = param_range_upper)

#end_time <- Sys.time()

#print('MCMC finished...')
#print(end_time - fit_time)
#print('--------------------')
#print('Total time...')
#print(end_time - start_time)


# fit_model <- modFit(f=get_residuals,
#                     p=param_guess,
#                     driving_data = lake_data_full,
#                     config = input_param_df,
#                     lake_morph = lake_morphology,
#                     lake_obs = obs_df,
#                     method = 'Marq',
#                     lower = c(0.00005,0.01,1.04),
#                     upper = c(0.005,0.4,1.15)) #0.001

##try MCMC
# fit_model <- modMCMC(f=get_residuals,
#                     p=param_guess,
#                     var0 = 1, # revisit this parameter 
#                     driving_data = lake_data_full,
#                     config = input_param_df,
#                     lake_morph = lake_morphology,
#                     lake_obs = obs_df,
#                     lower = c(0.00005,0.2,1.04),
#                     upper = c(0.005,0.4,1.15)) #0.001

# 
# end_time <- Sys.time()
# print(end_time - start_time)
