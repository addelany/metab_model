source(paste0(current_dir,"/main_metab_function_v3_test_model_script.R"), echo = FALSE)


#input_param_df$ip <- param_guess[1]
#input_param_df$resp_poc_l <- param_guess[2]
#input_param_df$resp_doc_l <- param_guess[3]




get_residuals <- function(p_guess,driving_data,config,lake_morph,lake_obs,secchi_ma_indicate){
  config$ip <- p_guess[1]
  config$resp_poc_l <- p_guess[2]
  config$resp_doc_l <- p_guess[3]

  model_output <- metab_model(driving_data,config,lake_morph)
  
  sampledate <- driving_data$sampledate
  epi_vars_output <- cbind(sampledate,(model_output[[1]]/driving_data$volume_epi))
  hypo_vars_output <- cbind(sampledate,(model_output[[2]]/driving_data$volume_hypo))
  
  #make individual vectors of residuals to be combined later
  do_obs_epi <- lake_obs[[1]] #%>% select(sampledate,do_epi)
  do_obs_hypo <- lake_obs[[2]] #%>% select(sampledate,do_hypo)
  doc_obs_epi <- lake_obs[[3]] #%>% select(sampledate,doc_epi)
  doc_obs_hypo <- lake_obs[[4]] #%>% select(sampledate,doc_hypo)
  #secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
  
  if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
    secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
  }else{
    if (secchi_ma_indicate == 1){
    #### moving average of predicted secchi values
    secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
    
    secchi_obs$secchi_ma_obs <- rollmean(secchi_obs$secchi, k = 15, fill = NA)
    secchi_obs$secchi_ma_obs <- ifelse(is.na(secchi_obs$secchi_ma_obs), secchi_obs$secchi[1],secchi_obs$secchi_ma_obs)
    }else{
      secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
    }
  }
  
  do_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_epi,by='sampledate')
  do_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_hypo,by='sampledate') %>% filter(do_value != 0)
  doc_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_epi,by='sampledate')
  doc_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_hypo,by='sampledate') %>% filter(doc_value != 0)
  
  secchi_value <- 1.7/model_output[[5]]$lec_epi_value
  secchi_df <- cbind(sampledate,model_output[[5]]) %>% mutate(secchi = 1.7/lec_epi_value) %>% select(sampledate,secchi) %>% inner_join(secchi_obs,by='sampledate')
  
  if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
    secchi_res <- (secchi_df$secchi - secchi_df$secchi_obs)
  }else{
    #### moving average of predicted secchi values
    secchi_df$secchi_ma_predict <- rollmean(secchi_df$secchi, k = 15, fill = NA)
    secchi_df$secchi_ma_predict <- ifelse(is.na(secchi_df$secchi_ma_predict), secchi_df$secchi[1],secchi_df$secchi_ma_predict)
    
    secchi_res <- secchi_df$secchi_ma_predict - secchi_df$secchi_ma_obs
    
  }
  
  do_epi_res <- do_epi_df$do_value - do_epi_df$do_epi
  do_hypo_res <- do_hypo_df$do_value - do_hypo_df$do_hypo
  
  doc_epi_res <- doc_epi_df$doc_value - doc_epi_df$doc_epi
  doc_hypo_res <- doc_hypo_df$doc_value - doc_hypo_df$doc_hypo
  
  all_res <- c(do_epi_res,do_hypo_res,doc_epi_res,doc_hypo_res,secchi_res)
  
  return(all_res)
  
}


#### define dfs and variables for tracking info
#create df of initial predictions and sampledates for comparison (y_prime)
epi_do_main <- NA
hypo_do_main <- NA
epi_doc_main <- NA
hypo_doc_main <- NA
secchi_main <- NA



##create dfs to store res values (e_prime)

epi_do_res_master <- data.frame(matrix(ncol = nrow(obs_df[[1]]), nrow = 101))

hypo_do_res_master <- data.frame(matrix(ncol = nrow(obs_df[[2]]), nrow = 101))

epi_doc_res_master <- data.frame(matrix(ncol = nrow(obs_df[[3]]), nrow = 101))

hypo_doc_res_master <- data.frame(matrix(ncol = nrow(obs_df[[4]]), nrow = 101))

secchi_res_master <- data.frame(matrix(ncol = nrow(obs_df[[5]]), nrow = 101))


###define variable for tracking psuedo observation info -- updated every iteration
pseudo_obs_df <- NA

##make param table
param_df_master <- data.frame(ip_param = NA, resp_poc_l_param = NA, resp_doc_l_param = NA)


#### IDENTIFY PARAMETER BOUNDS
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
  param_range_upper = c(0.05,0.2,0.035)
  print('using ME/MO/TR ranges')
}



#################### RUN LOOP #################
init_start_time <- Sys.time()
#for (i in seq.int(101)){
for (i in seq.int(6)){  
  if (i == 1){
    #input_param_df$ip <- param_guess[1]
    #input_param_df$resp_poc_l <- param_guess[2]
    #input_param_df$resp_doc_l <- param_guess[3]
    
    start_time <- Sys.time() 
    print('Finding parameters using observational data...')
    
    fit_model <- modFit(f=get_residuals,
                        p = param_guess,
                        driving_data = lake_data_full,
                        config = input_param_df,
                        lake_morph = lake_morphology,
                        lake_obs = obs_df,
                        secchi_ma_indicate = 1,
                        method = 'Marq',
                        lower = param_range_lower,
                        upper = param_range_upper)
    
    print('Initial parameters found')
    fit_time <- Sys.time()
    print(fit_time - start_time)
    print('-----------------------')
    
    
    #update params with best fit results
    input_param_df$ip <- fit_model$par[1]
    input_param_df$resp_poc_l <- fit_model$par[2]
    input_param_df$resp_doc_l <- fit_model$par[3]
    
    #run model
    model_results <- metab_model(lake_data_full,input_param_df,lake_morphology)
    
    #save output and calculate residuals 
    ##define DFs to use
    
    epi_df_init <- cbind(lake_data_full,model_results[[1]]) %>% 
      left_join(obs_df[[1]],by = c('sampledate')) %>% #do_epi
      left_join(obs_df[[3]],by = c('sampledate')) #doc_epi
    
    hypo_df_init <- cbind(lake_data_full,model_results[[2]]) %>% 
      left_join(obs_df[[2]],by = c('sampledate')) %>% #do_hypo
      left_join(obs_df[[4]],by = c('sampledate')) #doc_hypo
    
    secchi_df_init <- cbind(lake_data_full,model_results[[5]]) %>% 
      left_join(obs_df[[5]],by = c('sampledate')) %>% #secchi
      mutate(secchi_value = (1.7/lec_epi_value)) 
    
    secchi_ma_indicator <- 1
    #val_date <- '2010-01-01'
    
    #epi 
    res_do_epi <- epi_df_init %>% 
      drop_na(do_epi) %>% #obs col
      mutate(do_conc = do_value/volume_epi) %>% 
      mutate(do_res = do_epi - do_conc) %>% 
      select(sampledate,do_conc,do_res)
    
    #res_do_epi_cal <- res_do_epi %>% filter(sampledate < val_date)
    #res_do_epi_val <- res_do_epi %>% filter(sampledate > val_date)
    
    res_doc_epi <- epi_df_init %>% 
      drop_na(doc_epi) %>% #obs col
      mutate(doc_conc = (doc_r_value + doc_l_value)/volume_epi) %>% 
      mutate(doc_res = doc_epi - doc_conc) %>% 
      select(sampledate,doc_conc,doc_res)
    
    #res_doc_epi_cal <- res_doc_epi %>% filter(sampledate < val_date)
    #res_doc_epi_val <- res_doc_epi %>% filter(sampledate > val_date)
    
    #hypo
    res_do_hypo <- hypo_df_init %>% 
      drop_na(do_hypo) %>% #obs col
      mutate(do_conc = do_value/volume_hypo) %>% 
      mutate(do_res = do_hypo - do_conc) %>% 
      select(sampledate,do_conc,do_res)
    
    #res_do_hypo_cal <- res_do_hypo %>% filter(sampledate < val_date)
    #res_do_hypo_val <- res_do_hypo %>% filter(sampledate > val_date)
    
    res_doc_hypo <- hypo_df_init %>% 
      drop_na(doc_hypo) %>% #obs col
      mutate(doc_conc = (doc_r_value + doc_l_value)/volume_hypo) %>% 
      mutate(doc_res = doc_hypo - doc_conc) %>% 
      select(sampledate,doc_conc,doc_res)
    
    #res_doc_hypo_cal <- res_doc_hypo %>% filter(sampledate < val_date)
    #res_doc_hypo_val <- res_doc_hypo %>% filter(sampledate > val_date)
    
    #secchi
    secchi_obs_predict <- secchi_df_init %>% drop_na(secchi)
    
    if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
      res_secchi <- secchi_obs_predict %>% 
        #drop_na(secchi) %>% 
        mutate(secchi_res = secchi - secchi_value) %>% 
        select(sampledate,secchi_value,secchi_res)
    }else{
      #### moving average of predicted secchi values
      secchi_obs_predict$secchi_ma_predict <- rollmean(secchi_obs_predict$secchi_value, k = 15, fill = NA)
      secchi_obs_predict$secchi_ma_predict <- ifelse(is.na(secchi_obs_predict$secchi_ma_predict), secchi_obs_predict$secchi_value[1],secchi_obs_predict$secchi_ma_predict)
      
      if (secchi_ma_indicator == 1){
        secchi_obs_predict$secchi_ma_obs <- rollmean(secchi_obs_predict$secchi, k = 15, fill = NA)
        secchi_obs_predict$secchi_ma_obs <- ifelse(is.na(secchi_obs$secchi_ma_obs), secchi_obs$secchi[1],secchi_obs$secchi_ma_obs)
        res_secchi <- secchi_df_init %>% 
          mutate(secchi_res = secchi_ma_obs - secchi_ma_predict) %>% 
          select(sampledate,secchi_value,secchi_res)
      }else{
        res_secchi <- secchi_obs_predict %>% 
          #drop_na(secchi) %>% 
          mutate(secchi_res = secchi - secchi_ma_predict) %>% 
          select(sampledate,secchi_value,secchi_res)
        
      }
      
    }
    
    #create df of initial predictions and sampledates for comparison (y_prime)
    epi_do_main <- res_do_epi %>% select(sampledate,do_conc)
    hypo_do_main <- res_do_hypo %>% select(sampledate,do_conc) 
    epi_doc_main <- res_doc_epi %>% select(sampledate,doc_conc)
    hypo_doc_main <- res_doc_hypo %>% select(sampledate,doc_conc)
    secchi_main <- res_secchi %>% select(sampledate,secchi_value)
    
    
    
    ##create dfs to store res values (e_prime)
    
    epi_do_res_master <- data.frame(matrix(ncol = nrow(res_do_epi), nrow = 100))
    
    hypo_do_res_master <- data.frame(matrix(ncol = nrow(res_do_hypo), nrow = 100))
    
    epi_doc_res_master <- data.frame(matrix(ncol = nrow(res_doc_epi), nrow = 100))
    
    hypo_doc_res_master <- data.frame(matrix(ncol = nrow(res_doc_hypo), nrow = 100))
    
    secchi_res_master <- data.frame(matrix(ncol = nrow(res_secchi), nrow = 100))
    
    
    ##make param table
    #param_df_master <- data.frame(ip_param = NA, resp_poc_l_param = NA, resp_doc_l_param = NA)
    
    
    
    ##update first row of res tables with randomized residuals from first model run
    epi_do_res_master[1,] <- sample(res_do_epi$do_res) 
    
    hypo_do_res_master[1,] <- sample(res_do_hypo$do_res)
    
    epi_doc_res_master[1,] <- sample(res_doc_epi$doc_res)
    
    hypo_doc_res_master[1,] <- sample(res_doc_hypo$doc_res)
    
    secchi_res_master[1,] <- sample(res_secchi$secchi_res)
    
    
    epi_do_obs_pseudo <- epi_do_main %>% mutate(do_epi = do_conc + as.numeric(epi_do_res_master[1,])) %>% select(sampledate,do_epi)
    hypo_do_obs_pseudo <- hypo_do_main %>% mutate(do_hypo = do_conc + as.numeric(hypo_do_res_master[1,])) %>% select(sampledate,do_hypo)
    epi_doc_obs_pseudo <- epi_doc_main %>% mutate(doc_epi = doc_conc + as.numeric(epi_doc_res_master[1,])) %>% select(sampledate,doc_epi)
    hypo_doc_obs_pseudo <- hypo_doc_main %>% mutate(doc_hypo = doc_conc + as.numeric(hypo_doc_res_master[1,])) %>% select(sampledate,doc_hypo)
    secchi_obs_pseudo <- secchi_main %>% mutate(secchi = secchi_value + as.numeric(secchi_res_master[1,])) %>% select(sampledate,secchi)
    
    pseudo_obs_df <- list(epi_do_obs_pseudo,hypo_do_obs_pseudo,epi_doc_obs_pseudo,hypo_doc_obs_pseudo,secchi_obs_pseudo)
  
    
  }else if (i > 1){ ### BEGINNING OF BOOTSTRAP CREATION
    input_param_df$ip <- param_guess[1]
    input_param_df$resp_poc_l <- param_guess[2]
    input_param_df$resp_doc_l <- param_guess[3]
    
    start_time <- Sys.time() 
    #print('Finding parameters using observational data...')
    
    fit_model <- modFit(f=get_residuals,
                        p=param_guess,
                        driving_data = lake_data_full,
                        config = input_param_df,
                        lake_morph = lake_morphology,
                        lake_obs = pseudo_obs_df,
                        secchi_ma_indicate = 0,
                        method = 'Marq',
                        lower = param_range_lower,
                        upper = param_range_upper)
    
    print(paste('Iteration Level: ',i-1))
    print(paste('# of Iterations: ',fit_model$iterations))
    fit_time <- Sys.time()
    print(fit_time - start_time)
    print('-----------------------')
    
    
    #update params with best fit results
    input_param_df$ip <- fit_model$par[1]
    input_param_df$resp_poc_l <- fit_model$par[2]
    input_param_df$resp_doc_l <- fit_model$par[3]
    
    param_df_master[i-1,1] <- fit_model$par[1]
    param_df_master[i-1,2] <- fit_model$par[2]
    param_df_master[i-1,3] <- fit_model$par[3]
    
    
    
    #run model
    model_results <- metab_model(lake_data_full,input_param_df,lake_morphology)
    
    #save output and calculate residuals 
    ##define DFs to use
    
    epi_df_init <- cbind(lake_data_full,model_results[[1]]) %>% 
      left_join(pseudo_obs_df[[1]],by = c('sampledate')) %>% #do_epi
      left_join(pseudo_obs_df[[3]],by = c('sampledate')) #doc_epi
    
    hypo_df_init <- cbind(lake_data_full,model_results[[2]]) %>% 
      left_join(pseudo_obs_df[[2]],by = c('sampledate')) %>% #do_hypo
      left_join(pseudo_obs_df[[4]],by = c('sampledate')) #doc_hypo
    
    secchi_df_init <- cbind(lake_data_full,model_results[[5]]) %>% 
      left_join(pseudo_obs_df[[5]],by = c('sampledate')) %>% #secchi
      mutate(secchi_value = (1.7/lec_epi_value)) 
    
    secchi_ma_indicator <- 0
    
    #val_date <- '2010-01-01'
    
    #epi 
    res_do_epi <- epi_df_init %>% 
      drop_na(do_epi) %>% #obs col
      mutate(do_conc = do_value/volume_epi) %>% 
      mutate(do_res = do_epi - do_conc) %>% 
      select(sampledate,do_conc,do_res)
    
    #res_do_epi_cal <- res_do_epi %>% filter(sampledate < val_date)
    #res_do_epi_val <- res_do_epi %>% filter(sampledate > val_date)
    
    res_doc_epi <- epi_df_init %>% 
      drop_na(doc_epi) %>% #obs col
      mutate(doc_conc = (doc_r_value + doc_l_value)/volume_epi) %>% 
      mutate(doc_res = doc_epi - doc_conc) %>% 
      select(sampledate,doc_conc,doc_res)
    
    #res_doc_epi_cal <- res_doc_epi %>% filter(sampledate < val_date)
    #res_doc_epi_val <- res_doc_epi %>% filter(sampledate > val_date)
    
    #hypo
    res_do_hypo <- hypo_df_init %>% 
      drop_na(do_hypo) %>% #obs col
      mutate(do_conc = do_value/volume_hypo) %>% 
      mutate(do_res = do_hypo - do_conc) %>% 
      select(sampledate,do_conc,do_res)
    
    #res_do_hypo_cal <- res_do_hypo %>% filter(sampledate < val_date)
    #res_do_hypo_val <- res_do_hypo %>% filter(sampledate > val_date)
    
    res_doc_hypo <- hypo_df_init %>% 
      drop_na(doc_hypo) %>% #obs col
      mutate(doc_conc = (doc_r_value + doc_l_value)/volume_hypo) %>% 
      mutate(doc_res = doc_hypo - doc_conc) %>% 
      select(sampledate,doc_conc,doc_res)
    
    #res_doc_hypo_cal <- res_doc_hypo %>% filter(sampledate < val_date)
    #res_doc_hypo_val <- res_doc_hypo %>% filter(sampledate > val_date)
    
    #secchi
    secchi_obs_predict <- secchi_df_init %>% drop_na(secchi)
    
    if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
      res_secchi <- secchi_obs_predict %>% 
        #drop_na(secchi) %>% 
        mutate(secchi_res = secchi - secchi_value) %>% 
        select(sampledate,secchi_value,secchi_res)
    }else{
      #### moving average of predicted secchi values
      secchi_obs_predict$secchi_ma_predict <- rollmean(secchi_obs_predict$secchi_value, k = 15, fill = NA)
      secchi_obs_predict$secchi_ma_predict <- ifelse(is.na(secchi_obs_predict$secchi_ma_predict), secchi_obs_predict$secchi_value[1],secchi_obs_predict$secchi_ma_predict)
      
      if (secchi_ma_indicator == 1){
        secchi_obs_predict$secchi_ma_obs <- rollmean(secchi_obs_predict$secchi, k = 15, fill = NA)
        secchi_obs_predict$secchi_ma_obs <- ifelse(is.na(secchi_obs$secchi_ma_obs), secchi_obs$secchi[1],secchi_obs$secchi_ma_obs)
        res_secchi <- secchi_df_init %>% 
          mutate(secchi_res = secchi_ma_obs - secchi_ma_predict) %>% 
          select(sampledate,secchi_value,secchi_res)
      }else{
        res_secchi <- secchi_obs_predict %>% 
          #drop_na(secchi) %>% 
          mutate(secchi_res = secchi - secchi_ma_predict) %>% 
          select(sampledate,secchi_value,secchi_res)
        
      }
      
    }
    
    ##update first row of res tables with randomized residuals from first model run
    epi_do_res_master[i,] <- sample(res_do_epi$do_res) 
    
    hypo_do_res_master[i,] <- sample(res_do_hypo$do_res)
    
    epi_doc_res_master[i,] <- sample(res_doc_epi$doc_res)
    
    hypo_doc_res_master[i,] <- sample(res_doc_hypo$doc_res)
    
    secchi_res_master[i,] <- sample(res_secchi$secchi_res)
    
    
    epi_do_obs_pseudo <- epi_do_main %>% mutate(do_epi = do_conc + as.numeric(epi_do_res_master[i,])) %>% select(sampledate,do_epi)
    hypo_do_obs_pseudo <- hypo_do_main %>% mutate(do_hypo = do_conc + as.numeric(hypo_do_res_master[i,])) %>% select(sampledate,do_hypo)
    epi_doc_obs_pseudo <- epi_doc_main %>% mutate(doc_epi = doc_conc + as.numeric(epi_doc_res_master[i,])) %>% select(sampledate,doc_epi)
    hypo_doc_obs_pseudo <- hypo_doc_main %>% mutate(doc_hypo = doc_conc + as.numeric(hypo_doc_res_master[i,])) %>% select(sampledate,doc_hypo)
    secchi_obs_pseudo <- secchi_main %>% mutate(secchi = secchi_value + as.numeric(secchi_res_master[i,])) %>% select(sampledate,secchi)
    
    pseudo_obs_df <- list(epi_do_obs_pseudo,hypo_do_obs_pseudo,epi_doc_obs_pseudo,hypo_doc_obs_pseudo,secchi_obs_pseudo)
    
  }
}
final_time <- Sys.time()
print('complete!')
print(paste('Total run time: ',final_time - init_start_time))

# 
# # 
# # start_time <- Sys.time() 
# # print('Finding parameters using observational data...')
# # 
# # fit_model <- modFit(f=get_obs_residuals,
# #                     p=param_guess,
# #                     driving_data = lake_data_full,
# #                     config = input_param_df,
# #                     lake_morph = lake_morphology,
# #                     lake_obs = obs_df,
# #                     method = 'Marq',
# #                     lower = param_range_lower,
# #                     upper = param_range_upper)
# # 
# # 
# # 
# # print('Initial parameters found')
# # fit_time <- Sys.time()
# # print(fit_time - start_time)
# # 
# # init_pars <- fit_model$residuals
# 
# #update params with best fit results
# input_param_df$ip <- fit_model$par[1]
# input_param_df$resp_poc_l <- fit_model$par[2]
# input_param_df$resp_doc_l <- fit_model$par[3]
# 
# #run model
# model_results <- metab_model(lake_data_full,input_param_df,lake_morphology)
# 
# #save output and calculate residuals 
# ##define DFs to use
# 
# epi_df_init <- cbind(lake_data_full,model_results[[1]]) %>% 
#   left_join(obs_df[[1]],by = c('sampledate')) %>% #do_epi
#   left_join(obs_df[[3]],by = c('sampledate')) #doc_epi
# 
# hypo_df_init <- cbind(lake_data_full,model_results[[2]]) %>% 
#   left_join(obs_df[[2]],by = c('sampledate')) %>% #do_hypo
#   left_join(obs_df[[4]],by = c('sampledate')) #doc_hypo
# 
# secchi_df_init <- cbind(lake_data_full,model_results[[5]]) %>% 
#   left_join(obs_df[[5]],by = c('sampledate')) %>% #secchi
#   mutate(secchi_value = (1.7/lec_epi_value)) 
# 
# #val_date <- '2010-01-01'
# 
# #epi 
# res_do_epi <- epi_df_init %>% 
#   drop_na(do_epi) %>% #obs col
#   mutate(do_conc = do_value/volume_epi) %>% 
#   mutate(do_res = do_epi - do_conc) %>% 
#   select(sampledate,do_conc,do_res)
# 
# #res_do_epi_cal <- res_do_epi %>% filter(sampledate < val_date)
# #res_do_epi_val <- res_do_epi %>% filter(sampledate > val_date)
# 
# res_doc_epi <- epi_df_init %>% 
#   drop_na(doc_epi) %>% #obs col
#   mutate(doc_conc = (doc_r_value + doc_l_value)/volume_epi) %>% 
#   mutate(doc_res = doc_epi - doc_conc) %>% 
#   select(sampledate,doc_conc,doc_res)
# 
# #res_doc_epi_cal <- res_doc_epi %>% filter(sampledate < val_date)
# #res_doc_epi_val <- res_doc_epi %>% filter(sampledate > val_date)
# 
# #hypo
# res_do_hypo <- hypo_df_init %>% 
#   drop_na(do_hypo) %>% #obs col
#   mutate(do_conc = do_value/volume_hypo) %>% 
#   mutate(do_res = do_hypo - do_conc) %>% 
#   select(sampledate,do_conc,do_res)
# 
# #res_do_hypo_cal <- res_do_hypo %>% filter(sampledate < val_date)
# #res_do_hypo_val <- res_do_hypo %>% filter(sampledate > val_date)
# 
# res_doc_hypo <- hypo_df_init %>% 
#   drop_na(doc_hypo) %>% #obs col
#   mutate(doc_conc = (doc_r_value + doc_l_value)/volume_hypo) %>% 
#   mutate(doc_res = doc_hypo - doc_conc) %>% 
#   select(sampledate,doc_conc,doc_res)
# 
# #res_doc_hypo_cal <- res_doc_hypo %>% filter(sampledate < val_date)
# #res_doc_hypo_val <- res_doc_hypo %>% filter(sampledate > val_date)
# 
# #secchi
# secchi_obs_predict <- secchi_df_init %>% drop_na(secchi)
# 
# if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
#   res_secchi <- secchi_obs_predict %>% 
#     #drop_na(secchi) %>% 
#     mutate(secchi_res = secchi - secchi_value) %>% 
#     select(sampledate,secchi_value,secchi_res)
# }else{
#   #### moving average of predicted secchi values
#   secchi_obs_predict$secchi_ma_predict <- rollmean(secchi_obs_predict$secchi_value, k = 15, fill = NA)
#   secchi_obs_predict$secchi_ma_predict <- ifelse(is.na(secchi_obs_predict$secchi_ma_predict), secchi_obs_predict$secchi_value[1],secchi_obs_predict$secchi_ma_predict)
#   
#   if (secchi_ma_indicate == 1){
#   secchi_obs_predict$secchi_ma_obs <- rollmean(secchi_obs_predict$secchi, k = 15, fill = NA)
#   secchi_obs_predict$secchi_ma_obs <- ifelse(is.na(secchi_obs$secchi_ma_obs), secchi_obs$secchi[1],secchi_obs$secchi_ma_obs)
#   res_secchi <- secchi_df_init %>% 
#     mutate(secchi_res = secchi_ma_obs - secchi_ma_predict) %>% 
#     select(sampledate,secchi_value,secchi_res)
#   }else{
#     res_secchi <- secchi_obs_predict %>% 
#       #drop_na(secchi) %>% 
#       mutate(secchi_res = secchi - secchi_ma_predict) %>% 
#       select(sampledate,secchi_value,secchi_res)
#     
#   }
# 
# }
# 
# #create df of initial predictions and sampledates for comparison (y_prime)
# epi_do_main <- res_do_epi %>% select(sampledate,do_conc)
# hypo_do_main <- res_do_hypo %>% select(sampledate,do_conc) 
# epi_doc_main <- res_doc_epi %>% select(sampledate,doc_conc)
# hypo_doc_main <- res_doc_hypo %>% select(sampledate,doc_conc)
# secchi_main <- res_secchi %>% select(sampledate,secchi_value)
# 
# 
# 
# ##create dfs to store res values (e_prime)
# 
# epi_do_res_master <- data.frame(matrix(ncol = nrow(res_do_epi), nrow = 100))
# 
# hypo_do_res_master <- data.frame(matrix(ncol = nrow(res_do_hypo), nrow = 100))
# 
# epi_doc_res_master <- data.frame(matrix(ncol = nrow(res_doc_epi), nrow = 100))
# 
# hypo_doc_res_master <- data.frame(matrix(ncol = nrow(res_doc_hypo), nrow = 100))
# 
# secchi_res_master <- data.frame(matrix(ncol = nrow(res_secchi), nrow = 100))
# 
# 
# ##make param table
# param_df_master <- data.frame(ip_param = NA, resp_poc_l_param = NA, resp_doc_l_param = NA)
# 
# 
# 
# ##update first row of res tables with randomized residuals from first model run
# epi_do_res_master[1,] <- sample(res_do_epi$do_res) 
# 
# hypo_do_res_master[1,] <- sample(res_do_hypo$do_res)
# 
# epi_doc_res_master[1,] <- sample(res_doc_epi$doc_res)
# 
# hypo_doc_res_master[1,] <- sample(res_do_hypo$doc_res)
# 
# secchi_res_master[1,] <- sample(res_secchi$secchi_res)
# 
# 
# epi_do_obs_pseudo <- epi_do_main %>% mutate(do_epi = do_conc + as.numeric(epi_do_res_master[1,])) %>% select(sampledate,do_epi)
# hypo_do_obs_pseudo <- hypo_do_main %>% mutate(do_hypo = do_conc + as.numeric(hypo_do_res_master[1,])) %>% select(sampledate,do_hypo)
# epi_doc_obs_pseudo <- epi_doc_main %>% mutate(doc_epi = doc_conc + as.numeric(epi_doc_res_master[1,])) %>% select(sampledate,doc_epi)
# hypo_doc_obs_pseudo <- hypo_doc_main %>% mutate(doc_hypo = doc_conc + as.numeric(hypo_doc_res_master[1,])) %>% select(sampledate,doc_hypo)
# secchi_obs_pseudo <- secchi_main %>% mutate(secchi = secchi_value + as.numeric(seccho_res_master[1,])) %>% select(sampledate,secchi)
# 
# pseudo_obs_all <- c(epi_doc_obs_pseudo,hypo_do_obs_pseudo,epi_doc_obs_pseudo,hypo_doc_obs_pseudo,secchi_obs_pseudo)
# 
# 
# 
# 
# 
# 
# #res_secchi_cal <- res_secchi %>% filter(sampledate < val_date)
# #res_secchi_val <- res_secchi %>% filter(sampledate > val_date)
# 
# 
# 
# ##############################################################################################
# second_func <- function(){
#   
# }
# 
# 
# get_residuals <- function(driving_data,config,lake_morph,lake_obs){
#   
#   model_output <- metab_model(driving_data,config,lake_morph)
#   
#   sampledate <- driving_data$sampledate
#   epi_vars_output <- cbind(sampledate,(model_output[[1]]/driving_data$volume_epi))
#   hypo_vars_output <- cbind(sampledate,(model_output[[2]]/driving_data$volume_hypo))
#   
#   #make individual vectors of residuals to be combined later
#   do_obs_epi <- lake_obs[[1]] #%>% select(sampledate,do_epi)
#   do_obs_hypo <- lake_obs[[2]] #%>% select(sampledate,do_hypo)
#   doc_obs_epi <- lake_obs[[3]] #%>% select(sampledate,doc_epi)
#   doc_obs_hypo <- lake_obs[[4]] #%>% select(sampledate,doc_hypo)
#   secchi_obs <- lake_obs[[5]] %>% rename(secchi_obs = secchi)
#   
#   do_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_epi,by='sampledate')
#   do_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% select(sampledate,do_value) %>% inner_join(do_obs_hypo,by='sampledate') %>% filter(do_value != 0)
#   doc_epi_df <- cbind(sampledate,(model_output[[1]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_epi,by='sampledate')
#   doc_hypo_df <- cbind(sampledate,(model_output[[2]]/lake_data_full$volume_epi)) %>% mutate(doc_value = doc_l_value + doc_r_value) %>% select(sampledate,doc_value) %>% inner_join(doc_obs_hypo,by='sampledate') %>% filter(doc_value != 0)
#   
#   secchi_value <- 1.7/model_output[[5]]$lec_epi_value
#   secchi_df <- cbind(sampledate,model_output[[5]]) %>% mutate(secchi = 1.7/lec_epi_value) %>% select(sampledate,secchi) %>% inner_join(secchi_obs,by='sampledate')
#   
#   if(nhdid %in% c('nhdhr_143249470','nhdhr_143249640')){
#     secchi_res <- (secchi_df$secchi - secchi_df$secchi_obs)
#   }else{
#     #### moving average of predicted secchi values
#     secchi_df$secchi_ma_predict <- rollmean(secchi_df$secchi, k = 15, fill = NA)
#     secchi_df$secchi_ma_predict <- ifelse(is.na(secchi_df$secchi_ma_predict), secchi_df$secchi[1],secchi_df$secchi_ma_predict)
#     
#     secchi_res <- secchi_df$secchi_ma_predict - secchi_df$secchi_obs
#     
#   }
# 
#   do_epi_res <- do_epi_df$do_value - do_epi_df$do_epi
#   do_hypo_res <- do_hypo_df$do_value - do_hypo_df$do_hypo
#   
#   doc_epi_res <- doc_epi_df$doc_value - doc_epi_df$doc_epi
#   doc_hypo_res <- doc_hypo_df$doc_value - doc_hypo_df$doc_hypo
#   
#   all_res <- c(do_epi_res,do_hypo_res,doc_epi_res,doc_hypo_res,secchi_res)
#   return(all_res)
# }
# 
# # ####### RUN FITTING ####
# start_time <- Sys.time() 
# ## FIGURE OUT HOW TO SET LIMIT ON ITERATIONS
# print('Fitting model...')
# print('Finding starting parameters...')
# 
# fit_model <- modFit(f=get_residuals,
#                     p=param_guess,
#                     driving_data = lake_data_full,
#                     config = input_param_df,
#                     lake_morph = lake_morphology,
#                     lake_obs = obs_df,
#                     method = 'Marq',
#                     #lower = c(0.00005,0.2,1.04),
#                     #upper = c(0.005,0.4,1.15))
#                     # lower = c(0.0005,0.2,1.04,0.2,0.3),
#                     # upper = c(0.01,0.4,1.15,2,1))
#                     # lower = c(0.001,0.2,0.003,1),#,0.1), ## ip = c(0.0005,0.05)
#                     # upper = c(0.05,0.4,0.03,2))#,1))
#                     # lower = c(0.005,0.003,1),
#                     # upper = c(0.05,0.03,2))#,1))
#                     lower = param_range_lower,
#                     upper = param_range_upper)
# 
# #model_sp <- summary(fit_model)
# #model_sp[]
# #covar_out <- model_sp$cov.scaled * 2.4^2/2
# 
# print('Parameters found, running MCMC...')
# fit_time <- Sys.time()
# print(fit_time - start_time)
# 
# mcmc_run <- modMCMC(f=get_residuals,
#                     p=fit_model$par,
#                     var0 = 1,#model_sp$modVariance, # revisit this parameter
#                     jump = 0.01,#NULL,#covar_out,
#                     niter = 1000,
#                     driving_data = lake_data_full,
#                     config = input_param_df,
#                     lake_morph = lake_morphology,
#                     lake_obs = obs_df,
#                     #lower = c(0.00005,0.2,1.04),
#                     #upper = c(0.005,0.4,1.15)) #0.001
#                     #upper = c(0.005,0.4,1.15))
#                     # lower = c(0.0005,0.2,1.04,0.2,0.3),
#                     # upper = c(0.01,0.4,1.15,2,1))
#                     # lower = c(0.001,0.2,0.003,1),#,0.1),
#                     # upper = c(0.05,0.4,0.03,2))#,1))
#                     # lower = c(0.005,0.003,1),
#                     # upper = c(0.05,0.03,2))#,1))
#                     lower = param_range_lower,
#                     upper = param_range_upper)
# 
# end_time <- Sys.time()
# 
# print('MCMC finished...')
# print(end_time - fit_time)
# print('--------------------')
# print('Total time...')
# print(end_time - start_time)
# 
# 
# # fit_model <- modFit(f=get_residuals,
# #                     p=param_guess,
# #                     driving_data = lake_data_full,
# #                     config = input_param_df,
# #                     lake_morph = lake_morphology,
# #                     lake_obs = obs_df,
# #                     method = 'Marq',
# #                     lower = c(0.00005,0.01,1.04),
# #                     upper = c(0.005,0.4,1.15)) #0.001
# 
# ##try MCMC
# # fit_model <- modMCMC(f=get_residuals,
# #                     p=param_guess,
# #                     var0 = 1, # revisit this parameter 
# #                     driving_data = lake_data_full,
# #                     config = input_param_df,
# #                     lake_morph = lake_morphology,
# #                     lake_obs = obs_df,
# #                     lower = c(0.00005,0.2,1.04),
# #                     upper = c(0.005,0.4,1.15)) #0.001
# 
# # 
# # end_time <- Sys.time()
# # print(end_time - start_time)
