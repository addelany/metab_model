draws <- 100
std_dev <- 0.2

#create normalized distributions for the three stochastic parameters
p1_samples <- rnorm(draws,input_param_df$ip,input_param_df$ip*std_dev) #(n,mean,std)
p2_samples <- rnorm(draws,input_param_df$resp_poc_l,input_param_df$resp_poc_l*std_dev)
p3_samples <- rnorm(draws,input_param_df$resp_doc_l,input_param_df$resp_doc_l*std_dev)

n_days <- nrow(lake_data_full) #number of time steps


#save empy dataframes to fill as run model iterations (11 in total)


# #state variables
do_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
do_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))

doc_r_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
doc_l_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
doc_total_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
doc_r_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
doc_l_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
doc_total_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))

poc_r_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
poc_l_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
poc_r_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
poc_l_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))

secchi_df <- data.frame(matrix(ncol = n_days, nrow = 100))

#fluxes
npp_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_sed_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_pocl_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_pocr_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_docl_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_docr_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
atm_exch_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocl_settle_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocr_settle_epi_df <- data.frame(matrix(ncol = n_days, nrow = 100))
docr_in_df <- data.frame(matrix(ncol = n_days, nrow = 100))
docl_in_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocr_in_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocl_in_df <- data.frame(matrix(ncol = n_days, nrow = 100))
docr_out_df <- data.frame(matrix(ncol = n_days, nrow = 100))
docl_out_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocr_out_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocl_out_df <- data.frame(matrix(ncol = n_days, nrow = 100))

npp_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_sed_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_pocl_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_pocr_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_docl_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
resp_docr_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocl_settle_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocr_settle_hypo_df <- data.frame(matrix(ncol = n_days, nrow = 100))
pocr_sed_hypo <- data.frame(matrix(ncol = n_days, nrow = 100))
pocl_sed_hypo <- data.frame(matrix(ncol = n_days, nrow = 100))
poc_tot_sed_hypo <- data.frame(matrix(ncol = n_days, nrow = 100))

## RUN MODEL 100 TIMES WITH A NEW SAMPLE FROM EACH PARAMETER DISTRIBUTION EACH TIME
for (i in seq(1:draws)){
  # update parameters
  input_param_df$ip <- p1_samples[i]
  input_param_df$resp_poc_l <- p2_samples[i]
  input_param_df$resp_sed_hypo <- p3_samples[i]
  
  
  #run model
  model_output <- metab_model(lake_data_full,input_param_df,lake_morphology)
  
  #save output 
  # state variables 
  do_epi_df[i,] <- model_output[[1]]$do_value/lake_data_full$volume_epi
  do_hypo_df[i,] <- model_output[[2]]$do_value/lake_data_full$volume_hypo
  
  doc_r_epi_df[i,] <- model_output[[1]]$doc_r_value/lake_data_full$volume_epi
  doc_r_hypo_df[i,] <- model_output[[2]]$doc_r_value/lake_data_full$volume_hypo

  doc_l_epi_df[i,] <- model_output[[1]]$doc_l_value/lake_data_full$volume_epi
  doc_l_hypo_df[i,] <- model_output[[2]]$doc_l_value/lake_data_full$volume_hypo
  
  doc_total_epi_df[i,] <- (model_output[[1]]$doc_r_value + model_output[[1]]$doc_l_value)/lake_data_full$volume_epi
  doc_total_hypo_df[i,] <- (model_output[[2]]$doc_r_value + model_output[[2]]$doc_l_value)/lake_data_full$volume_hypo
  
  poc_r_epi_df[i,] <- model_output[[1]]$poc_r_value/lake_data_full$volume_epi
  poc_r_hypo_df[i,] <- model_output[[2]]$poc_r_value/lake_data_full$volume_hypo
  
  poc_l_epi_df[i,] <- model_output[[1]]$poc_l_value/lake_data_full$volume_epi
  poc_l_hypo_df[i,] <- model_output[[2]]$poc_l_value/lake_data_full$volume_hypo
  
  secchi_df[i,] <- 1.7/(model_output[[5]]$lec_epi_value)
  
  #fluxes 
  #epi
  npp_epi_df[i,] <- model_output[[3]]$npp_value
  resp_sed_epi_df[i,] <- model_output[[3]]$resp_sed
  resp_pocl_epi_df[i,] <- model_output[[3]]$resp_poc_l
  resp_pocr_epi_df[i,] <- model_output[[3]]$resp_poc_r
  resp_docl_epi_df[i,] <- model_output[[3]]$resp_doc_l
  resp_docr_epi_df[i,] <- model_output[[3]]$resp_doc_r
  atm_exch_epi_df[i,] <- model_output[[3]]$atm_exhange
  pocl_settle_epi_df[i,] <- model_output[[3]]$poc_l_settle
  pocr_settle_epi_df[i,] <- model_output[[3]]$poc_r_settle
  docr_in_df[i,] <- model_output[[3]]$doc_r_in
  docl_in_df[i,] <- model_output[[3]]$doc_l_in
  pocr_in_df[i,] <- model_output[[3]]$poc_r_in
  pocl_in_df[i,] <- model_output[[3]]$poc_l_in
  docr_out_df[i,] <- model_output[[3]]$doc_r_out
  docl_out_df[i,] <- model_output[[3]]$doc_l_out
  pocr_out_df[i,] <- model_output[[3]]$poc_r_out
  pocl_out_df[i,] <- model_output[[3]]$poc_l_out
  
  #hypo
  npp_hypo_df[i,] <- model_output[[4]]$npp_value
  resp_sed_hypo_df[i,] <- model_output[[4]]$resp_sed
  resp_pocl_hypo_df[i,] <- model_output[[4]]$resp_poc_l
  resp_pocr_hypo_df[i,] <- model_output[[4]]$resp_poc_r
  resp_docl_hypo_df[i,] <- model_output[[4]]$resp_doc_l
  resp_docr_hypo_df[i,] <- model_output[[4]]$resp_doc_r
  pocl_settle_hypo_df[i,] <- model_output[[4]]$poc_l_settle
  pocr_settle_hypo_df[i,] <- model_output[[4]]$poc_r_settle
  pocr_sed_hypo[i,] <- model_output[[4]]$pocr_sediment_flux
  pocl_sed_hypo[i,] <- model_output[[4]]$pocl_sediment_flux
  poc_tot_sed_hypo[i,] <- model_output[[4]]$poc_total_sediment_flux
  
  print(i)
}

############################## NOW CALCULATE UNCERTAINTY BOUNDS ###################################################


#define uncertainty calculation function first

uncertainty_output <- function(var_df,var_name){
mean_list <- colMeans(var_df)
q25_list <- c()
q75_list <- c()
for (i in seq(1:ncol(var_df))){
  q25_list[i] <- unname(quantile(var_df[,i],c(0.025),na.rm = TRUE)) #named q25 but actually 2.5%
  q75_list[i] <- unname(quantile(var_df[,i],c(0.975),na.rm = TRUE)) #named q75 but actually 97.5%
}

main_df <- data.frame(var_mean = mean_list, 
                      var_q25 = q25_list, 
                      var_q75 = q75_list)
colnames(main_df) <- c(paste(var_name,'_mean',sep=''),paste(var_name,'_q25',sep=''),paste(var_name,'_q75',sep = ''))
return(main_df)
}

do_epi_output <- uncertainty_output(do_epi_df,'do')
do_hypo_output <- uncertainty_output(do_hypo_df,'do')
doc_r_epi_output <- uncertainty_output(doc_r_epi_df,'doc_r')
doc_r_hypo_output <- uncertainty_output(doc_r_hypo_df,'doc_r')
doc_l_epi_output <- uncertainty_output(doc_l_epi_df,'doc_l')
doc_l_hypo_output <- uncertainty_output(doc_l_hypo_df,'doc_l')
doc_total_epi_output <- uncertainty_output(doc_total_epi_df,'doc_tot')
doc_total_hypo_output <- uncertainty_output(doc_total_hypo_df,'doc_tot')
poc_r_epi_output <- uncertainty_output(poc_r_epi_df,'poc_r')
poc_r_hypo_output <- uncertainty_output(poc_r_hypo_df,'poc_r')
poc_l_epi_output <- uncertainty_output(poc_l_epi_df,'poc_l')
poc_l_hypo_output <- uncertainty_output(poc_l_hypo_df,'poc_l')
secchi_output <- uncertainty_output(secchi_df,'secchi')

state_vars_epi_df <- cbind(do_epi_output,doc_r_epi_output,doc_l_epi_output,doc_total_epi_output,poc_r_epi_output,poc_l_epi_output,secchi_output)
state_vars_hypo_df <- cbind(do_hypo_output,doc_r_hypo_output,doc_l_hypo_output,doc_total_hypo_output,poc_r_hypo_output,poc_l_hypo_output)

write.csv(state_vars_epi_df,paste0('~/uncertainty/',nhdid,'/epi_vars.csv'),row.names = FALSE)
write.csv(state_vars_hypo_df,paste0('~/uncertainty/',nhdid,'/hypo_vars.csv'),row.names = FALSE)


## FLUX
#epi
npp_epi_output <- uncertainty_output(npp_epi_df,'npp')
resp_sed_epi_output <- uncertainty_output(resp_sed_epi_df,'resp_sed')
resp_pocl_epi_output <- uncertainty_output(resp_pocl_epi_df,'resp_pocl')
resp_pocr_epi_output <- uncertainty_output(resp_pocr_epi_df,'resp_pocr')
resp_docl_epi_output <- uncertainty_output(resp_docl_epi_df,'resp_docl')
resp_docr_epi_output <- uncertainty_output(resp_docr_epi_df,'resp_docr')
atm_exch_epi_output <- uncertainty_output(atm_exch_epi_df,'atm_exch')
pocl_settle_epi_output <- uncertainty_output(pocl_settle_epi_df,'pocl_settle')
pocr_settle_epi_output <- uncertainty_output(pocr_settle_epi_df,'poc_settle')
docr_in_output <- uncertainty_output(docr_in_df,'docr_in')
docl_in_output <- uncertainty_output(docl_in_df,'docl_in')
pocr_in_output <- uncertainty_output(pocr_in_df,'pocr_in')
pocl_in_output <- uncertainty_output(pocl_in_df,'pocl_in')
docr_out_output <- uncertainty_output(docr_out_df,'docr_out')
docl_out_output <- uncertainty_output(docl_out_df,'docl_out')
pocr_out_output <- uncertainty_output(pocr_out_df,'pocr_out')
pocl_out_output <- uncertainty_output(pocl_out_df,'pocl_out')

#hypo
npp_hypo_output <- uncertainty_output(npp_hypo_df,'npp')
resp_sed_hypo_output <- uncertainty_output(resp_sed_hypo_df,'resp_sed')
resp_pocl_hypo_output <- uncertainty_output(resp_pocl_hypo_df,'resp_pocl')
resp_pocr_hypo_output <- uncertainty_output(resp_pocr_hypo_df,'resp_pocr')
resp_docl_hypo_output <- uncertainty_output(resp_docl_hypo_df,'resp_docl')
resp_docr_hypo_output <- uncertainty_output(resp_docr_hypo_df,'resp_docr')
pocl_settle_hypo_output <- uncertainty_output(pocl_settle_hypo_df,'pocl_settle')
pocr_settle_hypo_output <- uncertainty_output(pocr_settle_hypo_df,'pocr_settle')
pocr_sed_output <- uncertainty_output(pocr_sed_hypo,'pocr_sed')
pocl_sed_output <- uncertainty_output(pocl_sed_hypo,'pocl_sed')
poc_tot_sed_output <- uncertainty_output(poc_tot_sed_hypo,'poc_tot')



fluxes_epi_df <- cbind(npp_epi_output,resp_sed_epi_output,resp_pocl_epi_output,resp_pocr_epi_output,resp_docl_epi_output,resp_docr_epi_output,
                           atm_exch_epi_output,pocl_settle_epi_output,pocr_settle_epi_output,docr_in_output,docl_in_output,pocr_in_output,
                           pocl_in_output,docr_out_output,docl_out_output,pocr_out_output,pocl_out_output)
fluxes_hypo_df <- cbind(npp_hypo_output,resp_sed_hypo_output,resp_pocl_hypo_output,resp_pocr_hypo_output,resp_docl_hypo_output,resp_docr_hypo_output,
                            pocl_settle_hypo_output,pocr_settle_hypo_output,pocr_sed_output,pocl_sed_output,poc_tot_sed_output)

write.csv(fluxes_epi_df,paste('~/uncertainty/',nhdid,'/epi_fluxes.csv', sep = ''),row.names = FALSE)
write.csv(fluxes_hypo_df,paste('~/uncertainty/',nhdid,'/hypo_fluxes.csv', sep = ''),row.names = FALSE)