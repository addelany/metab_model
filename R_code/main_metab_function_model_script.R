metab_model <- function(driver_df, params_df, lake_info){
  
  ### set up df structure for the state variables and the fluxes ###
  ## one df for each state variable and associated fluxes ##
  epi_vars <- data.frame(do_value = NA, poc_l_value = NA, poc_r_value = NA, doc_l_value = NA, doc_r_value = NA)
  hypo_vars <- data.frame(do_value = NA, poc_l_value = NA, poc_r_value = NA, doc_l_value = NA, doc_r_value = NA)
  
  epi_fluxes <- data.frame(atm_exhange = NA, do_entrainment = NA, resp_sed = NA, resp_poc_l = NA, resp_poc_r = NA, poc_l_settle = NA, 
                           poc_r_settle = NA, poc_l_entrainment = NA, poc_r_entrainment = NA, resp_doc_l = NA, doc_l_entrainment = NA, 
                           resp_doc_r = NA, doc_r_entrainment = NA, npp_value = NA, resp_total = NA)
  hypo_fluxes <- data.frame(resp_sed = NA, resp_poc_l = NA, resp_poc_r = NA, poc_l_settle = NA, poc_r_settle = NA, resp_doc_l = NA, 
                            resp_doc_r = NA, npp_value = NA, resp_total = NA)
  
  concentration_vars <- data.frame(lec_epi_value = NA, epi_light = NA, hypo_light = NA, entrainment_multiplier = NA)
  
  
  ##set initial values
  epi_vars[1,'do_value'] <- params_df[1,"do_init"]
  hypo_vars[1,'do_value'] <- params_df[1,"do_init"]*0 #initial volume of hypo is zero
  
  epi_vars[1,'poc_r_value'] <- params_df[1,"poc_r_init"]
  hypo_vars[1,'poc_r_value']  <- as.numeric(params_df[1,"poc_r_init"]*0)
  
  epi_vars[1,'poc_l_value'] <- params_df[1,"poc_l_init"]
  hypo_vars[1,'poc_l_value']  <- as.numeric(params_df[1,"poc_l_init"]*0)
  
  epi_vars[1,'doc_l_value'] <- params_df[1,"doc_l_init"]
  hypo_vars[1,'doc_l_value']  <- params_df[1,"doc_l_init"]*0
  
  epi_vars[1,'doc_r_value'] <- params_df[1,"doc_r_init"]
  hypo_vars[1,'doc_r_value']  <- params_df[1,"doc_r_init"]*0 

  #starting loop
  for (row in seq(2,nrow(driver_df))){#nrow(driver_df))){

    ## MODEL EQUATIONS ##
    theta_epi_npp <- (params_df[1,"theta_npp"]**(driver_df[row,'epi_temp'] - 20))
    theta_epi_resp <- (params_df[1,"theta_resp"]**(driver_df[row,'epi_temp'] - 20))
    
    concentration_vars[row,'epi_theta_npp'] <- theta_epi_npp
    concentration_vars[row,'epi_theta_resp'] <- theta_epi_resp
    theta_hypo_npp <- (params_df[1,"theta_npp"]**(driver_df[row,'hypo_temp'] - 20))
    theta_hypo_resp <- (params_df[1,"theta_resp"]**(driver_df[row,'hypo_temp'] - 20))
    
    
    concentration_vars[row,'hypo_theta_npp'] <- theta_hypo_npp
    concentration_vars[row,'hypo_theta_resp'] <- theta_hypo_resp
    
    ## MM Equations
    mm_do_epi <- (epi_vars[row-1,'do_value']/driver_df[row-1,'volume_epi'])/(params_df[1,"k_mm_do"] + (epi_vars[row-1,'do_value']/driver_df[row-1,'volume_epi']))
    tp_epi_adjusted <- driver_df[row-1,'tp_epi_approx']
    
    #need to account for v_hypo = 0 -- set mm_do to zero if that is the case
    if(driver_df[row-1,'volume_hypo'] == 0){
      mm_do_hypo <- 0
      #mm_tp_hypo <- 0
      tp_hypo_adjusted <- 0
      
    }else{
      mm_do_hypo <- (hypo_vars[row-1,'do_value']/driver_df[row-1,'volume_hypo'])/(params_df[1,"k_mm_do"] + (hypo_vars[row-1,'do_value']/driver_df[row-1,'volume_hypo']))
      tp_hypo_adjusted <- driver_df[row-1,'tp_hypo_approx']
    }
    
    #save mm values
    concentration_vars[row,'mm_epi'] <- mm_do_epi
    concentration_vars[row,'mm_hypo'] <- mm_do_hypo
    
    ## Inflow/Outflow calculations
    if (nhdid %in% c('nhdhr_143249640')){
      if (row == 2){ ### no first timestep information -- use estiamted inflow concentrations from config file
        epi_fluxes[row,'doc_l_in'] <- 0#params_df[1,"docl_inflow"]*(driver_df[row-1,'outflow_vol']) #inflow volume as outflow volume
        epi_fluxes[row,'doc_l_out'] <- (epi_vars[row-1,'doc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

        epi_fluxes[row,'doc_r_in'] <- 0#params_df[1,"docr_inflow"]*(driver_df[row-1,'outflow_vol'])
        epi_fluxes[row,'doc_r_out'] <- (epi_vars[row-1,'doc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

        epi_fluxes[row,'poc_l_in'] <- 0#params_df[1,"pocl_inflow"]*(driver_df[row-1,'outflow_vol'])
        epi_fluxes[row,'poc_l_out'] <- (epi_vars[row-1,'poc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

        epi_fluxes[row,'poc_r_in'] <- 0#params_df[1,"pocr_inflow"]*(driver_df[row-1,'outflow_vol'])
        epi_fluxes[row,'poc_r_out'] <- (epi_vars[row-1,'poc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

      }else{ ##if timestep > 2
      epi_fluxes[row,'doc_l_in'] <- driver_df[row-1,'docl_inflow']
      epi_fluxes[row,'doc_l_out'] <- (epi_vars[row-1,'doc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

      epi_fluxes[row,'doc_r_in'] <- driver_df[row-1,'docr_inflow']
      epi_fluxes[row,'doc_r_out'] <- (epi_vars[row-1,'doc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

      epi_fluxes[row,'poc_l_in'] <- driver_df[row-1,'pocl_inflow']
      epi_fluxes[row,'poc_l_out'] <- (epi_vars[row-1,'poc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])
      
      epi_fluxes[row,'poc_r_in'] <- driver_df[row-1,'pocr_inflow']
      epi_fluxes[row,'poc_r_out'] <- (epi_vars[row-1,'poc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])
      
      #print((epi_fluxes[row-1,'poc_r_in'] + epi_fluxes[row-1,'poc_l_in'])/(driver_df[row-1,'outflow_vol']))
      
      }
    }else{ # if running any lake other than Monona
    epi_fluxes[row,'doc_l_in'] <- params_df[1,"docl_inflow"]*(driver_df[row-1,'inflow_vol'])
    epi_fluxes[row,'doc_l_out'] <- (epi_vars[row-1,'doc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])
    
    epi_fluxes[row,'doc_r_in'] <- params_df[1,"docr_inflow"]*(driver_df[row-1,'inflow_vol'])
    #print(epi_fluxes[row,'doc_r_in']/driver_df[row-1,'inflow_vol'])
    #epi_fluxes[row,'doc_r_in'] <- params_df[1,"docr_inflow"]*(driver_df[row,'inflow_vol'])
    epi_fluxes[row,'doc_r_out'] <- (epi_vars[row-1,'doc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])

    epi_fluxes[row,'poc_l_in'] <- params_df[1,"pocl_inflow"]*(driver_df[row-1,'inflow_vol'])
    epi_fluxes[row,'poc_l_out'] <- (epi_vars[row-1,'poc_l_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])
    
    epi_fluxes[row,'poc_r_in'] <- params_df[1,"pocr_inflow"]*(driver_df[row-1,'inflow_vol'])
    epi_fluxes[row,'poc_r_out'] <- (epi_vars[row-1,'poc_r_value']/driver_df[row-1,'volume_epi'])*(driver_df[row-1,'outflow_vol'])
      }
    
    ###LEC calculation (independent of ice cover)
    params_df[1,"c_poc_l"] <- params_df[1,"c_poc_r"]
    
    lec_epi <- params_df[1,"c_w"] + 
      (params_df[1,"c_poc_l"]*(epi_vars[row-1,"poc_l_value"]/driver_df[row-1,'volume_epi'])) + 
      (params_df[1,"c_poc_l"]*(epi_vars[row-1,"poc_r_value"]/driver_df[row-1,'volume_epi'])) +
      (params_df[1,"c_doc_l"]*(epi_vars[row-1,"doc_l_value"]/driver_df[row-1,'volume_epi'])) + 
      (params_df[1,"c_doc_r"]*(epi_vars[row-1,"doc_r_value"]/driver_df[row-1,'volume_epi']))
    
    lec_hypo <- params_df[1,"c_w"] + 
      (params_df[1,"c_poc_l"]*(hypo_vars[row-1,"poc_l_value"]/driver_df[row-1,'volume_hypo'])) + 
      (params_df[1,"c_poc_l"]*(epi_vars[row-1,"poc_r_value"]/driver_df[row-1,'volume_epi'])) +
      (params_df[1,"c_doc_l"]*(hypo_vars[row-1,"doc_l_value"]/driver_df[row-1,'volume_hypo'])) + 
      (params_df[1,"c_doc_r"]*(hypo_vars[row-1,"doc_r_value"]/driver_df[row-1,'volume_hypo']))
    
    #save LEC value
    concentration_vars[row,'lec_epi_value'] <- lec_epi
    
    ### make light calculation function 
    light_calc <- function(z1,z2,a1,a2,ir,mu){
      deps = seq(z1, z2 , 0.1)
      irr =  ir * exp(-(mu)*deps)
      area = approx(c(z1,z2), c(a1,a2), deps)$y
      intarea = pracma::trapz((deps),(area))
      
      return((area %*% irr)/ (sum(area)))
      }
    
    ## ice flags
    if (driver_df[row,'ice'] == TRUE){
      o2_diff <- (driver_df[row-1,'do_sat_value']/driver_df[row-1,'volume_epi']) - (epi_vars[row-1,'do_value']/driver_df[row-1,'volume_epi']) #unit = grams/m^3
      epi_fluxes[row, "atm_exhange"] <- (driver_df[row-1,'gas_exchange_depth']*(o2_diff)*(lake_info[1,"area_total"])) * params_df[1,"k_winter_adjust"] #g/day

      ## save supplemental calculated information
      concentration_vars[row,'o2_diff_value'] <- o2_diff
      concentration_vars[row,'k_o2_value'] <- params_df[1,"k_winter_adjust"]
      concentration_vars[row,'lec_hypo_value'] <- 0
      
      #calculate epi light (assuming ice presence)
      light_amount_epi <- light_calc(z1=0,z2=lake_info[1,"maxdepth"],a1=lake_info[1,"area_total"],a2=0,ir = driver_df[row-1,'i_rad'], mu = lec_epi) * (1 - params_df[1,"albedo"])
      light_amount_epi <- light_amount_epi*params_df[1,"ice_transmit"] ##account for ice blocking 95% of light
      
      concentration_vars[row,'epi_light'] <- light_amount_epi
      concentration_vars[row,'hypo_light'] <- 0
      concentration_vars[row,'td_light'] <- NA
      concentration_vars[row,'meandepth_light'] <- NA
      concentration_vars[row,'lec_epi_value'] <- lec_epi
      
      epi_fluxes[row,'npp_value'] <- params_df[1,'pmax']*(1-exp(-params_df[1,'ip']*light_amount_epi/params_df[1,'pmax']))*tp_epi_adjusted*theta_epi_npp*driver_df[row-1,'volume_epi'] # (gday^-1)  #should this have arhn. eq? -- got rid of tp-carbon coefficient (do we still need it??)
      
      hypo_fluxes[row,'npp_value'] <- 0
      
      
    }else{ ##ice off
      o2_diff <- (driver_df[row-1,'do_sat_value']/driver_df[row-1,'volume_epi']) - (epi_vars[row-1,'do_value']/driver_df[row-1,'volume_epi']) #unit = grams/m^3
      
      epi_fluxes[row, "atm_exhange"] <- driver_df[row-1,'gas_exchange_depth']*(o2_diff)*(lake_info[1,"area_total"]) #g/day

      concentration_vars[row,'o2_diff_value'] <- o2_diff
      concentration_vars[row,'k_o2_value'] <- params_df[1,"k_atm"]
      
      if (is.na(driver_df[row-1,'thermocline_depth'])){
        light_amount_epi <- light_calc(z1=0,z2=lake_info[1,"maxdepth"],a1=lake_info[1,"area_total"],a2=0,ir = driver_df[row-1,'i_rad'], mu = lec_epi) * (1 - params_df[1,"albedo"])
        
        light_amount_hypo <- 0
      }else{
        light_amount_epi <- light_calc(z1=0,z2=driver_df[row-1,'thermocline_depth'],a1=lake_info[1,"area_total"],a2=driver_df[row-1,'area_td'],ir = driver_df[row-1,'i_rad'], mu = lec_epi) * (1 - params_df[1,"albedo"])
        
        #find light amount at thermocline depth -- this will be used for the initial irradiance value for the hypo light amount
        light_td <-  driver_df[row-1,'i_rad'] * exp(-(lec_epi)*driver_df[row-1,'thermocline_depth']) * (1 - params_df[1,"albedo"])
        md_light <- driver_df[row-1,'i_rad'] * exp(-(lec_epi)*(lake_morphology$volume_total/lake_morphology$area)) * (1 - params_df[1,"albedo"])## assume volume/area is meandepth
        
        #save light info
        concentration_vars[row,'td_light'] <- light_td
        concentration_vars[row,'meandepth_light'] <- md_light

        light_amount_hypo <- light_calc(z1=driver_df[row-1,'thermocline_depth'],z2= (lake_info[1,"maxdepth"]),a1=driver_df[row-1,'area_td'],a2=0,ir = light_td, mu = lec_hypo)
        }
      concentration_vars[row,'epi_light'] <- light_amount_epi
      concentration_vars[row,'hypo_light'] <- light_amount_hypo
      
      hypo_fluxes[row,'npp_value'] <- params_df[1,'pmax']*(1-exp(-params_df[1,'ip']*light_amount_hypo/params_df[1,'pmax']))*tp_hypo_adjusted*theta_hypo_npp*driver_df[row-1,'volume_hypo']
      epi_fluxes[row,'npp_value'] <- params_df[1,'pmax']*(1-exp(-params_df[1,'ip']*light_amount_epi/params_df[1,'pmax']))*tp_epi_adjusted*theta_epi_npp*driver_df[row-1,'volume_epi'] # (gday^-1) -- got rid of tp-carbon coefficient
      
      }
  
    if (is.na(driver_df[row,'thermocline_depth'])){
      sed_area_epi <- lake_info[1,'area_total']
      sed_area_hypo <- 0
      epi_sed_rate <- params_df[1,"resp_sed_hypo"]
    }else{
      sed_area_epi <- lake_info[1,'area_total'] - driver_df[row-1,'area_td']
      sed_area_hypo <- driver_df[row-1,'area_td']
      epi_sed_rate <- params_df[1,"resp_sed_epi"]
    }
    
    # CALCULATE RESPIRATION TERMS
    epi_fluxes[row,'resp_sed'] <- epi_sed_rate*theta_epi_resp*mm_do_epi*sed_area_epi
    epi_fluxes[row,'resp_poc_r'] <- epi_vars[row-1,'poc_r_value']*params_df[1,"resp_poc_r"]*theta_epi_resp*mm_do_epi
    epi_fluxes[row,'resp_poc_l'] <- epi_vars[row-1,'poc_l_value']*params_df[1,"resp_poc_l"]*theta_epi_resp*mm_do_epi
    epi_fluxes[row,'resp_doc_r'] <- epi_vars[row-1,'doc_r_value']*params_df[1,"resp_doc_r"]*theta_epi_resp*mm_do_epi
    epi_fluxes[row,'resp_doc_l'] <- epi_vars[row-1,'doc_l_value']*params_df[1,"resp_doc_l"]*theta_epi_resp*mm_do_epi
    epi_fluxes[row,'resp_total'] <-  epi_fluxes[row,'resp_poc_l'] + epi_fluxes[row,'resp_poc_r'] + epi_fluxes[row,'resp_doc_l'] + epi_fluxes[row ,'resp_doc_r']
    
    hypo_fluxes[row,'resp_sed'] <- params_df[1,"resp_sed_hypo"]*theta_hypo_resp*mm_do_hypo*sed_area_hypo
    hypo_fluxes[row,'resp_poc_r'] <- as.numeric(hypo_vars[row-1,'poc_r_value']*params_df[1,"resp_poc_r"]*theta_hypo_resp*mm_do_hypo)
    hypo_fluxes[row,'resp_poc_l'] <- as.numeric(hypo_vars[row-1,'poc_l_value']*params_df[1,"resp_poc_l"]*theta_hypo_resp*mm_do_hypo)
    hypo_fluxes[row,'resp_doc_r'] <- as.numeric(hypo_vars[row-1,'doc_r_value']*params_df[1,"resp_doc_r"]*theta_hypo_resp*mm_do_hypo)
    hypo_fluxes[row,'resp_doc_l'] <- as.numeric(hypo_vars[row-1,'doc_l_value']*params_df[1,"resp_doc_l"]*theta_hypo_resp*mm_do_hypo)
    hypo_fluxes[row,'resp_total'] <- hypo_fluxes[row,'resp_poc_l'] + hypo_fluxes[row,'resp_poc_r'] + hypo_fluxes[row,'resp_doc_l'] + hypo_fluxes[row,'resp_doc_r']
    

    concentration_vars[row,'sed_area_epi'] <- sed_area_epi
    concentration_vars[row,'sed_area_hypo'] <- sed_area_hypo
    
    ### ENTRAINMENT EQUATIONS ###

    v_entr <- driver_df[row,'volume_epi'] - driver_df[row-1,'volume_epi']
    
    mixing_check <- 0 ##set condition for mixing
    
    if(v_entr > 0){ ##this is the case where epi is growing
      if(driver_df[row,'volume_hypo'] == 0){ ##case immediately following fall mixing...ventr is nonzero (epi volume changes between two days) but current hypo volume is zero
        epi_fluxes[row,'poc_l_entrainment'] <- hypo_vars[row-1,"poc_l_value"]
        epi_fluxes[row,'poc_r_entrainment'] <- hypo_vars[row-1,"poc_r_value"]
        epi_fluxes[row,'doc_l_entrainment'] <- hypo_vars[row-1,"doc_l_value"]
        epi_fluxes[row,'doc_r_entrainment'] <- hypo_vars[row-1,"doc_r_value"]
        epi_fluxes[row,'do_entrainment'] <- hypo_vars[row-1,"do_value"]
        
        mixing_check <- 1 ##update condition if mixing has occurred
        concentration_vars[row,'entrainment_multiplier'] <- 0

      }else{
      entr_multiplier <- v_entr/driver_df[row-1,'volume_hypo'] 

      epi_fluxes[row,'poc_l_entrainment'] <- entr_multiplier*hypo_vars[row-1,"poc_l_value"] #entrainment for current timestep -- use previous variables for calculation
      epi_fluxes[row,'poc_r_entrainment'] <- entr_multiplier*hypo_vars[row-1,"poc_r_value"]
      epi_fluxes[row,'doc_l_entrainment'] <- entr_multiplier*hypo_vars[row-1,"doc_l_value"]
      epi_fluxes[row,'doc_r_entrainment'] <- entr_multiplier*hypo_vars[row-1,"doc_r_value"]
      epi_fluxes[row,'do_entrainment'] <- entr_multiplier*hypo_vars[row-1,"do_value"]
      
      concentration_vars[row,'entrainment_multiplier'] <- entr_multiplier
      }
    }else if (v_entr < 0){#this is the case where the epi is shrinking
      entr_multiplier <- v_entr/driver_df[row-1,'volume_epi'] 

      epi_fluxes[row,'poc_l_entrainment'] <- entr_multiplier*epi_vars[row-1,"poc_l_value"]
      epi_fluxes[row,'poc_r_entrainment'] <- entr_multiplier*epi_vars[row-1,"poc_r_value"]
      epi_fluxes[row,'doc_l_entrainment'] <- entr_multiplier*epi_vars[row-1,"doc_l_value"]
      epi_fluxes[row,'doc_r_entrainment'] <- entr_multiplier*epi_vars[row-1,"doc_r_value"]
      epi_fluxes[row,'do_entrainment'] <- entr_multiplier*epi_vars[row-1,"do_value"]
      
      concentration_vars[row,'entrainment_multiplier'] <- entr_multiplier
      
    }else if (v_entr == 0){
      epi_fluxes[row,'poc_l_entrainment'] <- 0
      epi_fluxes[row,'poc_r_entrainment'] <- 0
      epi_fluxes[row,'doc_l_entrainment'] <- 0
      epi_fluxes[row,'doc_r_entrainment'] <- 0
      epi_fluxes[row,'do_entrainment'] <- 0
      
    } else {
      print('entrainment error')
    }
    
    
    
    #determine POC settle flux -- add poc input depending on situation (strat/mixed)
    epi_fluxes[row,'poc_l_settle'] <- (params_df[1,"k_pocl_epi"]*epi_vars[row-1,"poc_l_value"])/(driver_df[row-1,'volume_epi']/lake_info[1,'area'])
    epi_fluxes[row,'poc_r_settle'] <- (params_df[1,"k_pocr_epi"]*epi_vars[row-1,"poc_r_value"])/(driver_df[row-1,'volume_epi']/lake_info[1,'area'])
    
    if(is.na(driver_df[row,'thermocline_depth'])){
      hypo_fluxes[row,'poc_l_settle'] <- 0
      hypo_fluxes[row,'poc_r_settle'] <- 0
    }else{
      hypo_fluxes[row,'poc_l_settle'] <- (params_df[1,"k_pocl_epi"]*hypo_vars[row-1,"poc_l_value"])/(driver_df[row-1,'volume_hypo']/driver_df[row-1,'area_td'])
      hypo_fluxes[row,'poc_r_settle'] <- (params_df[1,"k_pocr_hypo"]*hypo_vars[row-1,"poc_r_value"])/(driver_df[row-1,'volume_hypo']/driver_df[row-1,'area_td'])

      }
    
    # FIRST DAY OF STRATIFICATION -- reassign flux values for first day of stratification (based on proportion of epi and hypo)
    if(is.na(driver_df[row,'thermocline_depth']) == FALSE & is.na(driver_df[row-1,'thermocline_depth']) == TRUE){ ## First day of stratification
      
      vol_epi_proportion <- driver_df[row,'volume_epi']/(lake_morphology$volume_total) ##new epi volume/total lake volume
      vol_hypo_proportion <- (1 - vol_epi_proportion)
      
      epi_fluxes[row,'npp_value'] <- epi_fluxes[row,'npp_value']*vol_epi_proportion
      hypo_fluxes[row,'npp_value'] <- epi_fluxes[row,'npp_value']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_sed'] <- 0
      epi_fluxes[row-1,'resp_sed'] <- epi_fluxes[row-1,'resp_sed']*vol_epi_proportion
      hypo_fluxes[row,'resp_sed'] <- epi_fluxes[row-1,'resp_sed']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_poc_l'] <- epi_fluxes[row,'resp_poc_l']*vol_epi_proportion
      hypo_fluxes[row,'resp_poc_l'] <- epi_fluxes[row,'resp_poc_l']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_poc_r'] <- epi_fluxes[row,'resp_poc_r']*vol_epi_proportion
      hypo_fluxes[row,'resp_poc_r'] <- epi_fluxes[row,'resp_poc_r']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_doc_l'] <- epi_fluxes[row,'resp_doc_l']*vol_epi_proportion
      hypo_fluxes[row,'resp_doc_l'] <- epi_fluxes[row,'resp_doc_l']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_doc_r'] <- epi_fluxes[row,'resp_doc_r']*vol_epi_proportion
      hypo_fluxes[row,'resp_doc_r'] <- epi_fluxes[row,'resp_doc_r']*vol_hypo_proportion
      
      epi_fluxes[row,'resp_total'] <- epi_fluxes[row,'resp_poc_l'] + epi_fluxes[row,'resp_poc_r'] + epi_fluxes[row,'resp_doc_l'] + epi_fluxes[row,'resp_doc_r']
      hypo_fluxes[row,'resp_total'] <- hypo_fluxes[row,'resp_poc_l'] + hypo_fluxes[row,'resp_poc_r'] + hypo_fluxes[row,'resp_doc_l'] + hypo_fluxes[row,'resp_doc_r']
      
      epi_fluxes[row,'poc_l_settle'] <- epi_fluxes[row,'poc_l_settle']*vol_epi_proportion
      hypo_fluxes[row,'poc_l_settle'] <- epi_fluxes[row,'poc_l_settle']*vol_hypo_proportion
      
      epi_fluxes[row,'poc_r_settle'] <- epi_fluxes[row,'poc_r_settle']*vol_epi_proportion
      hypo_fluxes[row,'poc_r_settle'] <- epi_fluxes[row,'poc_r_settle']*vol_hypo_proportion
      }
    
    # on first day of mixing hypo, leftover hypo fluxes should be added to epi and then set to zero -- r_sed and poc_settle fluxes set to zero in their respective section 
    if (mixing_check == 1){
      epi_fluxes[row,'npp_value'] <- epi_fluxes[row,'npp_value'] + hypo_fluxes[row,'npp_value']
      epi_fluxes[row,'resp_poc_l'] <- epi_fluxes[row,'resp_poc_l'] + hypo_fluxes[row,'resp_poc_l']
      epi_fluxes[row,'resp_poc_r'] <- epi_fluxes[row,'resp_poc_r'] + hypo_fluxes[row,'resp_poc_r']
      
      epi_fluxes[row,'resp_doc_l'] <- epi_fluxes[row,'resp_doc_l'] + hypo_fluxes[row,'resp_doc_l']
      epi_fluxes[row,'resp_doc_r'] <- epi_fluxes[row,'resp_doc_r'] + hypo_fluxes[row,'resp_doc_r']
      epi_fluxes[row,'resp_total'] <- epi_fluxes[row,'resp_total'] + hypo_fluxes[row,'resp_total']
      
      hypo_fluxes[row,'npp_value'] <- 0
      hypo_fluxes[row,'resp_poc_l'] <- 0
      hypo_fluxes[row,'resp_poc_r'] <- 0
      hypo_fluxes[row,'resp_doc_l'] <- 0
      hypo_fluxes[row,'resp_doc_r'] <- 0
      hypo_fluxes[row,'resp_total'] <- 0
    }
    
    ############## OC GOVERNING EQUATIONS ############################# OC GOVERNING EQUATIONS ###################### OC GOVERNING EQUATIONS ##################

    epi_fluxes[row,'pocl_production'] <- epi_fluxes[row,'npp_value']*params_df[1,"c_npp_split"]
    epi_fluxes[row,'docl_production'] <- epi_fluxes[row,'npp_value']*(1-params_df[1,"c_npp_split"])
    
    
    ####POC_L 
    epi_vars[row,'poc_l_value'] <- epi_vars[row-1,"poc_l_value"] + epi_fluxes[row,'pocl_production'] - epi_fluxes[row,'resp_poc_l'] - epi_fluxes[row,'poc_l_settle'] + epi_fluxes[row,'poc_l_entrainment'] + epi_fluxes[row,'poc_l_in'] - epi_fluxes[row,'poc_l_out']
    
    if(is.na(driver_df[row,'thermocline_depth'])){
      hypo_fluxes[row,'poc_l_settle'] <- 0
      hypo_vars[row,'poc_l_value'] <-  hypo_vars[row-1,"poc_l_value"] + (hypo_fluxes[row,'npp_value']*params_df[1,"c_npp_split"]) - hypo_fluxes[row,'resp_poc_l'] - hypo_fluxes[row,'poc_l_settle'] - epi_fluxes[row,'poc_l_entrainment'] #+ epi_fluxes[row,'poc_l_settle']
    }else{
      hypo_vars[row,'poc_l_value'] <-  hypo_vars[row-1,"poc_l_value"] + (hypo_fluxes[row,'npp_value']*params_df[1,"c_npp_split"]) - hypo_fluxes[row,'resp_poc_l'] - hypo_fluxes[row,'poc_l_settle'] - epi_fluxes[row,'poc_l_entrainment'] + epi_fluxes[row,'poc_l_settle']
    }

    ###test for large negative fluxes
    sum_pocl_fluxes <- (hypo_fluxes[row,'npp_value']*params_df[1,"c_npp_split"]) - hypo_fluxes[row,'resp_poc_l'] + epi_fluxes[row,'poc_l_settle'] - hypo_fluxes[row,'poc_l_settle'] - epi_fluxes[row,'poc_l_entrainment']

  if((hypo_vars[row-1,'poc_l_value'] + sum_pocl_fluxes < 0) | mixing_check == 1){
      hypo_vars[row,'poc_l_value'] <- 0
      epi_vars[row,'poc_l_value'] <- epi_vars[row-1,"poc_l_value"] + epi_fluxes[row,'pocl_production'] - epi_fluxes[row,'resp_poc_l'] - epi_fluxes[row,'poc_l_settle'] + hypo_vars[row-1,'poc_l_value'] + epi_fluxes[row,'poc_l_in']- epi_fluxes[row,'poc_l_out']
      epi_fluxes[row,'poc_l_entrainment'] <- hypo_vars[row-1,'poc_l_value'] ##update entrainment value
    }

    ####POC_R 
    epi_vars[row,'poc_r_value'] <- epi_vars[row-1,"poc_r_value"] - epi_fluxes[row,'resp_poc_r'] - epi_fluxes[row,'poc_r_settle'] + epi_fluxes[row,'poc_r_entrainment'] + epi_fluxes[row,'poc_r_in'] - epi_fluxes[row,'poc_r_out']
    
    if(is.na(driver_df[row,'thermocline_depth'])){
      hypo_fluxes[row,'poc_r_settle'] <- 0
      hypo_vars[row,'poc_r_value'] <-  hypo_vars[row-1,"poc_r_value"] - hypo_fluxes[row,'resp_poc_r'] - hypo_fluxes[row,'poc_r_settle'] - epi_fluxes[row,'poc_r_entrainment'] #+ epi_fluxes[row,'poc_r_settle']
    }else{
      hypo_vars[row,'poc_r_value'] <-  hypo_vars[row-1,"poc_r_value"] - hypo_fluxes[row,'resp_poc_r'] - hypo_fluxes[row,'poc_r_settle'] + epi_fluxes[row,'poc_r_settle'] - epi_fluxes[row,'poc_r_entrainment']
    }
    
    ###test for large negative fluxes
    sum_pocr_fluxes <-  epi_fluxes[row,'poc_r_settle'] - hypo_fluxes[row,'resp_poc_r'] - hypo_fluxes[row,'poc_r_settle'] - epi_fluxes[row,'poc_r_entrainment']

    if((hypo_vars[row-1,'poc_r_value'] + sum_pocr_fluxes < 0) | mixing_check == 1){
      
      hypo_vars[row,'poc_r_value'] <- 0
      epi_vars[row,'poc_r_value'] <- epi_vars[row-1,"poc_r_value"] - epi_fluxes[row,'resp_poc_r'] - epi_fluxes[row,'poc_r_settle'] + hypo_vars[row,'poc_r_value'] + epi_fluxes[row,'poc_r_in'] - epi_fluxes[row,'poc_r_out']
      epi_fluxes[row,'poc_r_entrainment'] <- hypo_vars[row-1,'poc_r_value'] ##update entrainment value
    }
    
    if(is.na(driver_df[row,'thermocline_depth'])){
      hypo_fluxes[row,'pocr_sediment_flux'] <- epi_fluxes[row,'poc_r_settle']
      hypo_fluxes[row,'pocl_sediment_flux'] <- epi_fluxes[row,'poc_l_settle']
      hypo_fluxes[row,'poc_total_sediment_flux'] <- epi_fluxes[row,'poc_r_settle'] + epi_fluxes[row,'poc_l_settle'] #all settling POC 
      }else{
        hypo_fluxes[row,'pocr_sediment_flux'] <- hypo_fluxes[row,'poc_r_settle']
        hypo_fluxes[row,'pocl_sediment_flux'] <- hypo_fluxes[row,'poc_l_settle']
        hypo_fluxes[row,'poc_total_sediment_flux'] <- hypo_fluxes[row,'poc_r_settle'] + hypo_fluxes[row,'poc_l_settle'] #all settling POC 
        }
    
    ###budget check 
    pocl_budget <- (epi_fluxes[row,'pocl_production'] + (hypo_fluxes[row,'npp_value']*params_df[1,"c_npp_split"])) -
      (epi_fluxes[row,'resp_poc_l'] + hypo_fluxes[row,'resp_poc_l']) + 
      epi_fluxes[row,'poc_l_in'] -
      epi_fluxes[row,'poc_l_out'] - 
      hypo_fluxes[row,'pocl_sediment_flux'] -
      (epi_vars[row,'poc_l_value'] - epi_vars[row-1,'poc_l_value'])
    concentration_vars[row,'pocl_budget'] <- pocl_budget 
    
    pocr_budget <- epi_fluxes[row,'poc_r_in'] -
      epi_fluxes[row,'poc_r_out'] - 
      (epi_fluxes[row,'resp_poc_r'] + hypo_fluxes[row,'resp_poc_r']) - 
      hypo_fluxes[row,'pocr_sediment_flux'] -
      (epi_vars[row,'poc_r_value'] - epi_vars[row-1,'poc_r_value'])
    concentration_vars[row,'pocr_budget'] <- pocr_budget 
    
    
    ##DOC_L
    epi_vars[row,'doc_l_value'] <- epi_vars[row-1,'doc_l_value'] + epi_fluxes[row,'docl_production'] - epi_fluxes[row,'resp_doc_l'] + epi_fluxes[row,'doc_l_entrainment'] + epi_fluxes[row,'doc_l_in'] - epi_fluxes[row,'doc_l_out']
    sum_doc_l_fluxes <- hypo_fluxes[row,'npp_value']*(1-params_df[1,"c_npp_split"]) - hypo_fluxes[row,'resp_doc_l'] - epi_fluxes[row,'doc_l_entrainment']
    if((hypo_vars[row-1,'doc_l_value'] + sum_doc_l_fluxes < 0) | mixing_check == 1){ ###testing for large negative fluxes
      hypo_vars[row,'doc_l_value'] <- 0
      epi_vars[row,'doc_l_value'] <- epi_vars[row-1,'doc_l_value'] + epi_fluxes[row,'docl_production'] - epi_fluxes[row,'resp_doc_l'] + hypo_vars[row-1,'doc_l_value'] + epi_fluxes[row,'doc_l_in'] - epi_fluxes[row,'doc_l_out'] #move any leftover mass from hypo to epi
      epi_fluxes[row,'doc_l_entrainment'] <- hypo_vars[row-1,'doc_l_value'] ##update entrainment value
      }else{
      hypo_vars[row,'doc_l_value'] <- hypo_vars[row-1,'doc_l_value'] + (hypo_fluxes[row,'npp_value']*(1-params_df[1,"c_npp_split"])) - hypo_fluxes[row,'resp_doc_l'] - epi_fluxes[row,'doc_l_entrainment']    
    }
    
    
    ##DOC_R 
    epi_vars[row,'doc_r_value'] <- epi_vars[row-1,'doc_r_value'] - epi_fluxes[row,'resp_doc_r'] + epi_fluxes[row,'doc_r_entrainment'] + epi_fluxes[row,'doc_r_in'] - epi_fluxes[row,'doc_r_out']
    
    sum_doc_r_fluxes <- hypo_fluxes[row,'resp_doc_r'] - epi_fluxes[row,'doc_r_entrainment']
    if((hypo_vars[row-1,'doc_r_value'] + sum_doc_r_fluxes < 0) | mixing_check == 1){ ###testing for large negative fluxes
      hypo_vars[row,'doc_r_value'] <- 0
      epi_vars[row,'doc_r_value'] <- epi_vars[row-1,'doc_r_value'] - epi_fluxes[row,'resp_doc_r'] + hypo_vars[row-1,'doc_r_value'] + epi_fluxes[row,'doc_r_in'] - epi_fluxes[row,'doc_r_out'] #move any leftover mass from hypo to epi
      epi_fluxes[row,'doc_r_entrainment'] <- hypo_vars[row-1,'doc_r_value'] ##update entrainment value
      }else{
      hypo_vars[row,'doc_r_value'] <- hypo_vars[row-1,'doc_r_value'] - hypo_fluxes[row,'resp_doc_r'] - epi_fluxes[row,'doc_r_entrainment']
      }

    
    budget_docr <- epi_fluxes[row,'doc_r_in'] + epi_fluxes[row,'doc_r_entrainment'] - epi_fluxes[row,'resp_doc_r'] - epi_fluxes[row,'doc_r_out'] - (epi_vars[row,'doc_r_value'] - epi_vars[row-1,'doc_r_value'])

    if (budget_docr > 0.0001){
      print('over budget for DOCR')
      print(budget_docr)
      print(driver_df[row,'sampledate'])
      break
    }
    
    concentration_vars[row,'docr_budget'] <- budget_docr
    
    ##DO 
    epi_vars[row,'do_value'] <- epi_vars[row-1,'do_value'] + (epi_fluxes[row,'npp_value']*(32/12)) - (epi_fluxes[row,'resp_total']*(32/12)) - (epi_fluxes[row,'resp_sed']*(32/12)) + epi_fluxes[row, "atm_exhange"] + epi_fluxes[row,'do_entrainment']
    sum_do_fluxes <- hypo_fluxes[row,'npp_value']*(32/12) - (hypo_fluxes[row,'resp_total']*(32/12)) - (hypo_fluxes[row,'resp_sed']*(32/12)) - epi_fluxes[row,'do_entrainment']
    if((hypo_vars[row-1,'do_value'] + sum_do_fluxes < 0) | mixing_check == 1){ ###testing for large negative fluxes
      hypo_vars[row,'do_value'] <- 0
      epi_vars[row,'do_value'] <- epi_vars[row-1,'do_value'] + (epi_fluxes[row,'npp_value']*(32/12)) - (epi_fluxes[row,'resp_total']*(32/12)) - (epi_fluxes[row,'resp_sed']*(32/12)) + epi_fluxes[row, "atm_exhange"] + hypo_vars[row-1,'do_value']
      epi_fluxes[row,'do_entrainment'] <- hypo_vars[row-1,'do_value'] ##update entrainment value

      }else{
      hypo_vars[row,'do_value'] <- hypo_vars[row-1,'do_value'] + (hypo_fluxes[row,'npp_value']*(32/12)) - (hypo_fluxes[row,'resp_total']*(32/12)) - (hypo_fluxes[row,'resp_sed']*(32/12)) - epi_fluxes[row,'do_entrainment'] 
      }
    
  }#for loop end
  
  return(list(epi_vars, hypo_vars, epi_fluxes, hypo_fluxes, concentration_vars))
  
}#function end