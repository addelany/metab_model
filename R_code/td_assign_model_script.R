############## FUNCTIONS #################

extract_time_space <- function(wtemp){
  time <- as.Date(as.character(wtemp$date))
  depth <- sub("^[^_]*_", "", colnames(wtemp[2:ncol(wtemp)]))
  return(list('datetime' = time, 'depth' = depth))
}

calc_dens <- function(wtemp){
  dens = 999.842594 + (6.793952 * 10^-2 * (wtemp)) - (9.095290 * 10^-3 * (wtemp)^2) + (1.001685 * 10^-4 *(wtemp)^3) - (1.120083 * 10^-6* (wtemp)^4) + (6.536336 * 10^-9 * (wtemp)^5)
  return(dens)
}


calc_td_depth <- function(wtemp){
  
  grd.info <- extract_time_space(wtemp)
  temp <- as.matrix(wtemp[,-c(1)])
  dens <- calc_dens(temp)
  
  cbuoy.depth <- rep(NA, length(grd.info$datetime))
  thermocline_depth <- rep(NA, length(grd.info$datetime))
  
  condition<- apply(temp, 1, FUN=min,na.rm=TRUE) > 4
  
  for (ii in 1:length(cbuoy.depth)){
    idx = !is.na(temp[ii,])
    dens_data = dens[ii,idx]
    dens.diff = rev(dens_data)[1] - dens_data[1]
    
    if (condition[ii] && abs(dens.diff) > 0.05){
      cbuoy.depth[ii] <- center.buoyancy(temp[ii, idx], as.numeric(grd.info$depth[idx]))
      thermocline_depth[ii] <- thermo.depth(temp[ii, idx], as.numeric(grd.info$depth[idx]))
    }
  }
  
  zdeps <- as.numeric(grd.info$depth)
  wlm.depth <- rep(NA, length(grd.info$datetime))
  
  for (ii in 1:length(wlm.depth)){
    idx = !is.na(temp[ii,])
    dens_data = dens[ii,idx]
    dens.diff = rev(dens_data)[1] - dens_data[1]
    
    if (condition[ii] && abs(dens.diff) > 0.05){
      
      Ch <- rep(NA, length = length(dens_data))
      for (jj in 1:(length(dens_data)-1)){
        Ah = 1/(zdeps[jj+1]) * sum(dens_data[1:jj])
        Bh = 1/(zdeps[length(zdeps)] -zdeps[jj]) * sum(dens_data[(jj + 1): length(dens_data)])
        
        diffAh = sum( (dens_data[jj:(jj+1)] - Ah)^2 )
        diffBh = sum( (dens_data[jj:(jj+1)] - Bh)^2 )
        
        Ch[jj] = diffAh + diffBh
      }
      clineDep = zdeps[which.min(na.omit(Ch))]
      wlm.depth[ii] <- clineDep
    }
  }
  

  test <- data.frame('year' = year(grd.info$datetime), 'doy' = yday(grd.info$datetime),
                     'depth' = cbuoy.depth) #wlm.depth
  
  
  for (kk in unique(test$year)){
    idx <- which(kk == test$year)
    
    dx <- test[idx,3]
    dx[which(dx == (max(zdeps-1)))] = NA
    dx[which(dx == 0)] = NA
    
    NonNAindex <- which(!is.na(dx))
    if (length(na.omit(dx)) != 0){
      firstNonNA <- min(NonNAindex)
      lastNonNA <- max(NonNAindex)
      dx[firstNonNA:lastNonNA] =na.approx(dx)
    }
    test[idx,3] <-  dx
  }
  
  
  return(test$depth)#return(cbuoy.depth)#return(test$depth)
}