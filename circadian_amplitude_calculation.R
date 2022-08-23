library(Rssa)

# functions
nparACT_base <- function (X, SR, cutoff = 1, plot = T, fulldays = T){
    data <- X
    if (is.data.frame(data)==F){
      data = as.data.frame(data)
    }
    if(ncol(data) == 1){
      data[,1] <-  as.numeric(as.character(data[,1]))
      names(data)[1] <- "activity"
    } 
    if(ncol(data) == 2){
      data[,1] <- as.POSIXct(data[,1])
      data[,2] <- as.numeric(as.character(data[,2]))
      names(data)[1] <- "time"
      names(data)[2] <- "activity"
    } 
    if(ncol(data) == 3){
      names(data)[1] <- "date"
      names(data)[2] <- "time"
      names(data)[3] <- "activity"
      data$date <- NULL
      data$time <- as.POSIXct(data$time, format="%H:%M:%S")  
      data$activity <- as.numeric(as.character(data$activity))
    }
    if (any(is.na(data$activity)) == TRUE) stop("Please check your data! It must not contain NAs")
    
    bin_hr <- 60  
    a <- nrow(data) 
    e <- SR*60 ## samples per minute
    m <- bin_hr*SR*60  ## samples per hour
    full_days <- floor(a/(e*bin_hr*24))
    
    ## --- Cut data to full days
    # if (fulldays == T){
    #   data <- data[1:(e*bin_hr*24*full_days),"activity"]
    # }
    a <- nrow(data) 
    b <- floor(a/(SR*60)) ## full minutes recorded
    ## ------------------------------------------
    
    ## ---- Filtering, Cutoff for classification as movement
    nparACT_auxfunctions1$nparACT_filt(data, a, cutoff)
    ## ------------------------------------------
    
    ## ---- Calculate average for each minute (needed if SR != 1/60)
    if (SR != 1/60){
      data_min <- nparACT_auxfunctions1$nparACT_data_min(b, SR, data)
    }  else {
      data_min <- data$activity
    }
    ## ------------------------------------------
    
    ## ---- Calculate hourly averages
    data_hrs <- nparACT_auxfunctions1$nparACT_data_hrs(data, a, m)
    ## -----------------------------------------------------------------------------
    
    ## ---- Plot hourly data
    if (plot == T) {
      nparACT_auxfunctions2$nparACT_plot_hourly(data, data_hrs, SR)
    }
    
    ## ---- IS/IV calculation (based on data_hrs!)
    result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
    IS <- result_ISIV[1]
    IV <- result_ISIV[2]
    ## ---------------------------------------------------------------------------------
    
    ## ---------- Relative Amplitude (RA) calculation
    ## ---- Minutewise averages across 24hrs
    minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
    ## --------------------------------
    
    ## ---- Plot Minutewise averages
    if (plot == T){
      start.time <- NULL
      nparACT_auxfunctions2$nparACT_plot_minaverage(data, minaverage, start.time, a, SR)
    }
    ## --------------------------------      
    
    ## ---- Plot Hourly averages
    if (plot == T){
      nparACT_auxfunctions2$nparACT_plot_hraverage(data, minaverage, start.time, a, SR)
    }
    ## --------------------------------      
    
    ## ---- L5, M10, RA calculation
    result_RA <- nparACT_RAfunctions$nparACT_L5M10(data, minaverage, a, SR)
    # result_RA <- nparACT_RAfunctions$nparACT_L5M10_flex(data, a, SR)
    L5 <- result_RA[1]
    L5_starttime <- result_RA[2]
    M10 <- result_RA[3]
    M10_starttime <- result_RA[4]
    RA <- result_RA[5]
    nparACT_result <- data.frame(IS, IV, RA, L5, L5_starttime, M10, M10_starttime)
    return (nparACT_result)
}

nparACT_RAfunctions <- list(
  nparACT_L5M10 = function(data, minaverage, a, SR){
    ## ---- L5 values 
    L5_matrix <- matrix(NA, 1440)
    data_L5_M10 <- rep(minaverage, 2) 
    for (l in 1:1440){  
      for_L5 <- data_L5_M10[l:(299+l)]
      L5_matrix[l] <- mean(for_L5, na.rm = T)
    }
    ## --------------------------------
    
    ## ---- M10 values (most active 10h period)
    M10_matrix <- matrix (NA, 1440)
    for (m in 1:1440){  
      for_M10 <- data_L5_M10[m:(599+m)]
      M10_matrix[m] <- mean(for_M10, na.rm = T)
    }
    ## --------------------------------
    
    ## ---- Find min of L5 and max of M10
    L5 <- round(min(L5_matrix), digits = 2)
    L5_start <- which.min(L5_matrix)
    
    M10 <- round(max(M10_matrix), digits = 2)
    M10_start <- which.max(M10_matrix)    
    ## --------------------------------
    RA <- round((M10-L5)/(M10+L5), digits = 2)
    result_RA <- data.frame(L5, L5_start, M10, M10_start, RA)
    return(result_RA)
  },
  
  nparACT_L5M10_flex = function(data, a, SR){
    l = SR*60*60*24
    c = a/l
    data_average = matrix(NA, l)
    for (i in 1:l){    
      for_average <- data$activity[c(seq(i,c*1440,l))]
      data_average[i] <- mean(for_average, na.rm = T)
    }
    ## ---- L5 values (least active 5h period)
    L5_matrix <- matrix(NA, l)
    data_L5_M10 <- rep(data_average, 2) 
    for (l in 1:l){  
      for_L5 <- data_L5_M10[l:(60*SR*60*5-1+l)]
      L5_matrix[l] <- mean(for_L5, na.rm = T)
    }
    ## --------------------------------
    
    ## ---- M10 values (most active 10h period)
    M10_matrix <- matrix (NA, l)
    for (m in 1:l){  
      for_M10 <- data_L5_M10[m:(60*SR*60*10-1+m)]
      M10_matrix[m] <- mean(for_M10, na.rm = T)
    }
    ## --------------------------------
    
    ## ---- Find min of L5 and max of M10
    L5 <- round(min(L5_matrix), digits = 2)
    L5_start <- which.min(L5_matrix)
    
    M10 <- round(max(M10_matrix), digits = 2)
    M10_start <- which.max(M10_matrix)
    
    ## --------------------------------
    RA <- round((M10-L5)/(M10+L5), digits = 2)
    result_RA <- data.frame(L5, L5_start, M10, M10_start,RA)
    return(result_RA)
  }
)
nparACT_ISIVfunctions <- list(
  nparACT_ISIV = function(data_hrs, bin_hr){
    n <- nrow(data_hrs)
    p <- 1440/bin_hr  
    l <- 60/bin_hr   
    mean_all <- mean(data_hrs[1:n,]) 
    
    return_IS <- nparACT_ISIVfunctions$nparACT_IS(data_hrs, mean_all, bin_hr)
    
    IS <- return_IS[1]
    n <-  return_IS[3]
    p <-  return_IS[4]
    IV <- nparACT_ISIVfunctions$nparACT_IV(n, data_hrs, mean_all)
    
    result_ISIV <- c(IS, IV)
    return(result_ISIV)
    },
  
    nparACT_IS = function(data_hrs, mean_all, bin_hr){
        ## ---- IS numerator calculation
        result_ISnum <- matrix(NA, nrow = 24) 
        n <- nrow(data_hrs)
        p <- 1440/bin_hr  
        for (h in 1:24){ 
        s <- ceiling(n/p) 
        data_hrs3 <- data_hrs
        data_hrs3[s*p] <- NA 
        data_hrs3 <- matrix(data_hrs3) 
        hrlydat <- data_hrs3[c(seq(h,nrow(data_hrs3),24)),]
        hrlymean <- mean(hrlydat, na.rm = T) 
        x <- (hrlymean-mean_all)^2 
        result_ISnum[h,] <- x 
        }
        ISnum <- sum(result_ISnum)  
        ISnumerator <- n*ISnum
        ## ---- IS 11 calculation
        result_ISdenom <- matrix(NA, nrow = n)  
        for (j in 1:n){
        y <- ((data_hrs[j,]-mean_all)^2)
        result_ISdenom[j,] <- y  
        }
        ISdenom <- sum(result_ISdenom)   
        ISdenominator <- p*ISdenom  
        ## -----------------------------
        IS <- round(ISnumerator/ISdenominator, digits = 2)
        return_IS <- c(IS, ISdenom, n, p)
        return(return_IS)
    },
  
  nparACT_IV = function(n, data_hrs, mean_all){
    result_IVnum <- matrix(NA, nrow = n) 
    for (k in 2:n){
      z <- ((data_hrs[k,]-data_hrs[(k-1),])^2)  
      result_IVnum[k,] <- z  
    }
    IVnum <- sum(result_IVnum, na.rm = T)  
    IVnumerator <- n*IVnum  
    ## ---- IV denominator calculation
    result_ISdenom <- matrix(NA, nrow = n)  
    for (j in 1:n){
      y <- ((data_hrs[j,]-mean_all)^2)
      result_ISdenom[j,] <- y  
    }
    ISdenom <- sum(result_ISdenom)   
    IVdenominator <- (n-1)*ISdenom ## ISdenom can be used!
    ## -----------------------------
    IV <- round(IVnumerator/IVdenominator, digits = 2)
    return(IV)
  }
)

nparACT_auxfunctions1 <- list(
  nparACT_data_hrs = function(data, a, m){
    data_hrs <- matrix(NA, nrow = a/m) 
    for (i in 1:(a/m)){
      subset_h <- data$activity[(((i-1)*m)+1):((i*m))]
      mean_subset_h <- mean(subset_h)
      data_hrs[i] <- mean_subset_h 
    }
    return(data_hrs)
  },
  
  nparACT_data_min = function(b, SR, data){
    data_min <- matrix(NA, b) 
    for (d in 1:b){ 
      subset_min <- data$activity[(((d-1)*(SR*60))+1):((d*(SR*60)))]
      data_min[d] <- mean(subset_min)
    }
    return(data_min)
  },
  
  nparACT_filt = function(data, a, cutoff){
    for (k in 1:a){
      if (data$activity[k] < cutoff){
        data$activity[k] <- 0
      }
    }
  },
  
  nparACT_minaverage = function(b, data_min){
    ## ---- Minutewise averages across 24hrs -> 1440 values
    c <- ceiling(b/1440)  
    data_min[c*1440] <- NA 
    minaverage <- matrix(NA, 1440)
    for (i in 1:1440){    
      for_minaverage <- data_min[c(seq(i,c*1440,1440))]
      minaverage[i] <- mean(for_minaverage, na.rm = T)
    }
    return(minaverage)
  },
  
  ## ---- Hourly averages
  nparACT_hraverage_GA_loop = function(minaverage, data, a , SR){
    hraverage <- matrix(NA)
    for (i in 1:24){
      hraverage [i] <- mean(minaverage[(((i-1)*60)+1):(60*i)])
    }
    daytime <- matrix(NA)
    time <- data$time
    time <- as.character(time)
    for (v in seq(1,a,(SR*60*60))){  
      daytime[v] <- time[v]
    }
    daytime <- na.omit(daytime)
    daytime <- as.character(daytime)
    temp = unlist(str_split(daytime, ' ') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 2) == 0 ] 
    temp = unlist(str_split(timeinfo, ':') )
    temp_nums = 1:length(temp)
    timeinfo = temp[ (temp_nums %% 3) == 1 ] 
    start.time <- as.numeric(timeinfo[1])  
    for_hraverage_time <- rep(seq(1,24),2)
    seq <- seq(start.time, length.out = 24)
    hraverage_time <- for_hraverage_time[seq]
    hraverage_time <- as.numeric(hraverage_time)
    hraverage_time[hraverage_time==24] <- 0
    df_hraverage <- data.frame(hraverage_time, hraverage)
    df_hraverage <- df_hraverage[ order(hraverage_time, hraverage), ]
    hraverage_sorted <- df_hraverage[, 2]
    return(hraverage_sorted)
  }
)
# # ###############################################
# # calculate nonparametric variables
npar_act = matrix(NA, nrow = num_samp, ncol = 7)
for (i in 1:num_samp){
  act = act_data_list[[i]]
  npar_act[i,] = unlist(nparACT_base(act$activity,1/60,plot=F))
}
colnames(npar_act) = c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")
npar_act = as.data.frame(npa)

# calculate circadian energy by SSA
ssa_cc_energy = c()
for (i in 1:num_samp){
    act = act_data_list[[i]]
    # SSA circadian component
    s.act = ssa(act$activity, L = 1440, kind = "1d-ssa")
    cc <- Rssa::reconstruct(s.act, groups = list(c(2,3)))
    cc = cc$F1
    ssa_cc_energy[i] = sum(contributions(s.act)[2:3])
}
scale_mac = npar_act$M10/10^ceiling(log10(max(npar_act$M10)))
adj_ce = ssa_cc_energy/scale_mac

write.csv(adj_ce, file="./results/circadian_energy.csv", row.names=F)
write.csv(npar_act, file="./results/nonparametric_variables.csv", row.names=F)
