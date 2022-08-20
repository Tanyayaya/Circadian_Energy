library(lubridate)

act_file_path = './simulation_data'
act_files = list.files(act_file_path, full.names=FALSE, recursive=TRUE)
act_data_list = list()
for (i in 1:length(act_files)){
  file = act_files[i]
  data = read.csv(paste(act_file_path, file, sep='/'),header=TRUE,
                  colClasses = c("character","numeric"))
  data$time = as.POSIXct(data$time)
  act_data_list[[file]] = data
}

num_samp = length(act_files)
time2dectime <- function(time){
  if(any(c("POSIXct","POSIXt")%in%class(time))){
    # if we have POSIX input, make it a string to meet our (bad)
    # assumptions later...
    time <- strftime(time,"%H:%M")
  }
  if (is.numeric(time)) {
    # horrible unchecked assumption that if input is numeric 
    # it's already decimal 24h time and can be returned
    dectime <- time
  } else {
    # horrible unchecked assumption that otherwise we have
    # times in %H:%M string format -- note that 2:30PM will fail!
    hr_min <- strsplit(as.character(time),":")
    hr <- as.numeric(sapply(hr_min,`[`,1))
    min <- as.numeric(sapply(hr_min,`[`,2))
    min[is.na(min)] <- 0
    dectime <- hr+min/60
  }
  return(dectime)
}

hourly_data_list = list()
for(i in 1:num_samp){
  data = act_data_list[[i]]
  data[["Hours"]] = hour(data$time)
  data[["Date"]] = yday(data$time)
  data = aggregate(activity ~ Hours+Date, data, FUN= sum)
  data[["Date"]] = data[["Date"]]- 1 + as.Date("2019-01-01")
  hourly_data_list[[i]] = data
}

act_num_days = c()
for(i in 1:num_samp){
  act_num_days[i] = nrow(hourly_data_list[[i]])/24
}


