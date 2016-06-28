source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Number of unique tag codes detected on each reader

num_codes <- function(dat){
 
    ddply(dat, c("reader"), function(x){
    num_codes <- length(unique(x$tag_code))
    data.frame(num_codes)
    })

}

num_codes(all_det)

####################

#Summarize the number of unique tag codes by hour
time_res <- function(dat, resolution){
  
  if(resolution == "hour")

    #create new 'hour' column with a posixct format
    dat$hour <- as.POSIXct(paste(dat$date_time, format = "%Y:%m:%d %H:%M:%S"))
    
    #round detection data to nearest hour
    dat$hour <- as.POSIXct(round(dat$hour, "hour"))
    
    ddply(dat, c("reader", "hour"), function(x){
      
    #number of unique tag codes per reader  
    num_codes <- length(unique(x$tag_code))

    #create data frame
    data.frame(num_codes)
    })
    
}

#run function for hours
time_res(all_det, "hour")

####################

#Summarize the number of unique tag codes by day
time_res <- function(dat, resolution){
  
  if(resolution == "day")    
    
    #create new 'days' column with a posixct format
    dat$day <- as.POSIXct(paste(dat$date, format = "%Y:%m:%d"))

    ddply(dat, c("reader", "day"), function(x){
  
    #number of unique tag codes per reader  
    num_codes <- length(unique(x$tag_code))
  
    #create data frame
    data.frame(num_codes)
    })
    
}

#run function for days
time_res(all_det, "day")

####################

#Summarize the number of unique tag codes by week
#Note that this function uses lubridate and returns the (standardized) week of the year (i.e., week 40 of 52 weeks in a year)
time_res <- function(dat, resolution){
  
  if(resolution == "week")    
    
    #create new week column
    dat$week <- week(dat$date)
  
  ddply(dat, c("reader", "week"), function(x){
    
    #number of unique tag codes per reader  
    num_codes <- length(unique(x$tag_code))
    
    #create data frame
    data.frame(num_codes)
  })
  
}

#run function for weeks
time_res(all_det, "week")

####################

#Summarize the number of unique tag codes by month
time_res <- function(dat, resolution){
  
  if(resolution == "month")    
    
    #create new 'months' column
    dat$month <- substring(dat$date, first = 6, last = 7)
  
  ddply(dat, c("reader", "month"), function(x){
    
    #number of unique tag codes per reader  
    num_codes <- length(unique(x$tag_code))
    
    #create data frame
    data.frame(num_codes)
  })
  
}

#run function for months
time_res(all_det, "month")



