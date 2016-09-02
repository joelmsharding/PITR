source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")


#Number of unique tag codes detected on each reader - this would be the NULL option

num_codes <- function(dat){
 
    ddply(dat, c("reader"), function(x){
    num_codes <- length(unique(x$tag_code))
    data.frame(num_codes)
    })

}

num_codes(all_det)

####################


#Summarize the data by a specified time resolution (hour, day, week, month, year)
time_res <- function(dat, resolution, start_date = min(dat$date_time), end_date = max(dat$date_time)){
  
  range <- filter(dat, date_time >= start_date  & date_time <= end_date)
  
  if(resolution == "hour"){    
    
    #create new week column
    range$hour <- hour(range$date_time)
  
      det <- ddply(range, c("reader", "hour"), function(x){
    
      #number of unique tag codes per reader  
      num_codes <- length(unique(x$tag_code))
    
      #create data frame
      data.frame(num_codes)
      })
    }


####################
  
  if(resolution == "day"){    
    
    #create new 'days' column with a posixct format
    range$day <- as.POSIXct(paste(range$date, format = "%Y:%m:%d"))

      det <- ddply(range, c("reader", "day"), function(x){
  
      #number of unique tag codes per reader  
      num_codes <- length(unique(x$tag_code))
  
      #create data frame
      data.frame(num_codes)
      })
    }


####################

  if(resolution == "week"){    
    
    #create new week column
    range$week <- week(range$date)
  
      det <- ddply(range, c("reader", "week"), function(x){
    
      #number of unique tag codes per reader  
      num_codes <- length(unique(x$tag_code))
    
      #create data frame
      data.frame(num_codes)
      })
    }

####################
  
  if(resolution == "month"){    
    
    #create new month column
    range$month <- month(range$date_time)
  
      det <- ddply(range, c("reader", "month"), function(x){
    
      #number of unique tag codes per reader  
      num_codes <- length(unique(x$tag_code))
    
      #create data frame
      data.frame(num_codes)
      })
    }

####################

    if(resolution == "year"){    
    
    #create new week column
      range$year <- year(range$date_time)
  
      det <- ddply(range, c("reader", "year"), function(x){
    
      #number of unique tag codes per reader  
      num_codes <- length(unique(x$tag_code))
    
      #create data frame
      data.frame(num_codes)
      })
    }

return(det)
  
}

#run the function for all resolution options
time_res(f_test, "hour")
time_res(f_test, "day")
time_res(f_test, "week")
time_res(f_test, "month")
time_res(f_test, "year")
time_res(f_test, "week", "2015-11-01", "2015-11-30")
