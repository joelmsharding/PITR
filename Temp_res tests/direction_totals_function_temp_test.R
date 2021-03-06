#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#A function that tallies the number of fish that have passed above or below an array

###NEW
dir_total<- function(dat, resolution=NULL, start_date = min(dat$date_time), end_date = max(dat$date_time)){
  
  #Remove single reader rows from data set (created with pit_data function)
  xv<- subset(dat, antenna != "NA")
  
  #Filter data 
  rg <- filter(xv, date_time >= start_date  & date_time <= end_date)
  
  #create new temporal columns
  rg$year<- year(rg$date_time)
  rg$month <- month(rg$date_time)
  rg$week <- week(rg$date_time)
  rg$day <- day(rg$date_time)
  rg$hour <- hour(rg$date_time)
  
  if(resolution == "hour"){ 
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code", "year", "month", "day", "hour"), function(x){
     #Order by date_time
     xx<- x[order(x$date_time),]
     #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
     #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
    
     #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
     xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
    
    data.frame(xx)
  })
    #Remove rows where direction is N
    dir_c<- subset(dir, u_d != "N")
    #Sort by reader, tag code and date-time
    dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
    
    dir_t<- ddply(dir_cs, c("reader", "tag_code", "year", "month", "day", "hour"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$u_d)
      last_det<- max(x$date_time)
      last_dir<- last(x$u_d)
      
      
      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
    
  }
  
  if(resolution == "day"){ 
    
    #create new 'days' column with a posixct format
    rg$day <- as.POSIXct(paste(rg$date, format = "%Y:%m:%d"))
    
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code", "year", "month", "day"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
      
      #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      
      data.frame(xx)
    })
    
    #Remove rows where direction is N
    dir_c<- subset(dir, u_d != "N")
    #Sort by reader, tag code and date-time
    dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
    
    dir_t<- ddply(dir_cs, c("reader", "tag_code", "year", "month", "day"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$u_d)
      last_det<- max(x$date_time)
      last_dir<- last(x$u_d)
      
      
      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
    
  }
  
  if(resolution == "week"){ 
    
    #create new week column
    rg$week <- week(rg$date)
    
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code", "year", "month", "week"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
      
      #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      
      data.frame(xx)
    })
    
    #Remove rows where direction is N
    dir_c<- subset(dir, u_d != "N")
    #Sort by reader, tag code and date-time
    dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
    
    dir_t<- ddply(dir_cs, c("reader", "tag_code", "year", "month", "week"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$u_d)
      last_det<- max(x$date_time)
      last_dir<- last(x$u_d)
      
      
      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
  }
  
  if(resolution == "month"){ 
    
    #create new month column
    rg$month <- month(rg$date_time)
    
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code", "year", "month"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
      
      #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      
      data.frame(xx)
    })
    
    #Remove rows where direction is N
    dir_c<- subset(dir, u_d != "N")
    #Sort by reader, tag code and date-time
    dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
    
    dir_t<- ddply(dir_cs, c("reader", "tag_code", "year", "month"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$u_d)
      last_det<- max(x$date_time)
      last_dir<- last(x$u_d)
      
      
      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
  }
  
  
  if(resolution == "year"){ 
    
    #create new week column
    rg$year <- year(rg$date_time)
    
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code", "year"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
      
      #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      
      data.frame(xx)
    })
    
    #Remove rows where direction is N
    dir_c<- subset(dir, u_d != "N")
    #Sort by reader, tag code and date-time
    dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
    
    dir_t<- ddply(dir_cs, c("reader", "tag_code", "year"), function(x){
      #Order by date_time
      x[order(x$date_time),]
      #tot<- sum(x$tally)
      first_det<- min(x$date_time)
      first_dir<- first(x$u_d)
      last_det<- max(x$date_time)
      last_dir<- last(x$u_d)
      
      
      #Calculate time differences b/w first and last detections
      time_diff <- interval(first_det,last_det)
      time_diff_days<- round(time_diff/ddays(1),2)
      time_diff_mins<- round(time_diff/dminutes(1),2)
      data.frame(first_det, first_dir, last_det, last_dir, time_diff_days, time_diff_mins)
    })
    return(dir_t)
  }
  
  if(is.null(resolution)){ 
    #For each reader/ tag code...
    dir<- ddply(rg, c("reader", "tag_code"), function(x){
      #Order by date_time
      xx<- x[order(x$date_time),]
      #If the diffference between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      #xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
      
      #If the diffference between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
      xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
      
      data.frame(xx)
    })
  }
  
}

toty<- dir_total(rc,"year")
totm<- dir_total(rc,"month")
totw<- dir_total(rc,"week")
totd<- dir_total(rc,"day")
toth<- dir_total(rc,"hour")
