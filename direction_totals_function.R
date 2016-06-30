#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)
#currently does not account for differences in fish direction (up equal 1 and down equal -1, but can't differentiate form fish that moved up then down (equals 0) vs. 
#fish that moved downthen up (also equals 0))

#ALSO NOT WORKING PROPERLY BECAUSE BRIDGE COUNTER PIT DATA IS ACTUALLY OPERATING AS TWO SEPARATE MULTIPLEXERS (WHICH WE WILL FIX WITH NEW FUNCTION)

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#A function that tallies the number of fish that have passed above or below an array

#TO DO:
#CREATE FUNCTION THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (HOUR,WEEK,MONTH,YEAR,ALL)

dir_total<- function(dat){
  #Remove single reader rows from data set (created with pit_data function)
  xv<- subset(dat, antenna != "NA")
  #For each reader/ tag code...
  dir<- ddply(xv, c("reader","tag_code"), function(x){
    #Order by date_time
    xx<- x[order(x$date_time),]
    #If the diffferenc between two consecutive detections is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
    xx$tally<- ifelse(c("NA",diff(xx$antenna))>0,1,ifelse(c("NA",diff(xx$antenna))<0, -1, 0))
    data.frame(xx)
  })
  
  #Remove rows where direction is N
  dir_c<- subset(dir, tally != 0)
  #Sort by reader, tag code and date-time
  dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
  
  dir_t<- ddply(dir_cs, c("reader","tag_code"), function(x){
    #Order by date_time
    x[order(x$date_time),]
    tot<- sum(x$tally)
    first_det <- min(x$date_time)
    last_det <- max(x$date_time)
    
    #Calculate time differnces b/w first and last detections
    time_diff <- interval(first_det,last_det)
    time_diff_days<- round(time_diff/ddays(1),2)
    time_diff_mins<- round(time_diff/dminutes(1),2)
    data.frame(tot, first_det, last_det, time_diff_days, time_diff_mins)
    
  })
  return(dir_t)
}

tot<- dir_total(f_test)


