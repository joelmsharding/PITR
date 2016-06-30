#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)


#Sources pit dat function that produces data set called f_test
source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Determines the first and last detection on a given reader/ antenna
first_last <- function(dat){
  
  ddply(dat, c("reader", "antenna", "tag_code"), function(x){
    first_det <- min(x$date_time)
    last_det <- max(x$date_time)
    #Calculate time difference in minutes using lubridate
    time_diff_min <- round((interval(first_det,last_det)/dminutes()),2)
    data.frame(first_det, last_det, time_diff_min)
  })
}

fl<- first_last(f_test)
