source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Determines the first and last detection on a given reader/ antenna

#TO DO:
#CREATE FUNCTION THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# HOURS, DAY, WEEK,MONTH,YEAR,ALL)

first_last <- function(dat){
  
  ddply(dat, c("reader", "antenna", "tag_code"), function(x){
    first_det <- min(x$date_time)
    last_det <- max(x$date_time)
    time_diff <- last_det - first_det
    data.frame(first_det, last_det, time_diff)
  })
  
}

first_last(all_det)