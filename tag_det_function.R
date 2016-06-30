#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)
#THIS IS STILL A WORK IN PROGRESS AS THE FINAL PRODUCT IS MESSY RIGHT NOW, IT PROVIDES A DATA SET WITH REPEATING 
#VALUES OF TAG DEPLOYMENT DATA (FISH LENGTH AT TIME FO TAGGING ETC.) WITH INDIVIDUAL DETECTIONS FOR THAT TAG....
#different DATE_TIME ARE PULLED FROM EACH DATA SET
 
#Sources pit dat function that produces data set called f_test
source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Keeps tag_code in f_test that match tagging data
#Users must have data of tag deplyments with proper date formatting (a column called date and a column called time and column called tag_code) 

tag_match<- function(tags,dat){
tags$date_time <- as.POSIXct(paste(tags$date, tags$time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
  
#keep study-specific tag_codes
tag_match <- filter(dat,tag_code %in% tags$tag_code)

#Rename date_time columns for each data set
names(tag_match)[names(tag_match) == 'date_time'] <- 'date_time_detect'
names(tags)[names(tags) == 'date_time'] <- 'date_time_release'

#Drop separate date an time columns from each data set
drops <- c("date","time")

tag_match<- tag_match[ , !(names(tag_match) %in% drops)]
tags<- tags[ , !(names(tags) %in% drops)]


#merge telemetry data with tagging data
tag_all <- merge(tags, tag_match, by = "tag_code")
return(tag_all)
}

#user imports PIT tagging data (deployment data)
tag_dat <- read.csv("~/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/tags.csv")

#User also specifies pit tag data set (f_test)
#Run function
tm<- tag_match(tag_dat,f_test)
