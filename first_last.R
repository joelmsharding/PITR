source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Determines the first and last detection on a given reader/ antenna

#TO DO:
#CREATE FUNCTION THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# HOURS, DAY, WEEK,MONTH,YEAR,ALL)


first_last<- all_det %>%
  group_by(reader,tag_code,antenna) %>%
  summarize(first_det = min(date_time), last_det = max(date_time), time_diff = last_det - first_det)
