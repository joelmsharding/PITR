source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#Keeps tag_code in all_det that match tagging data

#upload tagging data
tags <- read.csv("tags.csv")
tags$release_date_time <- as.POSIXct(paste(tags$release_date, tags$release_time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
  
#keep study-specific tag_codes
tag_match <- filter(all_det,tag_code %in% tags$tag_code)

#merge telemetry data with tagging data
tag_all <- merge(tag_match, tags, by = "tag_code")