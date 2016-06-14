source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

#number of unique pit codes detected on each reader

num_codes<- all_det %>%
  group_by(reader) %>%
  summarize(num_codes = length(unique(tag_code)))

#should then, if the user so desires, be able to summarize the data by time - some sort of user-defined temporal resolution (hours, days, weeks, months)