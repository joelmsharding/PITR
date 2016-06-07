#Determine the first and last detection on a given reader/ antenna

first_last<- all_det %>%
  group_by(reader,tag_code,antenna) %>%
  summarize(first_det = min(date_time), last_det = max(date_time), time_diff = last_det - first_det)
