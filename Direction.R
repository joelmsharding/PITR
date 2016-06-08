source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")


dir<- all_det %>%
  group_by(reader) %>%
  arrange(tag_code, date_time) %>%
  mutate(u_d=ifelse(c(0,diff(antenna))>0,"up",ifelse(c("NA",diff(antenna))<0, "down", "N")))




direction<- function(dat){
  filter(dat, antenna != "NA")
  dir<- ddply(dat, c("reader","tag_code"), function(x){
    dplyr::arrange(date_time, antenna)
    u_d<- diff(x$antenna)
  })
  return(u_d)
  }
  
  
    
    