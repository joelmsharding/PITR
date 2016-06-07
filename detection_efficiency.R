source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data.R")

ma$antenna<- as.numeric(substr(ma$antenna, 2, 2))

########################
########################
########################
########################
#Calculate detection efficiency based on assumptions about fish movement direction (i.e. up, down, resident)

detection_eff<- function(dat, direction){

  if(direction=="up"){  
    
    det<- ddply(dat, c("antenna"), function(x){
  
      all_tag<- unique(subset(dat, antenna > x$antenna[1])$tag_code)
      antenna_tag<- unique(x$tag_code)
      no_all_tag<- length(unique(subset(dat, antenna > x$antenna[1])$tag_code))
      no_unique_tag<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE), 2)
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      no_missed_tags<- no_all_tag - no_unique_tag
      
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }
########################  
  if(direction=="down"){  
    
    det<- ddply(dat, c("antenna"), function(x){
      
      all_tag<- unique(subset(dat, antenna < x$antenna[1])$tag_code)
      antenna_tag<- unique(x$tag_code)
      no_all_tag<- length(unique(subset(dat, antenna < x$antenna[1])$tag_code))
      no_unique_tag<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE), 2)
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      no_missed_tags<- no_all_tag - no_unique_tag
      
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }
########################    
  if(direction=="resident"){  
    
    det<- ddply(dat, c("antenna"), function(x){
  
      all_tag<- unique(subset(dat, antenna != x$antenna[1])$tag_code)
      antenna_tag<- unique(x$tag_code)
      no_all_tag<- length(unique(subset(dat, antenna != x$antenna[1])$tag_code))
      no_unique_tag<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE), 2)
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      no_missed_tags<- no_all_tag - no_unique_tag
      
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }

return(det)  

}

########################
########################
########################
########################

#Run the function for all direction options
detection_eff(ma, "up")

detection_eff(ma, "down")

detection_eff(ma, "resident")
