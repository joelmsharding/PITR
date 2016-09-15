#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)
#NEED TO CREATE A SEPARATE FUNCTION THAT ALLOWS READERS AND ANTENNAS TO BE CHANGED PROPR TO USING THIS (SEE MY WHITEBOARD)
#CAN THEN RUN THIS FUNCTION ON BRIDGE COUNTER DATA AFTER SPITTING ANTENNAS 1-4 INTO 2 SEPARATE READERS EACH WITH 2 ANTENNAS TO CALCULATE CORRECT DETECTION EFFICIENCIES!


#Sources pit dat function that produces data set called f_test
source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

###################################################################
###ANTENNAS MUST BE ORDERED 1 THROUGH X, DOWNSTREAM TO UPSTREAM###
###################################################################

# TO DO...
#USER SPECIFIED ANTENNA CONFIGURATION IN ADDITION TO FOLLOWING FUNCTION? (i.e calculate detection efficienct for antenna 2 using antennas 3 and 5 for reader x)
#CREATE FUNCTION THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (WEEK,MONTH,YEAR,ALL)
#CREATE FUNCTION THAT ENABLES USER TO SPECIFY ARRAYS AND ANTENNAS (FROM MULTIPLEXERS OR BY COMBINING SINGLE READERS)


########################
########################
########################
########################
#Calculate detection efficiency based on assumptions about fish movement direction (i.e. up, down, resident)

det_eff<- function(dat, direction){

  if(direction=="up"){  
    
    det<- ddply(dat, c("reader","antenna"), function(x){
      
      #select unique tag codes for all antennas upstream of antenna X
      all_tag<- unique(subset(dat, antenna > x$antenna[1],na.rm=TRUE)$tag_code)
      
      #select all unique tag codes for antenna x
      antenna_tag<- unique(x$tag_code, na.rm=TRUE)
      
      #calculate the number of unique tag codes for all antennas UPSTREAM of antenna x
      no_all_tag<- length(unique(subset(dat, antenna > x$antenna[1], na.rm=TRUE)$tag_code))
      
      #the number of tags that are in both antenna x and antennas UPSTREAM of antenna x
      no_unique_tag<- sum(antenna_tag %in% all_tag, na.rm = TRUE)
      
      #calculate detection efficicency: the number of tags detected at antenna x that were 
      #detected at antennas UPSTREAM of x divided by the total number of tags detected UPSTREAM of antenna x
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      
      #Calculate number of tags missed at antenna x
      no_missed_tags<- no_all_tag - no_unique_tag
      
      #create data frame
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }
########################  
  if(direction=="down"){  
    
    det<- ddply(dat, c("reader","antenna"), function(x){
      
      #select unque tag codes for all antennas upstream of antenna X
      all_tag<- unique(subset(dat, antenna < x$antenna[1])$tag_code)
      
      #select all unique tag codes for antenna x
      antenna_tag<- unique(x$tag_code)
      
      #calculate the number of unique tag codes for all antennas DOWNSTREAM of antenna x
      no_all_tag<- length(unique(subset(dat, antenna < x$antenna[1])$tag_code))
      
      #the number of tags that are in both antenna x and antennas DOWNSTREAM of antenna x
      no_unique_tag<- sum(antenna_tag %in% all_tag, na.rm = TRUE)
      
      #calculate detection efficicency: the number of tags detected at antenna x that were 
      #detected at antennas DOWNSTREAM of x divided by the total number of tags detected DOWNSTREAM of antenna x
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      
      #calculate number of tags missed at antenna x
      no_missed_tags<- no_all_tag - no_unique_tag
      
      #create data frame
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }
########################    
  if(direction=="resident"){  
    
    det<- ddply(dat, c("reader","antenna"), function(x){
  
      #select unque tag codes for all antennas upstream of antenna X
      all_tag<- unique(subset(dat, antenna != x$antenna[1])$tag_code)
      
      #select all unique tag codes for antenna x
      antenna_tag<- unique(x$tag_code)
      
      #calculate the number of unique tag codes for antennas OTHER THAN antenna x
      no_all_tag<- length(unique(subset(dat, antenna != x$antenna[1])$tag_code))
      
      #the number of tags that are in both antenna x and antennas OTHER THAN of antenna x
      no_unique_tag<- sum(antenna_tag %in% all_tag, na.rm = TRUE)
      
      #calculate detection efficicency: the number of tags detected at antenna x that were 
      #detected at antennas OTHER THAN x divided by the total number of tags detected OTHER THAN on antenna x 
      det_eff<- round(sum(antenna_tag %in% all_tag, na.rm = TRUE) / no_all_tag, 2)
      
      #calculate number of tags missed at antenna x
      no_missed_tags<- no_all_tag - no_unique_tag
      
      #create data frame
      data.frame(det_eff,no_unique_tag, no_all_tag, no_missed_tags)
      })
    }

#Name columns for output
names(det)<- c("Array", "Antenna","Detection Efficiency", "Shared Detections", "Total Detections", "Missed Detections")
#Filer rows without antenna values (single readers not ID'd as arrays)
det<- filter(det, Antenna != "NA")
return(det)

}

########################
########################
########################
########################

#Run the function for all direction options
det_eff(f_test, "up")

det_eff(f_test, "down")

det_eff(f_test, "resident")
