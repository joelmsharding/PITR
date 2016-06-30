#TO DO:
#INTEGRATE CODE THAT ALLOWS USER TO SPECIFY TEMPORAL RESOLUTION (# BY HOUR, DAY, WEEK,MONTH,YEAR,ALL)

#Sources pit dat function that produces data set called f_test
source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#A function that determines upstream or downstream movement of a fish when more than one antenna in place. 
#Also determines whether antennas that detected consequtive detections are adjacent or more than one apart.



direction<- function(dat){
  #Remove single reader rows form data set (breated with pit_data function)
  xv<- subset(dat, antenna != "NA")
  #For each reader/ tag code...
  dir<- ddply(xv, c("reader","tag_code"), function(x){
    #Order by date_time
    xx<- x[order(x$date_time),]
    #If the diffferenc between two consecutive sdettions is positive then up/down (u_d) = up, if it's negative then u_d = down, if it's 0 then u_d = N.  
    xx$u_d<- ifelse(c(0,diff(xx$antenna))>0,"up",ifelse(c(0,diff(xx$antenna))<0, "down", "N"))
    #Calculate the number of antennas apart that consequtuve detections occur
    xx$no_ant<- c(0,abs(diff(xx$antenna)))
    data.frame(xx)
  })
  #Remove rows where direction is N
  dir_c<- subset(dir, u_d != "N")
  #Sort by reader, tag code and date-time
  dir_cs<- dir_c[order(dir_c$reader, dir_c$tag_code, dir_c$date_time),]
  return(dir_cs)
  }
  
dir_test<- direction(f_test)


    