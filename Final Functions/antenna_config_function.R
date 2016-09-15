# Function that renames antennas to order properly for detection efficiency and direction functions.
# For example if antennas were numbered 2,3,1,4 from downstream to upstream we would need to rename them 1,2,3,4 downstream to upstream

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Dat needs to be output from pit_data_function
antenna_config<- function(dat, r, ao1, ao2, ao3, ao4, an1, an2, an3, an4){
  #Select data that comes from reader you want to reconfig antennas for
  rd<- filter(dat, reader == r)
  #Select all other data to merge back in later
  rr<- filter(dat, reader != r)
  
  #Assign new antenna numbers based on number entries (ao1-> an1, ao2-> an2, ao3 -> an3, a04 -> an4)
  
  #If ao1 exists, give new antenna number an1
  if (!is.null(ao1)){
    x1<- which(rd$antenna == ao1)
    rd$antenna[x1] <- an1
  }
  
  #If ao2 exists, give new antenna number an2
  if (!is.null(ao2)){
    x2<- which(rd$antenna == ao2)
    rd$antenna[x2] <- an2
  }
  
  #If ao3 exists, give new antenna number an3
  if (!is.null(ao3)){
    x3<- which(rd$antenna == ao3)
    rd$antenna[x3] <- an3
  }
  
  #If ao4 exists, give new antenna number an4
  if (!is.null(ao4)){
    x4<- which(rd$antenna == ao4)
    rd$antenna[x4] <- an4
  }
  
  nc<- rbind(rd,rr)
  return(nc)
  
}

#TEST
ar<- antenna_config(f_test, "bridge_counter", 1,2,3,4,4,3,2,1)
