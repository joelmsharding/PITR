#Function that enables users to combine up to four single readers into 1 PIT array (like bridge reach 3/4 PIT configuration),

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Dat needs to be output from pit_data_function
reader_combine<- function(dat, name, r1, r2, r3=NULL, r4=NULL){
  #Select data that comes from readers you want to combine
  rd<- filter(dat, reader %in% c(r1,r2,r3,r4))
  #Select all other data to merge back in later
  rr<- filter(dat, !(reader %in% c(r1,r2,r3,r4)))
  
  #Assign new antenna numbers based on readers (r1, r2, r3 and r4 get antenna 3"s 1,2,3 and 4 respectively)
 
  #If r1 exists, give new antenna number 1 (so it would be antenna 1 on "new PIT array")
  if (!is.null(r1)){
    x1<- which(rd$reader == r1)
    rd$antenna[x1] <- 1
  }
  
  #If r2 exists, give new antenna number 2 (so it would be antenna 2 on "new PIT array")
  if (!is.null(r2)){
    x2<- which(rd$reader == r2)
    rd$antenna[x2] <- 2
  }
  
  
  #If r3 exists, give new antenna number 3 (so it would be antenna 3 on "new PIT array")
  if (!is.null(r3)){
    x3<- which(rd$reader == r3)
    rd$antenna[x3] <- 3
  }
  
  #If r4 exists, give new antenna number 4 (so it would be antenna 4 on "new PIT array")
  if (!is.null(r4)){
    x4<- which(rd$reader == r4)
    rd$antenna[x4] <- 4
  }
  
  #All get new reader name (becasue it is being combinerd into one PIT array)
  rd$reader<- name
  
  nc<- rbind(rd,rr)
  return(nc)
  
}

#TEST
rc<- reader_combine(f_test, "REACH34", "bridge_reach34_ant1", "bridge_reach34_ant2")
