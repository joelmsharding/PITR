#Function that enables users to split one multiplexer into 2+ separate readers (like bridge counter PIT configuration),
# or rename antennas to order properly for detection efficiency and direction functions,
# or combine two single readers into one multiplexer (like bridge reach 3/4 configuration)

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Dat needs to be output from pit_data_function
reader_split<- function(dat, r, ao1, ao2=NULL, ao3=NULL, an1, an2=NULL, an3=NULL){
  #Select data that comes from reader you want to split in two
  rd<- filter(dat,reader == r)
  #Select all other data to merge back in later
  rr<- filter(dat, reader != r)
  
  #Assign new reader names based on antenna specifications (ao1,a02,and a03 get "reader"_1; all others get get "reader"_2)
  if (!is.null(c(ao1,ao2,ao3))){
  rd$reader<- ifelse(rd$antenna == c(ao1,ao2,ao3), paste(r,"1",sep="_"), paste(r,"2",sep="_"))
  }
  
  #If ao1 exists, give new antenna number 1 (so it would be antenna 1 on "reader"_1)
  if (!is.null(ao1)){
    x1<- which(rd$antenna == ao1)
    rd$antenna[x1] <- 1
  }
  
  #If ao2 exists, give new antenna number 2 (so it would be antenna 2 on "reader"_1)
  if (!is.null(ao2)){
    x2<- which(rd$antenna == ao2)
    rd$antenna[x2] <- 2
  }
  
  #If ao3 exists, give new antenna number 3 (so it would be antenna 3 on "reader"_1)
  if (!is.null(ao3)){
    x3<- which(rd$antenna == ao3)
    rd$antenna[x3] <- 3
  }
  
  #If an1 exists, give new antenna number 1 (so it would be antenna 1 on "reader"_2)
  if (!is.null(an1)){
    x4<- which(rd$antenna == an1)
    rd$antenna[x4] <- 1
  }
  
  #If an2 exists, give new antenna number 2 (so it would be antenna 2 on "reader"_2)
  if (!is.null(an2)){
    x5<- which(rd$antenna == an2)
    rd$antenna[x5] <- 2
  }
  
  #If an3 exists, give new antenna number 3 (so it would be antenna 3 on "reader"_2)
  if (!is.null(an3)){
    x6<- which(rd$antenna == an3)
    rd$antenna[x6] <- 3
  }
  
  nc<- rbind(rd,rr)
  return(nc)
  
}

#Test
rs<- reader_split(f_test, "bridge_counter", ao1=1, an1=2)
