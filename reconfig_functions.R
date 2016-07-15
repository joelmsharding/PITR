#Function that enables users to split one multiplexer into 2+ separate readers (like bridge counter PIT configuration),
# or rename antennas to order properly for detection efficiency and direction functions,
# or combine two single readers into one multiplexer (like bridge reach 3/4 configuration)

source("~/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses/PITR/pit_data_function.R")

#Dat needs to be output from pit_data_function
reader_split<- function(dat, r, ao1, an1, ao2=NULL, an2=NULL, ao3=NULL, an3=NULL){
  rd<- filter(dat,reader == r)
  rr<- filter(dat, reader != r)
  
  if (!is.null(c(ao1,ao2,ao3))){
  rd$reader_new<- ifelse(rd$antenna == c(ao1,ao2,ao3), paste(r,"1",sep="_"), paste(r,"2",sep="_"))
  }
  
  
  if (!is.null(ao1)){
    if(rd$antenna == ao1) {
      rd$antenna_new <- 1
    }
  }
  
  if (!is.null(ao2)){
  if(rd$antenna == ao2) {
    rd$antenna_new <- 2
  }
  }
  
  if (!is.null(ao3)){
  if(rd$antenna == ao3) {
    rd$antenna_new <- 3
  }
  }
  
  if (!is.null(an1)){
  if(rd$antenna == an1) {
    rd$antenna_new <- 1
  }
  }
  
  if (!is.null(an2)){
  if(rd$antenna == an2) {
    rd$antenna_new <- 2
  }
  }
  
  if (!is.null(an3)){
  if(rd$antenna == an3) {
    rd$antenna_new <- 3
  }
  }
  
  #nc<- rbind(rd,rr)
  return(rd)
  
}

rs<- reader_split(f_test, "bridge_counter", ao1=1, an1=2)





#reader_combine<- function(dat,r1,r2,r3=NULL,r4=NULL,r_new)
  
#antenna_config<-