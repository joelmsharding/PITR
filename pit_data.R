# Need to create custom function library for this later
source("/Volumes/750 GB HD/Users/joelharding/Desktop/R/Code/functionlibrary.R")

#setwd("/Volumes/750 GB HD/Users/joelharding/Dropbox (Instream)/Projects/62 - PIT R & D/3 - Analyses")
setwd("/Volumes/750 GB HD/Users/joelharding/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/Bridge River")


#Import PIT txt files: have to specify 9 columns to avoid dropping last column

path_to_folder<- "~/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/Bridge River"

#User enters TEST PIT TAG numbers to filter out below
test<- c("")


pit_dat<- function(path_to_folder, test) {

test<- ""

counter_paths <- dir(path_to_folder, full.names = TRUE)
names(counter_paths) <- basename(counter_paths)


x<- plyr::ldply(counter_paths,
                      read.table, 
                      header=FALSE, 
                      fill=TRUE, 
                      stringsAsFactors=FALSE,
                      col.names=c("det_type", "date", "time", "dur", "tag_type", "tag_code", "antenna", "consec_det", "no_empt_scan_prior"))


#Separate detections (D) from events (E)

########################
########################
########################
########################

#DETECTIONS
d1<-filter(x,x$det_type=="D")

#Make blank values in last column NA (single antennas do not have 'antenna' values so last two columns are shifted down)
d1[d1==""] <- NA


###START HERE NEED TO SORT OUT WHY BRIDGE COUNTER ROWS WITH ANTENNA VALUES 3 AND 30 ARE GETTING INTO SINGLE ANTENNA FILE BELOW

#################
#Select all rows that are from single PIT antennas
#################

sa1<- filter(d1, !(antenna %in% c("A1","A2", "A3", "A4")))
sa2<- filter(sa1, !(is.na(antenna)))

#Change column names and shift order to match multiple antenna data
names(sa2) [8]<- c("consec_det")
names(sa2) [9]<- c("no_empt_scan_prior")
sa2$antenna<- "NA"
sa3<- data.frame(sa2[,c(1:7,11,8,9)])

# consec_det and no_empt_scan_prior columns to numeric
sa3$consec_det<- as.numeric(sa3$consec_det)
sa3$no_empt_scan_prior<- as.numeric(sa3$no_empt_scan_prior)

#Remove duplicate rows
sa<- sa3 %>%
  distinct(.id, date, time, dur, tag_type, tag_code, consec_det, no_empt_scan_prior)

#Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
sa_err<- filter(d1,is.na(antenna))

#Create list of filenames for single antennas for user to double check
sa_filenames<- unique(sa1$.id)

#################
#Select all rows that are from multiplexer PIT
#################

ma1<- filter(d1,complete.cases(no_empt_scan_prior))
#Filter proper rows
ma2<- filter(ma1,antenna %in% c("A1","A2", "A3", "A4"))

#Remove duplicate rows
ma<- ma2 %>%
  distinct(.id, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior)

#Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
ma_err<- filter(ma1,!(antenna %in% c("A1","A2", "A3", "A4")))

#Create list of filenames for single antennas for user to double check
ma_filenames<- unique(ma2$.id)

########################
########################
########################
########################

#EVENTS
e1<-filter(x,x$det_type=="E")

#Make new object with columns from E that are separated text
text<- select(e1,5:10)
#Combine these columns back into matrix of text strings
desc<-apply(text, 1, paste, collapse=" ")

#Insert combined text to replace sepearate text columns in E
e2<- cbind(e1[,c(1:4)], desc)
#Convert combined text from factor to character
e2<-mutate(e2,desc=as.character(desc))
#Remove NA and extra spaces at end of combined text
e2$desc<- gsub("NA", "", paste(e2$desc))
#Trim white space at beginning and end of character strings
e2$desc<- trimws(e2$desc, which = c("both"))

#Remove duplicate rows
e<- e2 %>%
  distinct(.id, date, time, desc)

#Filter voltage events for voltage plots
v<- filter(e, grepl('V',desc))

#START HERE, NEED TO FIND THE INVERSE OF 'DISTINCT' TO IDENTIFY DUPLICATE ROWS!
# NEED TO EXAMINE O1 TO SEE WHAT DETECTIONS AND EVENTS WERE OMITTED

########################
########################
########################
########################

#OTHER (Not D and not E)

o1<- filter(x, !(det_type %in% c("D","E")))

assign("volt_dat", v, envir=globalenv())
assign("event_dat", e, envir=globalenv())
assign("other", o1, envir=globalenv())
return(rbind(sa,ma))

}

xx<- pit_dat("~/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/Bridge River")

# Refer to http://support.oregonrfid.com/support/solutions/articles/5000006373-datalogger-record-format for explanation of record formatting

# Add a check for data types for each columns eg:are all antenna values charaters etc.?
# Add unique qualifier to avoid duplicate records (contingent on tag_ID, date and time?)
# Categories for presence at each antenna and directional movement?
# Single antenna or multi-plex?
#Create a subset of Events
#Create voltage plots from events file and relationships with detection efficiency
