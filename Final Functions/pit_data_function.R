#TO DO
#Temporal Rersolution not required
# Need to create custom function library for this later

######################
# ALL FILES MUST BE NAMED : pit_reader_mm_dd_yyyy.txt (where pit_reader is unique name of reader)
# MONTH, DAY AND YEAR MUST ALL BE SEPARATED BY UNDERSCORES
# IF .txt  NOT AT END OF FILE NAME IT MUST BE MANUALLY ADDED
#test_tags is a concantinated list of tag codes that were used to test equipment setup and should be removed from data set
######################

#User defines working directory
setwd("~/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/Bridge River")

pit_dat<- function(path_to_folder,test_tags=NULL){

require(stringr)
require(plyr)
require(dplyr)
require(lubridate)

#avoids scientific notation: use "options(scipen=0)" to turn back on
options(scipen=999)

#Function that rbinds rows when nrow of y is > 0
ant_func<- function(x,y) { 
  if(nrow(y) > 0) rbind(x,y) else x}

#Import PIT txt files: have to specify 9 columns to avoid dropping last column
#create directory of files in paths to folder
counter_paths <- dir(path_to_folder, full.names = TRUE)
names(counter_paths) <- basename(counter_paths)

f1<- plyr::ldply(counter_paths,
                 read.table, 
                 header=FALSE, 
                 fill=TRUE,
                 stringsAsFactors=FALSE,
                 skipNul = TRUE,
                 col.names=c("det_type", "date", "time", "dur", "tag_type", "tag_code", "antenna", "consec_det", "no_empt_scan_prior"))

#Create unique ID for each reader from file names
f1$reader<- str_sub(f1$.id,1,-16)

#Create new data frame dropping .id (filname) and moving reader name to left side
x<- data.frame(f1[,c(11,2:10)])


#Separate detections (D) from events (E)

########################
########################
########################
########################

#DETECTIONS
d1<-filter(x,x$det_type=="D")

#Rbind in old data from "Old PIT data conversion.R" if old data present
if(exists("old_dat", envir = .GlobalEnv)) {
  d1 <- rbind(d1,old_dat)
}


#Make blank values in last column NA (single antennas do not have 'antenna' values so last two columns are shifted down)
d1[d1==""] <- NA

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
sa4<- sa3 %>%
  distinct(reader, date, time, dur, tag_type, tag_code, consec_det, no_empt_scan_prior, .keep_all=TRUE)


#Create list of rows that were duplicates
sa_dup<- sa3[duplicated(sa3[1:10]) | duplicated(sa3[1:10], fromLast=TRUE),]

#Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
sa_err<- filter(d1,is.na(antenna))

#Create list of readers for single antennas for user to double check
sa_readers<- unique(sa1$reader)

#################
#Select all rows that are from multiplexer PIT
#################

ma1<- filter(d1,complete.cases(no_empt_scan_prior))
#Filter proper rows
ma2<- filter(ma1,antenna %in% c("A1", "A2", "A3", "A4"))

#Remove duplicate rows
ma3<- ma2 %>%
  distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

#Create list of rows that were duplicates
ma_dup<- ma2[duplicated(ma2[1:10]) | duplicated(ma2[1:10], fromLast=TRUE),]

#Filter rows that have incorrect values for antenna (user can look over and decide to correct in raw data if important)
ma_err<- filter(ma1,!(antenna %in% c("A1","A2", "A3", "A4")))

#Create list of filenames for single antennas for user to double check
ma_readers<- unique(ma2$reader)

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
#e2$desc<- trimws(e2$desc, which = c("both"))

#DB_EDIT########################
#Trim white space at beginning and end of character strings
#This does the same as the previous line of code but couldn't install memsic requried for the trimws function
e2$desc<- gsub("^\\s+|\\s+$", "", e2$desc)
#DB_EDIT#######################

#Remove duplicate rows
e<- e2 %>%
  distinct(reader, date, time, desc, .keep_all=TRUE)

#Create list of rows that were duplicates
e_dup<- e2[duplicated(e2[1:5]) | duplicated(e2[1:5], fromLast=TRUE),]

#Filter voltage events for voltage plots
v<- filter(e, grepl('V',desc))
v$date<- as.Date(v$date)
#Select only voltage numbers for plotting
v$volt<- str_sub(v$desc,-5,-2)
#v$volt<- as.numeric(trimws(v$volt, which = c("both")))

#DB_EDIT########################
#Trim white space at beginning and end of character strings
#This does the same as the previous line of code but couldn't install memsic requried for the trimws function
v$volt<- as.numeric(gsub("^\\s+|\\s+$", "", v$volt))
#DB_EDIT#######################

v$time<- str_sub(v$time,1,8)
v$datetime <- as.POSIXct(paste(v$date, v$time, sep=" "), format="%Y-%m-%d %H:%M:%S")

########################
########################
########################
########################

#OTHER (Not D and not E)
o1<- filter(x, !(det_type %in% c("D","E")))

#Only retain unique rows
o2<- o1%>%
  distinct(reader, date, time, dur, tag_type, tag_code, antenna, consec_det, no_empt_scan_prior, .keep_all=TRUE)

#Select for rows that 'H' in tag_type column (this selects detection that have corrupted data in our test files)
o3<- filter(o2, grepl("H", tag_type))

#Change det_type to D to fix corrupt code
o3$det_type<- "D"

#Select rows that are from multiplexers
ma_re<- o3%>%
  filter(grepl("A",antenna))

#Rbind ma_re to multiplexer data
ma<- ant_func(ma3,ma_re)

#Remove 'A' from antenna values and convert to numeric
ma$antenna<- as.numeric(substr(ma$antenna, 2, 2))

#Select rows that are from single readers
sa_re1<- filter(o3, !(grepl ("A",antenna)))

#Shift numbers down 2 columns to match sa data
names(sa_re1) [8]<- c("consec_det")
names(sa_re1) [9]<- c("no_empt_scan_prior")
sa_re1$antenna<- "NA"
sa_re<- data.frame(sa_re1[,c(1:7,11,8,9)])

#Rbind sa_re to single reader data
sa<- ant_func(sa4,sa_re)


#Combine single and multi reader data if both exist
if(nrow(ma)>0 & nrow(sa)>0) {
  xx <- rbind(ma,sa)
}

if(nrow(ma)==0 & nrow(sa)>0) {
  xx <- sa
}

if(nrow(ma)>0 & nrow(sa)==0) {
  xx <- ma
}

#Remove known test tags if they exist
#xx<- dplyr::filter(xx,!(tag_code %in% test_tags))

if(!(is.null(test_tags))) {
  xx<- filter(xx,!(tag_code %in% test_tags))
}

all_det<- xx

#Make antenna numeric
all_det$antenna<- as.numeric(all_det$antenna)

#Create new column that combines date and time
all_det$date_time <- as.POSIXct(paste(all_det$date, all_det$time, sep=" "), format="%Y-%m-%d %H:%M:%S")

#FUNCTION END CODE
assign("single_data", sa4, envir=globalenv()) #single reader data
assign("multi_data", ma3, envir=globalenv()) #multi reader data
assign("single_duplicate", sa_dup, envir=globalenv()) #single data removed as duplicates
assign("multi_duplicate", ma_dup, envir=globalenv()) #multi data removed as duplicates
assign("single_error", sa_err, envir=globalenv()) #single reader data errors (only based on our test data)
assign("multi_error", ma_err, envir=globalenv()) #multi reader data errors (only based on our test data)
assign("single_readers", sa_readers, envir=globalenv()) #list of all single readers in data set
assign("multi_readers", ma_readers, envir=globalenv()) #list of all multi readers in data set
assign("volt_dat", v, envir=globalenv()) #volateg data used in volt_plot function
assign("event_dat", e, envir=globalenv()) #events data
assign("other_dat", o1, envir=globalenv()) #other data (not detections or events, used can visually scane to make sure no data was missed due to corrupt coding etc.)
return(all_det) # cleaned and combined detection data set
}

#User identifies file path (folder) to data files
ptf<- "~/Dropbox (Instream)/Projects/62 - PIT R & D/5 - Data/Raw PIT Files/Bridge River"

#Define test tags (if any)
tt=c("900_230000010075","900_230000010079","900_230000010080")

f_test<- pit_dat(ptf,tt)

# Refer to http://support.oregonrfid.com/support/solutions/articles/5000006373-datalogger-record-format for explanation of record formatting


