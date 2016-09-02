######################
# ALL FILES MUST BE NAMED : pit_reader_mm_dd_yyyy.txt (where pit_reader is unique name of reader)
# IF .txt  NOT AT END OF FILE NAME IT MUST BE NMANUALLY ADDED
# MONTH, DAY AND YEAR MUST ALL BE SEPARTED BY UNDERSCORES
######################

library(stringr)
library(plyr)
library(dplyr)

#avoids scientific notation: use "options(scipen=0)" to turn back on
options(scipen=999)

#User defines working directory
setwd("/Volumes/750 GB HD/Users/joelharding/Dropbox (Instream)/Projects/19 - Keogh River Fish Fence/5 - Data/2016/PIT/Old Format PIT Files")


#Import PIT txt files: have to specify 9 columns to avoid dropping last column

#User identifies location of data files
path_to_folder<- "~/Dropbox (Instream)/Projects/19 - Keogh River Fish Fence/5 - Data/2016/PIT/Old Format PIT Files"

#create directory of files in paths to folder
counter_paths <- dir(path_to_folder, full.names = TRUE)
names(counter_paths) <- basename(counter_paths)

o_f1<- plyr::ldply(counter_paths,
                   read.table, 
                   header=FALSE, 
                   fill=TRUE,
                   stringsAsFactors=FALSE,
                   col.names=c("date", "time", "dur", "tag_type", "tag_code", "antenna", "consec_det", "no_empt_scan_prior"))

#Create unique ID for each reader from file names, make sure all file names have .txt or .log at end
o_f1$reader<- str_sub(o_f1$.id,1,-16)

#Create new data frame dropping .id (filname) and moving reader name to left side
o_x<- data.frame(o_f1[,c(10,2:9)])

#Values other than dates in data file (NEED TO UPDATE THIS SO WE CAN SELECT ANYTHING WITH A DATE FORMAT AND AVOID DOING THIS EACH TIME)
not_date<- c("=~=~=~=~=~=~=~=~=~=~=~=", ">upnew", "upload", "Site", "---------", "0.7A", "0.8A", "1.3A", "1.2A", "1.1A", ">")

#########################
######OLD DETECTIONS#####
#########################

#Assign D to detection based on tag type "R" or "A"
o_x$det_type<- ifelse(o_x$tag_type %in% c("R","A"),"D","NA")

#Filter out detections
o_d<- filter(o_x,det_type=="D")

#Arrange columns to match new data
old_dat<- data.frame(o_d[,c(1,10,2:9)])

#Need to change date format here from MM/DD/YYYY to YYYY-MM-DD
old_dat$date<- as.character(as.Date(o_d1$date, format="%m/%d/%Y"))

####################
######OLD OTHER#####
####################

#Filter other lines (junk)
o_o<- filter(o_x, date %in% not_date)
o_o$det_type<- "O"

#####################
######OLD EVENTS#####
#####################

#Filter event lines
o_e<-filter(o_x, !(date %in% not_date) & det_type =="NA")
o_e$det_type<- "E"

#Arrange columns to match new data
o_e1<- data.frame(o_e[,c(1,10,2:9)])

#Need to change date format here from MM/DD/YYYY to YYYY-MM-DD
o_e1$date<- as.character(as.Date(o_e1$date, format="%m/%d/%Y"))


