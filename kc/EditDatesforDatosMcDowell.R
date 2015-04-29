

# Load the project
project_directory <- "C:/Users/leonmi/Google Drive/Discharge" # library(devtools)
#project_directory <- "C:/Users/aa20/Dropbox/Manuscripts/Composite Method/composite_client"
library(ProjectTemplate)
#source(paste0(project_directory,"/","lib/loadProject.R"))
setwd(project_directory)
#source("C:/Users/leonmi/Google Drive/loadest/composite_client/src/scripts/reportHelpers.R")

# Redefine project_directory now that loadProjectFromCache has cleared the old variable
project_directory <- "C:/Users/leonmi/Google Drive/Discharge" 
#project_directory <- "C:/Users/aa20/Dropbox/Manuscripts/Composite Method/composite_client"

# Set options for producing the html filee
library(knitr)
opts_chunk$set(echo=FALSE, message=FALSE, warn=FALSE)
options(warn=-1)

# Load libraries we'll use, suppressing messages via message=FALSE option to knitr chunk
library(MASS)
library(rloadest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
#MPRDischargeFrom1999


newnames <- as.vector(t(read.csv(
  "DatosMcDowell98-02-2.csv",
  sep=",",header=FALSE,skip=0,nrows=1,stringsAsFactors=FALSE)[1,]))

DatosMcDowell <- read.csv(
  "DatosMcDowell98-02-2.csv",
  sep=",",header=FALSE,col.names=newnames,skip=1,stringsAsFactors=FALSE)

#Sept,Oct,Nov,Dec,Jan,Feb,March,April,May, June, July, Aug


# 
# tmpMonth <- grep("^.*\\.", DatosMcDowell$Dateof,value=TRUE)


#DatosMcDowell$Dateof <- as.character(DatosMcDowell$Dateof)
tmpMonth <- strsplit(DatosMcDowell$Dateof,"\\.")
tmpMonth2 <- sapply(tmpMonth,"[",1) 
DatosMcDowell$Month <- tmpMonth2
tmpDateYr <- sapply(tmpMonth,"[",2) 
DatosMcDowell$DateYr <- tmpDateYr
#tmpMonth2 <- grep("+[[:blank:]]\\.{0}", DatosMcDowell$Dateof,value=TRUE)

#sapply(strsplit(Test,"\\.",fixed = TRUE),"[[",2) 
DatosMcDowell$Month <- ifelse(DatosMcDowell$Month =="Sept","Sep", DatosMcDowell$Month) 
DatosMcDowell$MonthNum<- match(DatosMcDowell$Month, month.abb)

tmpMonth <- strsplit(DatosMcDowell$Dateof," ")
tmpMonth2 <- sapply(tmpMonth,"[",1) 
DatosMcDowell$Month2 <- tmpMonth2



DatosMcDowell$commacount <- str_count(DatosMcDowell$DateYr, ',')
DatosMcDowell$tmpDay <- strsplit(DatosMcDowell$DateYr,",")
#extract year 
DatosMcDowell$tmpyear2 <- ifelse(DatosMcDowell$commacount==1,sapply(DatosMcDowell$tmpDay,"[",2) ,NA)

#extract day of month 
DatosMcDowell$tmpDay2 <- ifelse(DatosMcDowell$commacount==1,sapply(DatosMcDowell$tmpDay,"[",1) ,NA)
DatosMcDowell$tmpDay2 <- ifelse(DatosMcDowell$commacount==2,sapply(DatosMcDowell$tmpDay,"[",2) ,DatosMcDowell$tmpDay2)

#now for tmpDateYr2
#extract date
tmpDateYr2 <- sapply(tmpMonth,"[",2) 
DatosMcDowell$DateYr2 <- tmpDateYr2

DatosMcDowell$commacount <- str_count(DatosMcDowell$DateYr2, ',')
DatosMcDowell$tmpDay <- strsplit(DatosMcDowell$DateYr2,",")
#extract year
DatosMcDowell$tmpdateStr3<- strsplit(DatosMcDowell$Month,",") 
DatosMcDowell$commacount2 <- str_count(DatosMcDowell$Month, ',')
DatosMcDowell$tmpyear3 <- ifelse(DatosMcDowell$commacount2==1,sapply(DatosMcDowell$tmpdateStr3,"[",2) ,DatosMcDowell$tmpyear2)
DatosMcDowell$Year <- ifelse(is.na(DatosMcDowell$tmpyear3), DatosMcDowell$tmpyear2, DatosMcDowell$tmpyear3)
DatosMcDowell$tmpyear3 <-NULL
DatosMcDowell$tmpyear2 <-NULL
DatosMcDowell$commacount2 <-NULL
DatosMcDowell$tmpdateStr3<- NULL
#extract day of month 
DatosMcDowell$tmpDay3 <- ifelse(DatosMcDowell$commacount==1,sapply(DatosMcDowell$tmpDay,"[",1) ,DatosMcDowell$tmpDay2)
DatosMcDowell$tmpDay3 <- ifelse(DatosMcDowell$commacount==2,sapply(DatosMcDowell$tmpDay,"[",2) ,DatosMcDowell$tmpDay3)

DatosMcDowell$Day <- ifelse(is.na(DatosMcDowell$tmpDay3), DatosMcDowell$tmpDay2, DatosMcDowell$tmpDay3)
#Get Year

DatosMcDowell$tmpDay3 <- NULL
DatosMcDowell$tmpDay2 <- NULL


DatosMcDowell$MonthNum2<- match(DatosMcDowell$Month2, month.abb, nomatch=DatosMcDowell$MonthNum)
DatosMcDowell$MonthNum2<- match(DatosMcDowell$Month2, month.name, nomatch=DatosMcDowell$MonthNum)

DatosMcDowell$MonthNum <- ifelse(is.na(DatosMcDowell$MonthNum),DatosMcDowell$MonthNum2, DatosMcDowell$MonthNum)

DatosMcDowell$MonthNum2<-NULL
DatosMcDowell$Month2<- NULL



#concate Day, MonthNum, Year into date!
DatosMcDowell$TmpDate <- paste0(DatosMcDowell$MonthNum, "/",DatosMcDowell$Day, "/",DatosMcDowell$Year)
DatosMcDowell$Date <- as.POSIXct(strptime(paste0(DatosMcDowell$MonthNum, "/",DatosMcDowell$Day, "/",DatosMcDowell$Year)
                                          ,format="%m/%d/%Y",tz="America/Puerto_Rico"))

DatosMcDowell$tmpDay<- NULL
write.csv(DatosMcDowell,
          "./DatosMcDowell98-02Export2.csv",na="NA",,row.names=TRUE)
