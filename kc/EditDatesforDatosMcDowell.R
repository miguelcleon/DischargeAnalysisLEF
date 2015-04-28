

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
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

newnames <- as.vector(t(read.csv(
  "DatosMcDowell.csv",
  sep=",",header=FALSE,skip=0,nrows=1,stringsAsFactors=FALSE)[1,]))

DatosMcDowell <- read.csv(
  "DatosMcDowell.csv",
  sep=",",header=FALSE,col.names=newnames,skip=1,stringsAsFactors=FALSE)

#Sept,Oct,Nov,Dec,Jan,Feb,March,April,May, June, July, Aug


# 
# tmpMonth <- grep("^.*\\.", DatosMcDowell$Dateof,value=TRUE)



tmpMonth <- strsplit(DatosMcDowell$Dateof,"\\.")
tmpMonth2 <- sapply(tmpMonth,"[",1) 
DatosMcDowell$Month <- tmpMonth2
#tmpMonth2 <- grep("+[[:blank:]]\\.{0}", DatosMcDowell$Dateof,value=TRUE)

#sapply(strsplit(Test,"\\.",fixed = TRUE),"[[",2) 
DatosMcDowell$Month <- ifelse(DatosMcDowell$Month =="Sept","Sep", DatosMcDowell$Month) 
DatosMcDowell$MonthNum<- match(DatosMcDowell$Month, month.abb)

tmpMonth <- strsplit(DatosMcDowell$Dateof," ")
tmpMonth2 <- sapply(tmpMonth,"[",1) 
DatosMcDowell$Month2 <- tmpMonth2

DatosMcDowell$MonthNum2<- match(DatosMcDowell$Month2, month.abb, nomatch=DatosMcDowell$MonthNum)
DatosMcDowell$MonthNum2<- match(DatosMcDowell$Month2, month.name, nomatch=DatosMcDowell$MonthNum)

DatosMcDowell$MonthNum <- ifelse(is.na(DatosMcDowell$MonthNum),DatosMcDowell$MonthNum2, DatosMcDowell$MonthNum)

DatosMcDowell$MonthNum2<-NULL
DatosMcDowell$Month2<- NULL

DatosMcDowell$Date <- as.POSIXct(strptime(DatosMcDowell$Month ,format="%m %d,%Y",tz="America/Puerto_Rico"))
