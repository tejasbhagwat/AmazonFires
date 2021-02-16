#==================================================================================================================================
# EXTRACTING AMAZONIA IGNITION POINTS FOR EACH COUNTRY PER YEAR 
#==================================================================================================================================
# IMPORTANT INFO
#' THIS SCRIPT TAKES THE IGNITION POINTS FOR EACH COUNTRY WHICH WERE EXTRACTED FROM THE 'Only_Amazon_Fires.shp' FILE. WITHOUT THE 
#' COUNTRY-WISE SEPARATION, THIS SCRIPT WON'T WORK. 
#' 
# LOADING THE NECESSARY LIBRARIES 
library(ncdf4)
library("fields")
library(chron)
library(raster)
library(rgdal)
library(ggplot2)
library(stringr)
library(reshape2)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(ggcorrplot)
library(tidyverse)
library(viridis)
library(corrplot)
library(dplyr)
library(cowplot)
library(sf)
#==================================================================================================================================
# SETTING THE WORKING DIRECTORY
# THIS IS THE FOLDER WHERE THE IGNITION POINTS PER COUNTRY SHOULD BE SAVED. 
setwd("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/For_analysis_2020_manuscript/Heat_map/IP/IP_17_18/")

#==================================================================================================================================
# LOADING COUNTRY BOUNDARYES 
country_list = list.files("./", pattern = "*.shp$")
#country_list = country_list[-9]
#==================================================================================================================================
# LOADING COUNTRY IGNITION POINTS 
for(n in 1:length(country_list)){
  # EXTRACT THE COUNTRY NAME 
  country_name = substr(country_list[n], 0, nchar(as.character(country_list[n]))-4)
  
  # CREATE A DIRECTORY WHERE THE YEARLY POINTS WILL BE SAVED
  country_dir = dir.create(paste0(getwd(),"/Yearly_IP/",country_name))
  
  # LOAD THE FILE FOR A COUNTRY 9BASED ON THE COUNTRY LIST PROVIDED ABOVE)
  country_name_IP = readOGR(paste0(getwd()), layer = country_name)
  
  # CHANGE THE DATE COLUMN FORMAT 
  country_name_IP@data$ACQ_DATE = as.POSIXct(country_name_IP@data$ACQ_DATE)
  
  # FORMAT THE DATE FORMAT
  country_name_IP$ACQ_DATE <- as.Date(country_name_IP$ACQ_DATE, format = "%m/%d/%y")
  
  # SPLIT IGNITION POINTS BY DATE (YEAR IN THIS CASE)
  test = as.list(split(country_name_IP, year(country_name_IP$ACQ_DATE)))
  
  # WRITE IT OUT BY YEAR IN THE DIRECTORY SPECIFIED IN THE LOOP
  for (i in 1:length(test)) {
    writeOGR(test[[i]], paste0(getwd(),"/Yearly_IP/",country_name), paste0(country_name,"_",names(test)[i]), driver="ESRI Shapefile")
  }
}












