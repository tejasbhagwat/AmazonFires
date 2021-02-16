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
library(cowplot)
#==================================================================================================================================
# SETTING THE WORKING DIRECTORY
setwd("C:/DLR_2020_Tejas/Amazon_Paper/Results_Timestamped")

country_name = "Venezuela"
country_list = list.files(paste0("C:/DLR_2020_Tejas/Amazon_Paper/Results_Timestamped/", country_name), pattern = ".csv")
dir.create(paste0(getwd(),"/Results_Timestamped_withStats/",country_name,"_TransAnalysis"))

#colnames(country_data)
for (i in 1:length(country_list)) {
  country_data = read.csv(paste0(getwd(),"/",country_name,"/",country_list[i]))
  country_data[country_data < 0] <- 0
  # COMBINING ALL FOREST TYPES INTO ONE FOREST CLASS  
  country_data$Forest_Out = rowSums(country_data[,c("Out_Class_1","Out_Class_2","Out_Class_3","Out_Class_4","Out_Class_5")])* 6.25
  country_data$Forest_In = rowSums(country_data[,c("In_Class_1","In_Class_2","In_Class_3","In_Class_4","In_Class_5")])* 6.25
  country_data$Non_Forest_Out = rowSums(country_data[,c("Out_Class_6","Out_Class_7","Out_Class_8","Out_Class_9","Out_Class_10","Out_Class_11","Out_Class_12","Out_Class_13","Out_Class_14","Out_Class_15")])* 6.25
  country_data$Non_Forest_In = rowSums(country_data[,c("In_Class_6","In_Class_7","In_Class_8","In_Class_9","In_Class_10","In_Class_11","In_Class_12","In_Class_13","In_Class_14","In_Class_15")])* 6.25
  country_data$Total_Burned = rowSums(country_data[,c("Forest_Out","Forest_In","Non_Forest_Out","Non_Forest_In")])
  write.csv(country_data, paste0(getwd(),"/Results_Timestamped_withStats/", country_list[i])) 
                          
}
  

