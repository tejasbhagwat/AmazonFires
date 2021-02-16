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
library(nngeo)
library(rgeos)
library(gdalUtils)
library(maptools)
library(geosphere)
library(foreach)
library(doParallel)

#==================================================================================================================================
# SETTING THE WORKING DIRECTORY
setwd("C:/DLR_2020_Tejas/Amazon_Paper")

country_name = "Colombia"

# Fire Transmission tables 
fire_data = list.files(paste0("C:/DLR_2020_Tejas/Amazon_Paper/Final_otucomes_Amazon_manuscript/Results/Final_fire_transmission_tables_protected_areas__per_country_2001_2018/", country_name), pattern = ".csv")
# IG points with date info
IG_points_files = list.files(paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/IG_BA_Analysis/", country_name,"/IG_Points"), pattern = ".shp")
#IG_points_files = IG_points_files[-16]
for (i in 1:length(fire_data)) {
    f1 = read.csv(paste0("C:/DLR_2020_Tejas/Amazon_Paper/Final_otucomes_Amazon_manuscript/Results/Final_fire_transmission_tables_protected_areas__per_country_2001_2018/",(country_name),"/",fire_data[i]))
    s1 = readOGR(paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/IG_BA_Analysis/", country_name,"/IG_Points"), paste0(substr(IG_points_files[i], 0, nchar(IG_points_files[i])-4)))
    s1_df = s1@data
    result = merge(f1, s1_df, by = "Id", all = FALSE)
    write.csv(result, paste0("C:/DLR_2020_Tejas/Amazon_Paper/Results_Timestamped/", substr(fire_data[i], 0, nchar(fire_data[i])-4),"_TS.csv"))
}



















