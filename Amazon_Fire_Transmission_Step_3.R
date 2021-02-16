#==================================================================================================================================
# EXTRACTING AMAZONIA IG-POINTS - BURNED AREA OVERLAPPING PAIRS AND SELECTING THE EARLIEST ACQUIRED IG POINT THAT IS CLOSEST TO THE 
# BURNED AREA BORDER
#==================================================================================================================================
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
setwd("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/")

####################################################################
# PREPARING BURNED AREAS, PROTECTED AREAS AND IGNITION POINTS DATA #
####################################################################
#==================================================================================================================================
#==================================================================================================================================
# LOADING THE PROTECTED AREA BUFFER DIRECTORY (WILL BE DIFFERENT FOR EACH USER). WE JUST NEED THE PATH 
pa_buffer_dir = "C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/FINAL_BUFFER"

# LISTIING ALL THE BUFFER AREAS IN THE DIRECTORY 
pa_buffer_list = list.files(pa_buffer_dir, pattern = "*.shp$")

# EXTRACTING THE NAMES OF THE BUFFER AREAS WITHOUT THE .SHP EXTENSION 
pa_names = substr(pa_buffer_list, 0, nchar(as.character(pa_buffer_list))-4)

#==================================================================================================================================
#==================================================================================================================================
# LOADING THE IG POINTS FOR THE COUNTRY 
#IP_dir = "C:/Users/Tejas/Desktop/DLR_2020/Emmanuel_Amazon/For_analysis_2020_manuscript/Heat_map/IP/Yearly_IP/Bolivia_IP"
#IP_country_yearly_list = substr(list.files(paste0(IP_dir), pattern = "*.shp$"), 0, 
#                                nchar(as.character(list.files(paste0(IP_dir), 
#                                                              pattern = "*.shp$")))-4)
#IP_country_yearly_list = IP_country_yearly_list[-1]
#==================================================================================================================================
#==================================================================================================================================
# LOADING THE BURNED AREAS FOR THE COUNTRY
#ba_dir = "C:/Users/Tejas/Desktop/DLR_2020/Emmanuel_Amazon/Vector_frequencies/Bolivia"
#ba_country_yearly_list = substr(list.files(paste0(ba_dir), pattern = "*.shp$"), 0, 
#                                nchar(as.character(list.files(paste0(ba_dir), 
#                                                              pattern = "*.shp$")))-4)
#==================================================================================================================================
#############################################################################################################################
#' FUNCTION TO USE THE ABOVE THREE LAYERS IN COMBINATION TO PRODUCE IGNITION POINTS-BURNED AREAS PAIRS THAT FALL INSIDE THE #
#' PROTECTED AREAS OF EACH OF THE COUNTRY LIMITS.                                                                           #
#############################################################################################################################
# FUNCTION IS DESIGNED TO WORK WITH ONE COUNTRY AT A TIME FOR ERROR FREE OUTPUT...
#IG_point_country_name  = "Venezuela_IP_17_18"
#BA_country_name = "Ecuador"
#PA_buffer_name = "buffer_venz"

# SETTING UP PARALLEL COMPUTING 
#cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)

IG_BA_Pairs = function(IG_point_country_name, BA_country_name, buffer_tube_file_name, PA_buffer_name){
  # EXTRACTING THE COUNTRY IGNITION POINT DATA
  IG_dir = paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/For_analysis_2020_manuscript/Heat_map/IP/IP_17_18/",IG_point_country_name) # 
  IG_country_yearly_list = substr(list.files(paste0(IG_dir), pattern = "*.shp$"), 0,
                                  nchar(as.character(list.files(paste0(IG_dir), 
                                                                pattern = "*.shp$")))-4)
  
  # REMOVING THE UNCOMMON YEAR
  #IG_country_yearly_list = IG_country_yearly_list[-1]

  # EXTRACTING THE COUNTRY BURNED AREA POLYGON DATA
  ba_dir = paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/Vector_frequencies/",BA_country_name) # 
  ba_country_yearly_list = substr(list.files(paste0(ba_dir), pattern = "*.shp$"), 0, 
                                  nchar(as.character(list.files(paste0(ba_dir), 
                                                                pattern = "*.shp$")))-4)
  # PICKING THE CERTAIN YEARS
  ba_country_yearly_list = ba_country_yearly_list[str_detect(ba_country_yearly_list, "2017|2018")]
  #ba_country_yearly_list = ba_country_yearly_list[c(17,18)] 
  
  
  # RUNNING ON SAMPLE DATA 
  country_pa = readOGR(pa_buffer_dir, PA_buffer_name) # replace this
  #plot(country_pa)

  # INTRODUCING THE IGNITION POINT LOOP 
  for (z in 1:length(IG_country_yearly_list)) {
    # FOR 2001
    IG_Year = st_read(IG_dir,IG_country_yearly_list[z]) # put z here 
    IG_Year = as(IG_Year, "Spatial")
    #IG_Year = as(IG_Year, "SpatialPointsDataFrame")
    crs(country_pa) = crs(IG_Year)
    IG_Year_pa = crop(IG_Year, country_pa)
    
    # INTRODUCING THE BURNED AREAS LOOP  
    #for (x in 1:length(ba_country_yearly_list)) {
    # FOR 2001
    ba_Year = st_read(ba_dir, ba_country_yearly_list[z]) 
    ba_Year = as(ba_Year, "Spatial")
    #ba_Year = as(ba_Year, "SpatialPointsDataFrame")
    
    # SELECTING IPs THAT ARE INSIDE BURNED AREAS
    IG_inside_ba = crop(IG_Year_pa, ba_Year)

    # FILTERING BURNED AREAS THAT HAVE IPs INSIDE THEM
    # PROVISION FOR SKIPPING THE EMPTY OUTPUTS 
    skip_to_next = F
    tryCatch(intersect(ba_Year, IG_inside_ba), error = function(e){skip_to_next <<- T})
    if(skip_to_next){next}
    
    ba_with_IG = intersect(ba_Year, IG_inside_ba)
    #head(ba_with_IG@data)
    
    # ADDING MORE METADATA TO BURNED AREAS 
    ba_with_IG@data$LATITUDE = coordinates(ba_with_IG)[,2]
    ba_with_IG@data$LONGITUDE = coordinates(ba_with_IG)[,1]
    
    # ASSIGNING UIDs TO IPs 
    ba_with_IG@data$Id = seq(1, length(ba_with_IG))
    writeOGR(ba_with_IG, paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/IG_BA_Analysis/",BA_country_name,"/Burned_Areas"), paste0("BA_",ba_country_yearly_list[z]), driver = "ESRI Shapefile", overwrite_layer = T)  
    
    # FORMATTING DATE AND ADDING AN ID COLUMN TO THE IGNITION POINTS METADATA
    IG_inside_ba@data$ACQ_DATE = as.Date(IG_inside_ba@data$ACQ_DATE)
    IG_inside_ba@data$Id = seq(1, length(IG_inside_ba))
    #head(IG_inside_ba@data)
    
    # SPLITTING BURNED AREAS BY INDIVIDUAL POLYGONS 
    ba_with_IG_ls = as.list(split(ba_with_IG, ba_with_IG$Id))
  
    # CREATING EMPTY LIST WHERE IPs WITH MINIMUM DISTANCE AND EARLIEST DATE WILL BE INSERTED 
    IG_final = list()
    for (i in 1:length(ba_with_IG_ls)) {
      # CROPPING THE IGNITION POINTS BY BURNED AREAS
      IG_inside_ba_filt = crop(IG_inside_ba, ba_with_IG_ls[[i]])
      # FINDING THE DISTANCES TO CLOSES BORDERS 
      test = dist2Line(IG_inside_ba_filt, ba_with_IG_ls[[i]])
      # CONVERTING IT TO DATAFRAME 
      test_df = as.data.frame(test)
      # ADDING AN COLUMN TO THE IP POINTS THAT SHOWS THE DISTANCES 
      IG_inside_ba_filt$DistToEdge = test_df$distance
      # ADDING A UNIQUE IDENTIFIER THAT FOR THE PAIR 
      IG_inside_ba_filt$Id = ba_with_IG_ls[[i]]$Id
      # CHOOSING THE IP RECORDS WITH THE EARLIEST DATE
      IG_border = IG_inside_ba_filt[which.min(IG_inside_ba_filt@data$ACQ_DATE),]
      # CHOOSING THE IP THAT ARE CLOSEST TO THE BORDER 
      IG_border = IG_border[which.min(IG_border$DistToEdge),]
      # ADDING IT TO THE EMPTY DATAFRAME 
      IG_final[[i]] = IG_border
    }
    IG_final_vec = do.call("rbind", IG_final)
    writeOGR(IG_final_vec, paste0("C:/DLR_2020_Tejas/Amazon_Paper/Emmanuel_Amazon/IG_BA_Analysis/",BA_country_name,"/Ignition_Points"), paste0("IG_",ba_country_yearly_list[z]), driver = "ESRI Shapefile", overwrite_layer = T) # replace this 
  } # THIS CLOSES THE Z LOOP 
  # Assigning inside versus outside PAs designations 
  # Loading the areas with the buffer patch. These areas are outside actual PAs. 
  buffer_only_all = readOGR(paste0(getwd(), "/Buffer_areas_dissolve"), buffer_tube_file_name)
  #plot(buffer_only_all)
  # Loading the IG points 
  IG_list = list.files(paste0("./IG_BA_Analysis/",BA_country_name,"/Ignition_Points"), pattern = ".*shp")
  IG_list = IG_list[str_detect(IG_list, "2017|2018")]
  #test = readOGR(paste0(getwd(), "/IG_BA_Analysis/Brazil/Ignition_Points"), "IG_Brazil_All")
  
  for (t in 1:length(IG_list)) {
    test = readOGR(paste0(getwd(), "/IG_BA_Analysis/",BA_country_name,"/Ignition_Points"), substr(IG_list[t], 0, nchar(IG_list[t])-4))
    test$Location = NA
    buffer_only_all = spTransform(buffer_only_all, CRSobj = crs(test))
    
    # extra column will only be added if there is data in the dataframe. 
    test_out = test[!is.na(sp::over(test, sp::geometry(buffer_only_all))), ] 
    if(length(test_out$Location) != 0){test_out$Location <- "outside"}
    #plot(test_out, pch = 21, cex = 0.5, col = "blue", add = T)
    
    test_in = test[is.na(sp::over(test, sp::geometry(buffer_only_all))), ] 
    if(length(test_in$Location) != 0){test_in$Location <- "inside"}
    #plot(test_in, pch = 21, cex = 0.5, col = "red", add = T)
    
    test = bind(test_out, test_in)
    writeOGR(test, paste0(getwd(),"/IG_BA_Analysis/",BA_country_name,"/Ignition_Points"), substr(IG_list[t], 0, nchar(IG_list[t])-4), 
             driver = "ESRI Shapefile", overwrite_layer = T)
    
  }
  
} # THIS CLOSES THE FUNCTION 

IG_BA_Pairs(BA_country_name = "Brazil", IG_point_country_name = "Brazil_IP_17_18", PA_buffer_name = "Buff_brs", buffer_tube_file_name = "Brazil")

#stop cluster
#stopCluster(cl)
#gc()

#==================================================================================================================================
# Assigning inside versus outside PAs designations 
# Loading the areas with the buffer patch. These areas are outside actual PAs. 
buffer_only_all = readOGR(paste0(getwd(), "/Buffer_areas_dissolve"), "Brazil")
#plot(buffer_only_all)
# Loading the IG points 
IG_list = list.files("./IG_BA_Analysis/Brazil/Ignition_Points/", pattern = ".*shp")
#IG_list = IG_list[str_detect(IG_list, "2017|2018")]
#test = readOGR(paste0(getwd(), "/IG_BA_Analysis/Brazil/Ignition_Points"), "IG_Brazil_All")

for (t in 1:length(IG_list)) {
  test = readOGR(paste0(getwd(), "/IG_BA_Analysis/Brazil/Ignition_Points"), substr(IG_list[t], 0, nchar(IG_list[t])-4))
  test$Location = NA
  buffer_only_all = spTransform(buffer_only_all, CRSobj = crs(test))
  
  # extra column will only be added if there is data in the dataframe. 
  test_out = test[!is.na(sp::over(test, sp::geometry(buffer_only_all))), ] 
  if(length(test_out$Location) != 0){test_out$Location <- "outside"}
  #plot(test_out, pch = 21, cex = 0.5, col = "blue", add = T)
  
  test_in = test[is.na(sp::over(test, sp::geometry(buffer_only_all))), ] 
  if(length(test_in$Location) != 0){test_in$Location <- "inside"}
  #plot(test_in, pch = 21, cex = 0.5, col = "red", add = T)
  
  test = bind(test_out, test_in)
  writeOGR(test, paste0(getwd(),"/IG_BA_Analysis/Brazil/Ignition_Points"), substr(IG_list[t], 0, nchar(IG_list[t])-4), 
           driver = "ESRI Shapefile", overwrite_layer = T)
  
}

#==================================================================================================================================
# FIXING FID COLUMN 

IG_point_country_name = "Venezuela"
#table_dir = paste0("./Tables_IGNITION_points/",IG_point_country_name) 
table_list = list.files(paste0("./Tables_IGNITION_points/",IG_point_country_name), pattern = ".*csv")


for (i in 1:length(table_list)) {
    test = read.csv(paste0("./Tables_IGNITION_points/",IG_point_country_name,"/",table_list[i]))
    test$FID = test$FID + 1
    write.csv(test, paste0(getwd(),"/Tables_IGNITION_points/", IG_point_country_name, "/", table_list[i]))
}


test = read.csv(paste0("./Tables_IGNITION_points/",IG_point_country_name,"/",table_list[1]))
head(test)
test$FID = test$FID + 1
write.csv(test, paste0(getwd(),"/Tables_IGNITION_points/", IG_point_country_name, "/", table_list[1]))










