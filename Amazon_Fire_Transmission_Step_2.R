#==================================================================================================================================
#  EXTRACTING AMAZONIA BURNED AREAS FOR EACH COUNTRY PER YEAR 
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
#==================================================================================================================================
# SETTING THE WORKING DIRECTORY
setwd("C:/Users/Tejas/Desktop/DLR_2020/Emmanuel_Amazon")
#==================================================================================================================================
#----------------------------------------------------------------------
# STEP 1: CLIPPING THE BURNED AREAS WITH GRID=1 PER COUNTRY PER YEAR --
#----------------------------------------------------------------------
#==================================================================================================================================
# LOADING THE COUNTRY BORDERS
country_list = list.files("./For_analysis_2020_manuscript/Shape_files_of_study_areas/Country_limits", pattern = "*.shp$")
#country_list = country_list[2]
#==================================================================================================================================
# LOADING THE BURNED AREA FRQUENCIES. THESE ARE ALREADY SEPARATED BY YEAR IN THE EXISTING DATA. ACROSS AMAZON. WE NEED TO SEPARATE
# BY COUNTRY

ba_list = list.files("./Vector_frequencies/", pattern = "*.shp$")

#==================================================================================================================================
#test = readOGR(paste0(getwd(),"/Vector_frequencies"), "2001")
#test_filt = test[test$gridcode == 1,]
#test_ct = readOGR(paste0(getwd(),"/For_analysis_2020_manuscript/Shape_files_of_study_areas/Country_limits"), "Bolivia")
#ba_bolivia = crop(test_filt, test_ct, progress = "text")
#plot(ba_bolivia)
#plot(test_ct, add = T)

# CLIPPING SELECTING GRID=1 AND CLIPPING BA BY COUNTRY 

for (i in 1:length(country_list)) {
  country_name = substr(country_list[i], 0, nchar(as.character(country_list[i]))-4)
  country_vec = readOGR(paste0(getwd(),"/For_analysis_2020_manuscript/Shape_files_of_study_areas/Country_limits"), 
                        layer = country_name)
  dir.create(paste0(getwd(),"/Vector_frequencies/",country_name))
  # CREATING FOLDER FOR THE OUTPUT 
  for(n in 1:length(ba_list)){
    
    # PROVISION FOR SKIPPING THE EMPTY OUTPUTS 
    skip_to_next = F
    
    # EXTRACTING THE BURNED AREA YEAR 
    ba_yr = substr(ba_list[n], 0, nchar(as.character(ba_list[n]))-4)
    
    # LOADING THE FIRST YEAR 
    ba_yr_vec = readOGR(paste0(getwd(),"/Vector_frequencies"), layer = ba_yr)
    
    # FILTERING BY GRID CODE = 1
    ba_yr_vec = ba_yr_vec[ba_yr_vec$gridcode == 1,]
    country_ba = crop(ba_yr_vec, country_vec, progress = "text")
    
    # THE SNIPPET BELOW WILL SKIP THE WRITING FILE IF 'country_ba' IS EMPTY! 
    tryCatch(writeOGR(country_ba, paste0(getwd(),"/Vector_frequencies/",country_name), 
                      paste0(country_name,"_ba_",ba_yr), driver="ESRI Shapefile"), error = function(e){skip_to_next <<- T})
    if(skip_to_next){next}
  }
}

