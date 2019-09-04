##############################################################################################################################################################
#### Analyse NDVI changes with satellite imagery ####
# Calculates the difference in NDVI between two dates
# R.Bienz / 04.09.2019
##############################################################################################################################################################
library(raster)
library(rgdal)
library(rgeos)
library(lidR)

setwd("C:/Auswertungen/Trockenheit_2019/")
rasterOptions(tmpdir= "C:/Auswertungen/Trockenheit_2019/temp/",todisk=TRUE, progress="text")#progress bar wird angegeben
######################################################################################################################################################################################
#### Import data ####
######################################################################################################################################################################################
# Forest delination
wa <- readOGR(dsn="data",layer = "wa_lv95",encoding = "ESRI Shapefile")
wa <- wa[wa@data$Shape_STAr>20,] # Remove small polygons
wa_agg <- aggregate(wa,dissolve=T) # Dissolve polygons

# Area of interest delination
kt <- readOGR(dsn="data",layer = "kt",encoding = "ESRI Shapefile")


# Import of Sentinel 2 data (first the rasters were projected into the correct coordinate system)
b4_time1 <- raster("wd/040619_b04_cor.tif")
b8_time1 <- raster("wd/040619_b08_cor.tif")
b4_time2 <- raster("wd/180819_b04.tif")
b8_time2 <- raster("wd/180819_b08.tif")

######################################################################################################################################################################################
#### Calculation ####
######################################################################################################################################################################################
ndvi_time1 <- (b8_time1-b4_time1)/(b8_time1+b4_time1)

ndvi_time2 <- (b8_time2-b4_time2)/(b8_time2+b4_time2)

ndvi_diff <- ndvi_time2 - ndvi_time1

writeRaster(ndvi_diff,"result/ndvi_diff_180819_040619.tif")

######################################################################################################################################################################################
#### Postprocessing ####
######################################################################################################################################################################################
# Clip to Area of interest (bounding box)
ndvi_diff <- raster("result/ndvi_diff_180819_040619.tif")
ndvi_diff_kt <- crop (ndvi_diff,kt)

# Clip to forest delination
if (!file.exists("wd/wa_ras.tif")){
  wa_ras <- rasterize(wa,ndvi_diff_kt,mask=T)
  wa_ras[!is.na(wa_ras[])] <- 1
  writeRaster(wa_ras,"wd/wa_ras.tif")
}
wa_ras <- raster("wd/wa_ras.tif")

ndvi_diff_wa <- ndvi_diff_kt * wa_ras # This method was used, because raster::mask() did not work properly with polygons which have holes in them.

writeRaster(ndvi_diff_wa,"result/ndvi_diff_wa2_180819_040619.tif")


######################################################################################################################################################################################
#### Data analysis ####
######################################################################################################################################################################################
ndvi_diff_wa <- raster("result/ndvi_diff_wa2_180819_190617.tif")

length(which(!is.na(ndvi_diff_wa[])))/100 # forest area 
length(which(ndvi_diff_wa[]< (-0.08)))/100 # Affected forest area in ha with threshold -0.08
length(which(ndvi_diff_wa[]< (-0.08)))/length(which(!is.na(ndvi_diff_wa[]))) # Affected forest area in % with threshold -0.08






