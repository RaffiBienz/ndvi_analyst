##############################################################################################################################################################
#### Analyse NDVI changes with satellite imagery ####
# Calculates the difference in NDVI between two dates
# R.Bienz / 21.07.2020
##############################################################################################################################################################
library(raster)
library(rgdal)
library(rgeos)

#setwd()
dir.create("temp",showWarnings = FALSE)
dir.create("result",showWarnings = FALSE)
rasterOptions(tmpdir= paste0(getwd(),"/temp"),todisk=TRUE, progress="text")#progress bar wird angegeben
######################################################################################################################################################################################
#### Import data ####
######################################################################################################################################################################################
# Forest delination
wa <- readOGR(dsn="example_data",layer = "wa_example",encoding = "ESRI Shapefile")
wa <- wa[area(wa)>20,] # Remove small polygons
wa_agg <- aggregate(wa,dissolve=T) # Dissolve polygons

# Area of interest delination
kt <- readOGR(dsn="example_data",layer = "kt",encoding = "ESRI Shapefile")

# Areas with clouds
remove_clouds <- FALSE # If areas with clouds should be ignored, create shapefile with clouds and set TRUE.
if (remove_clouds){
  clouds <- readOGR(dsn="data",layer = "clouds",encoding = "ESRI Shapefile")
}

# Import of Sentinel 2 data (first the rasters need to be projected into the correct coordinate system in a GIS)
b4_time1 <- raster("example_data/190617_b04_ex.tif")
b8_time1 <- raster("example_data/190617_b08_ex.tif")
b4_time2 <- raster("example_data/180819_b04_ex.tif")
b8_time2 <- raster("example_data/180819_b08_ex.tif")
origin(b4_time2) <- origin(b4_time1)
origin(b8_time2) <- origin(b8_time1)

######################################################################################################################################################################################
#### Calculation ####
######################################################################################################################################################################################
ndvi_time1 <- (b8_time1-b4_time1)/(b8_time1+b4_time1)

ndvi_time2 <- (b8_time2-b4_time2)/(b8_time2+b4_time2)

ndvi_diff <- ndvi_time2 - ndvi_time1

writeRaster(ndvi_diff,"result/ndvi_diff_180819_190617.tif", overwrite=TRUE)

######################################################################################################################################################################################
#### Postprocessing ####
######################################################################################################################################################################################
# Clip to Area of interest (bounding box)
ndvi_diff <- raster("result/ndvi_diff_180819_190617.tif")
ndvi_diff_kt <- crop(ndvi_diff,kt)

# Clip to forest delination
if (!file.exists("temp/wa_ras.tif")){
  wa_ras <- rasterize(wa_agg,ndvi_diff_kt,mask=T)
  wa_ras[!is.na(wa_ras[])] <- 1
  writeRaster(wa_ras,"temp/wa_ras.tif")
}
wa_ras <- raster("temp/wa_ras.tif")
ndvi_diff_wa <- ndvi_diff_kt * wa_ras # This method was used, because raster::mask() did not work properly with polygons which have holes in them.

# Remove areas with clouds
if (remove_clouds){
  if (!file.exists("temp/cloud_ras.tif")){
    cloud_ras <- rasterize(clouds,ndvi_diff_kt,mask=T)
    cloud_ras[is.na(cloud_ras[])] <- 1
    cloud_ras[cloud_ras[]<1] <- 0
    writeRaster(cloud_ras,"temp/cloud_ras.tif",overwrite=T)
  }
  cloud_ras <- raster("temp/cloud_ras.tif")
  ndvi_diff_wa <- ndvi_diff_wa * cloud_ras # This method was used, because raster::mask() did not work properly with polygons which have holes in them.
}

writeRaster(ndvi_diff_wa,"result/ndvi_diff_wa_180819_190617.tif", overwrite=TRUE)
removeTmpFiles(h=0)

######################################################################################################################################################################################
#### Analysis ####
######################################################################################################################################################################################
threshold <- -0.08 # Which NDVI reduction is regarded as a change, 
ndvi_diff_wa <- raster("result/ndvi_diff_wa_180819_190617.tif")

length(which(!is.na(ndvi_diff_wa[])))/100 # forest area 
length(which(ndvi_diff_wa[]< threshold))/100 # Affected forest area in ha with threshold
length(which(ndvi_diff_wa[]< threshold))/length(which(!is.na(ndvi_diff_wa[])))*100 # Affected forest area in % with threshold


######################################################################################################################################################################################
#### Remove edge effects and single pixels ####
######################################################################################################################################################################################
threshold <- -0.08 # Which NDVI reduction is regarded as a change
ndvi_diff_wa <- raster("result/ndvi_diff_wa_180819_190617.tif")

ndvi_recl <- reclassify(ndvi_diff_wa,matrix(c(-10000,threshold,1,threshold,1000,0),byrow = T,ncol = 3))
#writeRaster(ndvi_recl,"wd/ndvi_recl.tif", overwrite=TRUE)

ndvi_focal <- focal(ndvi_recl,matrix(rep(1,9),ncol = 3),fun=modal)
writeRaster(ndvi_focal,"result/ndvi_diff_wa_focal_180819_190617.tif", overwrite=TRUE)


######################################################################################################################################################################################
#### Analysis without edge effects ####
######################################################################################################################################################################################
ndvi_focal <- raster("result/ndvi_diff_wa_focal_180819_190617.tif")
length(which(ndvi_focal[]==1))/100 # Affected forest area in ha with threshold
length(which(ndvi_focal[]==1))/length(which(!is.na(ndvi_focal[])))*100 # Affected forest area in % with threshold





