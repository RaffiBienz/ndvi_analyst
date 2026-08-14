##############################################################################################################################################################
#### Analyse NDVI changes with satellite imagery ####
# Calculates the difference in NDVI between two dates
# R.Bienz / 14.08.2026
##############################################################################################################################################################
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
library(terra)

# set the working directory and create necessary folders
setwd(".")
dir.create("wd",showWarnings = FALSE)
dir.create("data",showWarnings = FALSE)
dir.create("results",showWarnings = FALSE)

options(warn=-1) # disable warnings, enable warnings for debugging

# Copy the Sentinel-2 or swissEO S2-SR data into the data folder. Copy shapefiles of the area of interest, the forest delineation and the areas with cloud cover into the data folder.
swissEO = TRUE # Set to TRUE, if you use swissEO S2-SR data, set to FALSE, if you use data directly from Copernicus

######################################################################################################################################################################################
#### Set paths and global variables ####
######################################################################################################################################################################################
time1 <- "08082025"
time2 <- "22072026"

threshold <- -0.08 # Which NDVI reduction is regarded as a change

# paths to Sentinel-2 data
if (swissEO) {
  path_time1 <- "data/swissEO/swissEO-S2-SR-08082025.tif"
  path_time2 <- "data/swissEO/swissEO-S2-SR-22072026.tif"
} else {
  path_time1_b4 <- "data/copernicus/T32TMT_20200807T102559_B04_10m.jp2"
  path_time1_b8 <- "data/copernicus/T32TMT_20200807T102559_B08_10m.jp2"
  path_time2_b4 <- "data/copernicus/T32TMT_20210812T102559_B04_10m.jp2"
  path_time2_b8 <- "data/copernicus/T32TMT_20210812T102559_B08_10m.jp2"
}


# name of forest delineation shapefile
path_fd <- "data/wa.shp"

# name of area of interest shapefile
path_aoi <- "data/kt.shp"

# name of cloud shapefile
remove_clouds <- FALSE # If areas with clouds should be ignored, create dataset with clouds and set TRUE.
path_cl <- "data/clouds.shp"

######################################################################################################################################################################################
#### Import data ####
######################################################################################################################################################################################
# Forest delineation
wa <- vect(path_fd)
wa <- wa[as.numeric(expanse(wa))>10,] # Remove small polygons
wa_agg <- aggregate(wa, dissolve=T) # Dissolve polygons

# Area of interest delineation
kt <- vect(path_aoi)

# Areas with clouds (create shapefile in a GIS)
if (remove_clouds){
  clouds <- vect(path_cl)
}

# Import of Sentinel-2 data
if (swissEO){
  rgbi_time1 <- rast(path_time1)
  rgbi_time2 <- rast(path_time2)
} else {
  b4_time1 <- rast(path_time1_b4)
  b8_time1 <- rast(path_time1_b8)
  b4_time2 <- rast(path_time2_b4)
  b8_time2 <- rast(path_time2_b8)
}


######################################################################################################################################################################################
#### Calculation ####
######################################################################################################################################################################################
# NDVI calculation
if (swissEO){
  ndvi_time1 <- (rgbi_time1[[4]]-rgbi_time1[[1]])/(rgbi_time1[[4]]+rgbi_time1[[1]])
  ndvi_time2 <- (rgbi_time2[[4]]-rgbi_time2[[1]])/(rgbi_time2[[4]]+rgbi_time2[[1]])
} else {
  ndvi_time1 <- (b8_time1-b4_time1)/(b8_time1+b4_time1)
  ndvi_time2 <- (b8_time2-b4_time2)/(b8_time2+b4_time2)
}

# NDVI difference
ndvi_diff <- ndvi_time2 - ndvi_time1
if (!swissEO){
  ndvi_diff <- project(ndvi_diff, crs(kt))
}

writeRaster(ndvi_diff, paste0("results/ndvi_diff_", time1,"_", time2,".tif"), overwrite=TRUE)

######################################################################################################################################################################################
#### Postprocessing ####
######################################################################################################################################################################################
# Clip to Area of interest (bounding box)
ndvi_diff <- rast(paste0("results/ndvi_diff_", time1,"_", time2,".tif"))
ndvi_diff_kt <- crop(ndvi_diff,kt)

# Clip to forest delineation
if (!file.exists("wd/wa_ras.tif")){
  wa_ras <- rasterize(wa_agg,ndvi_diff_kt,mask=T)
  wa_ras[!is.na(wa_ras[])] <- 1
  writeRaster(wa_ras,"wd/wa_ras.tif")
}
wa_ras <- rast("wd/wa_ras.tif")
origin(wa_ras) <- origin(ndvi_diff_kt)
ndvi_diff_wa <- ndvi_diff_kt * wa_ras 

# Remove areas with clouds
if (remove_clouds){
  if (!file.exists("wd/cloud_ras.tif")){
    cloud_ras <- rasterize(clouds,ndvi_diff_kt)
    cloud_ras[cloud_ras[]==1] <- 0
    cloud_ras[is.na(cloud_ras[])] <- 1
    writeRaster(cloud_ras,"wd/cloud_ras.tif",overwrite=T)
  }
  cloud_ras <- rast("wd/cloud_ras.tif")
  ndvi_diff_wa <- ndvi_diff_wa * cloud_ras
}

writeRaster(ndvi_diff_wa, paste0("results/ndvi_diff_wa_", time1,"_", time2,".tif"), overwrite=TRUE)

######################################################################################################################################################################################
#### Data analysis ####
######################################################################################################################################################################################
print("Results without edge effects removal:")

print(paste0("Total forest area in ha: ", round(length(which(!is.na(ndvi_diff_wa[])))/100,2))) # Total forest area in ha
print(paste0("Affected forest area in ha: ", round(length(which(ndvi_diff_wa[]< (threshold)))/100,2))) # Affected forest area in ha with threshold
print(paste0("Affected forest area in %: ", round(length(which(ndvi_diff_wa[]< (threshold)))/length(which(!is.na(ndvi_diff_wa[])))*100,2))) # Affected forest area in % with threshold

######################################################################################################################################################################################
#### Remove edge effects ####
######################################################################################################################################################################################
ndvi_diff_wa <- rast(paste0("results/ndvi_diff_wa_", time1,"_", time2,".tif"))

ndvi_recl <- ifel(ndvi_diff_wa < threshold, 1, 0)

ndvi_focal <- focal(ndvi_recl,w=3,fun="modal", na.policy = "omit")
writeRaster(ndvi_focal, paste0("results/ndvi_diff_binary_", time1,"_", time2,".tif"), overwrite=TRUE)

# Apply on NDVI-Difference dataset
ndvi_diff_focal <- ndvi_diff_wa * ndvi_focal
writeRaster(ndvi_diff_focal, paste0("results/ndvi_diff_wo_edges_", time1,"_", time2,".tif"), overwrite=TRUE)
######################################################################################################################################################################################
#### Data analysis without edge effects ####
######################################################################################################################################################################################
ndvi_focal <- rast(paste0("results/ndvi_diff_binary_", time1,"_", time2,".tif"))

print("Results with edge effects removal:")
print(paste0("Affected forest area in ha: ", round(length(which(ndvi_focal[]==1))/100,2))) # Affected forest area in ha with threshold
print(paste0("Affected forest area in %: ", round(length(which(ndvi_focal[]==1))/length(which(!is.na(ndvi_focal[])))*100,2))) # Affected forest area in % with threshold



