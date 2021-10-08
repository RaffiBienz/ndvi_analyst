##############################################################################################################################################################
#### Analyse NDVI changes with satellite imagery ####
# Calculates the difference in NDVI between two dates
# R.Bienz / 07.10.2021
##############################################################################################################################################################
if (!require("rgdal")) install.packages("rgdal")
if (!require("rgeos")) install.packages("rgeos")
if (!require("raster")) install.packages("raster")


# set the working directory and create necessary folders
setwd(".")
dir.create("temp",showWarnings = FALSE)
dir.create("wd",showWarnings = FALSE)
dir.create("data",showWarnings = FALSE)
dir.create("results",showWarnings = FALSE)
rasterOptions(tmpdir= paste0(getwd(),"/temp"),todisk=TRUE, progress="")
options(warn=-1) # disable warnings, enable for debugging

# Copy the Sentinel-2 data into the data folder. Copy shapefiles of the area of interest, the forest delination and the areas with cloud cover into the data folder.

######################################################################################################################################################################################
#### Set paths and global variables ####
######################################################################################################################################################################################
time1 <- "07082020"
time2 <- "12082021"

coregistrate_rasters <- TRUE

threshold <- -0.08 # Which NDVI reduction is regarded as a change

# paths to images of bands 4 and 8 of two moments in time
path_time1_b4 <- "data/sent2/T32TMT_20200807T102559_B04_10m.jp2"
path_time1_b8 <- "data/sent2/T32TMT_20200807T102559_B08_10m.jp2"
path_time2_b4 <- "data/sent2/T32TMT_20210812T102559_B04_10m.jp2"
path_time2_b8 <- "data/sent2/T32TMT_20210812T102559_B08_10m.jp2"

# name of forest delination shapefile
name_fd <- "wa"

# name of area of interest shapefile
name_aoi <- "kt"

# name of cloud shapefile
remove_clouds <- FALSE # If areas with clouds should be ignored, create shapefile with clouds and set TRUE.
name_cl <- "clouds"

######################################################################################################################################################################################
#### Import data ####
######################################################################################################################################################################################
# Forest delination
wa <- readOGR(dsn="data",layer = name_fd, encoding = "ESRI Shapefile")
wa <- wa[area(wa)>20,] # Remove small polygons
wa_agg <- aggregate(wa,dissolve=T) # Dissolve polygons

# Area of interest delination
kt <- readOGR(dsn="data",layer = name_aoi, encoding = "ESRI Shapefile")

# Areas with clouds (create shapefile in a GIS)
if (remove_clouds){
  clouds <- readOGR(dsn="data",layer = names_cl, encoding = "ESRI Shapefile")
}

# Import and projection (lv95) of Sentinel 2 data
path_t1_b4_proj <- paste0("wd/b4_", time1, ".tif")
if (!file.exists(path_t1_b4_proj)){
  b4_time1_org <- raster(path_time1_b4)
  b4_time1 <- projectRaster(b4_time1_org, crs=CRS("+init=epsg:2056"))
  writeRaster(b4_time1, path_t1_b4_proj)
}

path_t1_b8_proj <- paste0("wd/b8_", time1, ".tif")
if (!file.exists(path_t1_b8_proj)){
  b8_time1_org <- raster(path_time1_b8)
  b8_time1 <- projectRaster(b8_time1_org, crs=CRS("+init=epsg:2056"))
  writeRaster(b8_time1, path_t1_b8_proj)
}

b4_time1 <- raster(path_t1_b4_proj)
b8_time1 <- raster(path_t1_b8_proj)

path_t2_b4_proj <- paste0("wd/b4_", time2, ".tif")
if (!file.exists(path_t2_b4_proj)){
  b4_time2_org <- raster(path_time2_b4)
  b4_time2 <- projectRaster(b4_time2_org, crs=CRS("+init=epsg:2056"))
  writeRaster(b4_time2, path_t2_b4_proj)
}

path_t2_b8_proj <- paste0("wd/b8_", time2, ".tif")
if (!file.exists(path_t2_b8_proj)){
  b8_time2_org <- raster(path_time2_b8)
  b8_time2 <- projectRaster(b8_time2_org, crs=CRS("+init=epsg:2056"))
  writeRaster(b8_time2, path_t2_b8_proj)
}

b4_time2 <- raster(path_t2_b4_proj)
b8_time2 <- raster(path_t2_b8_proj)

origin(b4_time2) <- origin(b4_time1)
origin(b8_time2) <- origin(b8_time1)

######################################################################################################################################################################################
#### Coregistration of Satellite images ####
######################################################################################################################################################################################
# Sometimes images from two differnt times do not exactly match (check in GIS). Here a simple coregsitration method is applied, to find a shift, which minimizes the differences between two rasters.
shifts_x <- c(-10,0,10)
shifts_y <- c(-10,0,10)

coregistrate <- function(ras_t1, ras_t2){
  coreg_results <- matrix(ncol=3,nrow=0)
  for (x in shifts_x){
    for (y in shifts_y){
      ras_t1_shift <- shift(ras_t1,x,y)
      diff_sq <- (ras_t1_shift - ras_t2)^2
      diff_sq_mean <- mean(diff_sq[], na.rm = T)
      coreg_results <- rbind(coreg_results,c(x,y,diff_sq_mean))
      print(paste0("x shift: ", x," / y shift: ", y, " / mean sq error: ", diff_sq_mean))
    }
  }
  return(coreg_results)
}

if (coregistrate_rasters){
  co_res <- coregistrate(b8_time1, b8_time2)
  co_min <- co_res[which.min(co_res[,3]),]
  print(paste0("best x shift: ",co_min[1], " / best y shift: ", co_min[2], " / mean sq error: ", co_min[3]))
  b4_time1 <- shift(b4_time1,co_min[1],co_min[2])
  b8_time1 <- shift(b8_time1,co_min[1],co_min[2])
}



######################################################################################################################################################################################
#### Calculation ####
######################################################################################################################################################################################
# NDVI of time 1
ndvi_time1 <- (b8_time1-b4_time1)/(b8_time1+b4_time1)

# NDVI of time 2
ndvi_time2 <- (b8_time2-b4_time2)/(b8_time2+b4_time2)

# NDVI difference
ndvi_diff <- ndvi_time2 - ndvi_time1

writeRaster(ndvi_diff, paste0("results/ndvi_diff_", time1,"_", time2,".tif"), overwrite=TRUE)

######################################################################################################################################################################################
#### Postprocessing ####
######################################################################################################################################################################################
# Clip to Area of interest (bounding box)
ndvi_diff <- raster(paste0("results/ndvi_diff_", time1,"_", time2,".tif"))
ndvi_diff_kt <- crop(ndvi_diff,kt)

# Clip to forest delination
if (!file.exists("wd/wa_ras.tif")){
  wa_ras <- rasterize(wa_agg,ndvi_diff_kt,mask=T)
  wa_ras[!is.na(wa_ras[])] <- 1
  writeRaster(wa_ras,"wd/wa_ras.tif")
}
wa_ras <- raster("wd/wa_ras.tif")
ndvi_diff_wa <- ndvi_diff_kt * wa_ras # This method was used, because raster::mask() did not work properly with polygons which have holes in them.

# Remove areas with clouds
if (remove_clouds){
  if (!file.exists("wd/cloud_ras.tif")){
    cloud_ras <- rasterize(clouds,ndvi_diff_kt,mask=T)
    cloud_ras[is.na(cloud_ras[])] <- 1
    cloud_ras[cloud_ras[]<1] <- 0
    writeRaster(cloud_ras,"wd/cloud_ras.tif",overwrite=T)
  }
  cloud_ras <- raster("wd/cloud_ras.tif")
  ndvi_diff_wa <- ndvi_diff_wa * cloud_ras # This method was used, because raster::mask() did not work properly with polygons which have holes in them.
}

writeRaster(ndvi_diff_wa, paste0("results/ndvi_diff_wa_", time1,"_", time2,".tif"), overwrite=TRUE)
removeTmpFiles(h=0)

######################################################################################################################################################################################
#### Data analysis ####
######################################################################################################################################################################################
ndvi_diff_wa <- raster(paste0("results/ndvi_diff_wa_", time1,"_", time2,".tif"))

length(which(!is.na(ndvi_diff_wa[])))/100 # forest area 
length(which(ndvi_diff_wa[]< (threshold)))/100 # Affected forest area in ha with threshold
length(which(ndvi_diff_wa[]< (threshold)))/length(which(!is.na(ndvi_diff_wa[])))*100 # Affected forest area in % with threshold


######################################################################################################################################################################################
#### Remove edge effects ####
######################################################################################################################################################################################
ndvi_diff_wa <- raster(paste0("results/ndvi_diff_wa_", time1,"_", time2,".tif"))

ndvi_recl <- reclassify(ndvi_diff_wa,matrix(c(-10000,threshold,1,threshold,1000,0),byrow = T,ncol = 3))
#writeRaster(ndvi_recl,"wd/ndvi_recl.tif", overwrite=TRUE)

ndvi_focal <- focal(ndvi_recl,matrix(rep(1,9),ncol = 3),fun=modal)
writeRaster(ndvi_focal, paste0("results/ndvi_diff_wa_focal_", time1,"_", time2,".tif"), overwrite=TRUE)

# Apply on NDVI-Difference dataset
ndvi_diff_focal <- ndvi_diff_wa * ndvi_focal
writeRaster(ndvi_diff_focal, paste0("results/ndvi_diff_final_", time1,"_", time2,".tif"), overwrite=TRUE)
######################################################################################################################################################################################
#### Data analysis without edge effects ####
######################################################################################################################################################################################
ndvi_focal <- raster(paste0("results/ndvi_diff_wa_focal_", time1,"_", time2,".tif"))

print(paste0("Affected forest area in ha: ", round(length(which(ndvi_focal[]==1))/100,2))) # Affected forest area in ha with threshold
print(paste0("Affected forest area in %: ", round(length(which(ndvi_focal[]==1))/length(which(!is.na(ndvi_focal[])))*100,2))) # Affected forest area in % with threshold

removeTmpFiles(h=0)



