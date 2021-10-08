# Analyse NDVI changes with satellite imagery (Sentinel-2)
Created by Raffael Bienz
08.10.2021

The example data is provided by:
- Satellite imagery: Copernicus Sentinel data 2020+2021, processed by ESA.
- Shapefiles of study area and forest delination: Kanton of Aargau, 2021.

## Usage

### Clone repository
```
git clone https://github.com/RaffiBienz/ndvi_analyst.git
```

### Required data
Download satellite imagery (Sentinel-2) of two moments in time. For example from: https://scihub.copernicus.eu/dhus/
For the analysis  only bands 4 and 8 are needed.

Further the following data is required as shapefiles:
- Delination of the area of interest
- Delination of forest areas
- Delination of clouds in the satellite images (if any)

Copy all  datasets into the data folder.

### Setup R
- Install R and if desired RStudio (Scirpt was tested with R version 4.0.3).
- Required packages: rgdal, rgeos, raster (These packages are automatically installed, when the script is run for the first time)

### Preparations
Set the following paramters in NDVI_analyst.R:
- Working directory
- Coregistration Variable -> Sometimes images from two differnt times do not exactly match (check in GIS). If set to TRUE, a simple coregsitration method is applied, to find a shift, which minimizes the differences between the two rasters.
- Threshold: Which minimum NDVI reduction is regarded as a change.
- Time variables
- Paths to the satellite images
- Name of the forest delination shapefile
- Name of the area of interest shapefile
- Cloud removal variable -> If clouds are present on any of the satellite images, create a shapefile in a GIS and mark the clouds with polygons. Then copy the shapefile into the data folder and set this variable to TRUE.
- Name of the cloud shapefile

### Execution
Execute the scirpt in R-Studio or on the command line with:
```
Rscript NDVI_analyst.R
```

Different files are generated in the results folder. The two most important are:
- ndvi_diff_final_....tif: Difference in NDVI between the two moments in time.
- ndvi_diff_wa_focal_....tif: Areas with changes according to the defined threshold.




