# Analyse NDVI changes with satellite imagery (Sentinel-2)
Created by Raffael Bienz

14.08.2026

The example data is provided by:
- Satellite imagery: Copernicus Sentinel data, processed by ESA, and swissEO S2-SR data by swisstopo.
- Vector data of the area of interest and forest delineation: Canton of Aargau, 2026.

## Usage

### Clone repository
```
git clone https://github.com/RaffiBienz/ndvi_analyst.git
```

### Required data
Download satellite imagery for two dates. If you use swissEO S2-SR data, it can be obtained from: https://www.swisstopo.admin.ch/de/satellitenbilder-swisseo-s2-sr

Further, the following vector data is required:
- Delineation of the area of interest
- Delineation of forest areas
- If necessary: Delineation of cloud-covered areas in the satellite images

Copy all datasets into the data folder.

### Setup R
- Install R and if desired RStudio
- Required package: terra (the package is automatically installed when the script is run for the first time)

### Preparations
Set the following parameters in NDVI_analyst.R:
- Working directory
- Data source: The script works for swissEO S2-SR data and for data directly acquired from Copernicus.
- Threshold: Minimum NDVI reduction regarded as a change.
- Time variables
- Paths to the satellite images
- Path to the forest delineation 
- Path to the area of interest 
- Cloud removal variable: If clouds are present on any of the satellite images, create a shapefile or gpkg in a GIS and mark the clouds with polygons. Then copy the dataset into the data folder and set this variable to TRUE.
- Path to the cloud dataset

### Execution
Execute the script in RStudio or on the command line with:
```
Rscript NDVI_analyst.R
```

Different files are generated in the results folder. The two most important are:
- ndvi_diff_wa_....tif: NDVI difference cropped to the area of interest and masked to forest area.
- ndvi_diff_binary_....tif: Binary raster of detected change after thresholding and edge-effect reduction.




