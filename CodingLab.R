# Coding lab for "The Magic of Where:How Spatial Data Strengthen Insights into the World"
# By Zhan Wang, Department of Agricultural Economics, Purdue University (zhanwang@purdue.edu)
# June 21, 2023

# Preparation ----
# Check if necessary packages have been installed or not. If not, install them
necessaryPackages <- c("plyr", "raster", "RColorBrewer","RColorBrewer","rstudioapi","maptools")  
toInstallPackages <- necessaryPackages[!(necessaryPackages %in% installed.packages()[ , "Package"])]   
if(length(toInstallPackages))
{
  install.packages(toInstallPackages, repos="http://cran.us.r-project.org") 
}

# Initialization ----
# Clean up work space
rm(list=ls())
# Load necessary packages
library(rgdal)
library(raster)
library(plyr)
library(RColorBrewer)
library(maptools)
library(rstudioapi)
# Set the path of workspace to be the current folder
scriptpath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(scriptpath))
print(getwd())

# Step 1: Load data ----

# Load the table of corn yield at baseline (2022)
# This file is downloaded from USDA website:
# https://www.nass.usda.gov/Statistics_by_State/
# Under "Indiana" - "County Estimate" - "Corn County Estimates" - "2022Corn" - "QuickStats" - "Spreadsheet"

# Feel free to explore the website more for data for other crops / states
# If you cannot find the data you want under state's page, please refers to 
# https://quickstats.nass.usda.gov/
# for direct data downloading

df.corn = read.csv("inputs/0E8F68A9-8189-3852-9062-3D693EBA7F9D.csv")
# check column names of the table
colnames(df.corn)
# Variables we will use in this lab are:
# number 7 "State.ANSI"
# number 10 "County" 
# number 11 "County.ANSI" 
# number 25 "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE  -  <b>VALUE</b>#
# Rename variable number 25 to "Yield" (unit: BU / acre)
colnames(df.corn)[25] = "Yield"
# Open the table
View(df.corn)
# We can find the last row is the sum of all counties in Indiana
# Its "Geo Level" variable equals "STATE". So we can get rid of it by excluding rows with "Geo Level"  = "STATE"
df.corn = subset(df.corn, Geo.Level != "STATE")
# Get rid of other variables
df.corn = subset(df.corn, select = c(State.ANSI, County.ANSI, County, Yield))
# View the table again，remember the state ID for Indiana equals 18
View(df.corn)

# Load the shapefile of counties in Indiana
# This file is downloaded from US Census website:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# Under "2018" - " County" -  "cb_2018_us_county_20m.zip"
shp.county = readOGR(dsn="inputs/cb_2018_us_county_20m/cb_2018_us_county_20m.shp")
# Visualize the shapefile just loaded
plot(shp.county)
# We also find this shapefile contains all counties in the states, but we only need states in Indiana
# The data in a shapefile can be accessed with shp@data, which is in table format
# Let us print the first five county's information with head function
head(shp.county@data)
# Here we can see the state index in shapefile is "STATEFP", and county index is "COUNTFP"
# First, select counties in Indiana only
shp.Indiana = subset(shp.county, STATEFP==18)
# Visualize the shapefile
plot(shp.Indiana)

# Step 2: Match yield data with county for visualization ----
# check the data of indiana counties from shapefile and tables again
shp.Indiana@data
df.corn
# We can find the county ID in shapfile is named "COUNTYFP"
# So we need to change the variable name of county ID in USDA data to be "COUNTYFP" as well
# In order to match the data

df.corn.data = subset(df.corn, select = c(County.ANSI, Yield))
colnames(df.corn.data)[1] = "COUNTYFP"

shp.Indiana@data$COUNTYFP = as.numeric(shp.Indiana@data$COUNTYFP)
shp.Indiana@data = join(shp.Indiana@data, df.corn.data)
# Some counties do not have corresponding records from USDA. To check them:
shp.Indiana@data[is.na(shp.Indiana@data$Yield),]
# Let us use the value "OTHER COUNTIES" for those counties
shp.Indiana@data[is.na(shp.Indiana@data$Yield),]$Yield = df.corn[df.corn$County == "OTHER COUNTIES",]$Yield

# Let us visualize the yield data at baseline
numColor = 5
yieldColors = brewer.pal(numColor, "Greens") 
yieldClass = cut(shp.Indiana$Yield, numColor)
yieldColorsClass= yieldColors[as.numeric(yieldClass)]

plot(shp.Indiana, col=yieldColorsClass,  bg = "White")
text(shp.Indiana, labels = shp.Indiana$Yield, halo = TRUE, hw = 0.08, hc = "white", cex = 0.8)
legend("left", title = "Yield (bu/Acre)", legend=unique(levels(yieldClass)),
       fill=yieldColors)

# We can compare it with the figure made by USDA:
# https://www.nass.usda.gov/Statistics_by_State/Indiana/Publications/County_Estimates/2022/2022_in_corn_CE.pdf
# We are doing a good job!

# Step 3: Calculate average yield change by county ----
# Load the raster file of corn yield
# We are using the output from AgMIP tool with the following parameters:
# Crop model: EPIC
# Global climate model: HadGEM2-ES
# RCP: RPC8.5
# With CO2 fertilization
# No irrigation
# Crop: maize

# The first raster contains data in 31 bands for simulations between 2005 and 2035. 
# Here we need to get the data for year 2022, or band 18
r.baseline = raster("inputs/epic_hadgem2-es_rcp8p5_ssp2_co2_noirr_yield_mai_annual_2005_2035.nc4", band = 18)
# The second raster contains data in 31 bands for simulations between 2035 and 2065. 
# Here we need to get the data for year 2050, or band 16
r.future = raster("inputs/epic_hadgem2-es_rcp8p5_ssp2_co2_noirr_yield_mai_annual_2035_2065.nc4", band = 16)

# Let us visualize them
plot(r.baseline)
plot(r.future)

# In the next step, we need to clip those raster to Indiana only
# First of all, make sure the coordinate reference system of raster and shapefile are consistent
shp.Indiana = spTransform(shp.Indiana, proj4string(r.baseline))
# Let us start with checking the actual extent of Indiana
extent(shp.Indiana)
# The raster dataset has the resolution of 0.5 degree
# So let us prepare the extent that contains the entire Indiana
ext = extent(-88.5, -84.5, 37.5, 42)

# Use this extent to crop the raster to Indiana
r.baseline.Indiana <- crop(r.baseline, ext)
r.future.Indiana <- crop(r.future, ext)

# To check our work, visualize the raster and shapefile together
plot(r.baseline.Indiana) 
plot(shp.Indiana, add=TRUE)

plot(r.future.Indiana) 
plot(shp.Indiana, add=TRUE)

# Let us plot these figures with the same range of values, so they are directly comparable
plot(r.baseline.Indiana, zlim = c(3, 11)) 
plot(shp.Indiana, add=TRUE)

plot(r.future.Indiana, zlim = c(3, 11)) 
plot(shp.Indiana, add=TRUE)

# Step 4: Project future yield ----
# One interesting feature of raster in R is that you can perform math operations directly
# It is equivalent with performing the same operation for each grid

# First, calculate the difference of crop yield between 2050 and 2022
r.change = r.future.Indiana - r.baseline.Indiana

# Second, calculate the percentage change of crop yield by 2050, comparing with 2022
r.pctChange = r.change / r.baseline.Indiana * 100
plot(r.pctChange)
plot(shp.Indiana, add=TRUE)

# Third, we need to mapping the projected change in yield from grid level to county level
# We will use zonal statistics to calculate the average yield change in each county

pctChange = extract(r.pctChange, shp.Indiana, fun = mean, na.rm = TRUE, df = TRUE)

shp.Indiana@data$yieldPctChange = round(pctChange$layer,0)

numColor = 5

yieldpctColors = brewer.pal(numColor, "Blues") 
# Here the order of colors are reversed, so darker color means higher reduction
yieldpctColors = rev(yieldpctColors)
yieldpctClass = cut(shp.Indiana$yieldPctChange, numColor)
yieldpctColorsClass= yieldpctColors[as.numeric(yieldpctClass)]

plot(shp.Indiana, col=yieldpctColorsClass,  bg = "White", main = "Percentage change in corn yield \nfrom 2022 to 2050")
text(shp.Indiana, labels = shp.Indiana$yieldPctChange, halo = TRUE, hw = 0.08, hc = "white", cex = 0.8)
legend("left", title = "Change (%)", legend=unique(levels(yieldpctClass)),
       fill=yieldpctColors)


shp.Indiana@data$YieldProjected = shp.Indiana@data$Yield * (1 + shp.Indiana@data$yieldPctChange/100) 

# Step 5: Visualization of climate change's impact on yield ----
# For visualization purpose, round the yield to integers
shp.Indiana$Yield = round(shp.Indiana$Yield,0)
shp.Indiana$YieldProjected= round(shp.Indiana$YieldProjected,0)

# mapping all yields (baseline and projected) with colors
allYield = c(shp.Indiana$Yield, shp.Indiana$YieldProjected)
length(allYield)
numColor = 9

yieldAllColors = brewer.pal(numColor, "Greens") 
yieldAllClass = cut(allYield, numColor)
yieldAllColorsClass= yieldAllColors[as.numeric(yieldAllClass)]

# Plot baseline and projected yield in parallel
# Save the current setting of parallel plot first
def.par = par()
par(mfrow = c(1, 2), xpd = NA)
plot(shp.Indiana, col=yieldAllColorsClass[1:length(allYield)/2],  bg = "White", main = "Observed Corn Yield \n(2022)")
text(shp.Indiana, labels = shp.Indiana$Yield, halo = TRUE, hw = 0.08, hc = "white", cex = 0.8)

plot(shp.Indiana, col=yieldAllColorsClass[length(allYield)/2+1:length(allYield)],  bg = "White",  main = "Projected Corn Yield\n (2050)")
text(shp.Indiana, labels = shp.Indiana$YieldProjected, halo = TRUE, hw = 0.08, hc = "white", cex = 0.8)

legend("left", title = "Yield (bu/Acre)", legend=unique(levels(yieldAllClass)),
       fill=yieldAllColors, horiz = F, bty = "n", inset=c(-0.3,0))
# After use, save all figures and set the default setting back
# Note: this command will clean up all figures generated.
dev.off()