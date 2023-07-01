#' ---
#' title : "Movement Ecology Workshop- ATBC 2023"
#' subtitle: "Habitat selection Analysis"  
#' author: "Supratim, Nilanjan"
#' date: "`r format(Sys.time(), '%d %B, %Y')`" 
#' ---

#' ### Loading the libraries
#+ docPreamble, warning=FALSE, message=FALSE

library(adehabitatLT)
library(move)
library(adehabitatHR)
library(mapview)
library(lubridate)
library(sp)
library(rgdal)
library(maptools)
library(raster)
#library(spatialEco)
library(reshape2)
library(ggplot2)
library(ctmm)

# Set the working directory
#setwd("G:/CU")
#dir()
#' ## Load data
data <- read.csv("Leopard.csv", header = TRUE)
#View(data)

#' ## Make the dataframe
data_df <-as.data.frame(data)
head(data_df)

#' ## Prepare the timestamp
data_df$date <- as.POSIXct(paste(data_df$year, data_df$month, data_df$day, data_df$hh, data_df$mm, data_df$ss), format = "%y %m %d %H %M %S")

#' ## Trajectory preparartion
data <- as.ltraj(xy = data_df[,c("x","y")], date = data_df$date,id = "data", typeII = TRUE)

#' ##Trajectory
dat <- data[["traj"]][[1]]

# lets change our date to a universal format, readable by R
dat$datetime <- as.POSIXct(paste(data_df$year, data_df$month, data_df$day, data_df$hh, data_df$mm, data_df$ss), format = "%y %m %d %H %M %S")

# Now we can isolate the date only and add that as the date column
dat$date <- as.Date(dat$datetime, format="%Y-%m-%d")

# add the CRS projection to the data and create a spatial object
sp.UTM <- SpatialPoints(cbind(data_df$x, data_df$y), #data_df,
                        proj4string = CRS("+init=epsg:32735"))

#'
#' #~~~~~~~~~~~~~~~~~~~~ MCP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usually when representing the general home range of a species, 
# weather it be for land conservation or species monitoring purposes, 
# we use Minimum Convex Polygons (MCP) for our movement data

# using the adehabitatHR package, we will make an MCP for our animal
# and make the total homerange area output in km squared for the units
MCP95 <- mcp(sp.UTM, percent = 95, unin = "m", unout = "km2")
MCP50 <- mcp(sp.UTM, percent = 50, unin = "m", unout = "km2")

#' ## Area calculation
MCP95
MCP50

#' ## Check our MCP and points
mapview (MCP95) + (MCP50)

#' ##Prepare the map
mapview(MCP95, col.regions = "red") +  
  mapview(MCP50, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.5, cex = 0.5)

mapview(MCP95, col.regions = "red", map.type = "OpenStreetMap") + 
  mapview(MCP50, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.1, cex = 0.5)

mapview(MCP95, col.regions = "red", map.type = "OpenTopoMap") + 
  mapview(MCP50, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.1, cex = 0.5)

#' ## Save the raster layer
#writeOGR(obj=MCP95,dsn=".",layer="MCP95",driver="ESRI Shapefile")
#writeOGR(obj=MCP50,dsn=".",layer="MCP50",driver="ESRI Shapefile")

#' # Kernel Density Estimation

# next we will create Kernel Density Estimator (KDE) polygons representing 
# the uitization density of the homerange. There are several options 
# available for KDE called "smoothing factors", however the most 
# commonly used are href, Least Squared Cross Validation, and manual
# plugin for the smoothing value. the polygons created by KDE
# are referred to as "isopleths" a 99% isopleth contour represents the
# total range, a 95% isopleth contour represents the most likely range
# and the 50% isopleth contour represents the "Core area" or area with
# the most abundance of points

#' ## Bandwith 'href'
Khref <- kernelUD(sp.UTM, h = 'href', grid = 100, extent = 2.2)
Khref50 <- getverticeshr(Khref, 50, unin = "m", unout = "km2")
Khref95 <- getverticeshr(Khref, 95, unin = "m", unout = "km2")

#' ## Area calculation
Khref50
Khref95

#' ##Prepare the map
mapview(Khref50, col.regions = "red") +  
  mapview(Khref95, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.5, cex = 0.5)

#' ## Bandwith 'LSCV'
KLSCV <- kernelUD(sp.UTM, h = 'LSCV', grid = 500, extent = 2.2)
KLSCV50 <- getverticeshr(KLSCV, 50, unin = "m", unout = "km2")
KLSCV95 <- getverticeshr(KLSCV, 95, unin = "m", unout = "km2")

#' ## Area calculation
KLSCV50 
KLSCV95

#' ##Prepare the map
mapview(KLSCV50, col.regions = "red") +  
  mapview(KLSCV95, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.5, cex = 0.5)

#' ## Bandwith 'manual plug-in'
Kh800 <- kernelUD(sp.UTM, h = 800, grid = 1000, extent =2.2)
Kh80095 <- getverticeshr(Kh800, 95, unin = "m", unout = "km2")
Kh80050 <- getverticeshr(Kh800, 50, unin = "m", unout = "km2")

#' ## Area calculation
Kh80095
Kh80050

#' ##Prepare the map
mapview(Kh80095, col.regions = "red") +  
  mapview(Kh80050, col.regions = "green") +
  mapview(sp.UTM, alpha = 0.5, cex = 0.5)

#' ## Save the raster layer
#writeOGR(obj=Kh80095,dsn=".",layer="KDE95",driver="ESRI Shapefile")
#writeOGR(obj=Kh80050,dsn=".",layer="KDE50",driver="ESRI Shapefile")


#' # AKDE Home-range Calculation
#' ## Load data
#' ctmm requires the data in *telemetry* object. You can convert your data using the *as.telemetry* function
dat <- read.csv("Hornbill telemetry in northeast India.csv")
ctmm_dat <-as.telemetry(dat)

#' extract data from one individual
indv1 <- ctmm_dat$`5_rifle`

ind_var1 <- variogram(indv1)
plot(ind_var1)

#' # Estimate an initial model
GUESS <- ctmm.guess(indv1, variogram= ind_var1, interactive=FALSE)

#' # Select the best model
FIT <- ctmm.select(indv1, GUESS, trace=2)
summary(FIT)

#' # AKDE home-range
ud <- akde(indv1, FIT, weights=TRUE)
summary(ud)

plot(ud)
