#' ---
#' title : "Movement Ecology Workshop- ATBC 2023"
#' subtitle: "Habitat selection Analysis"  
#' author: "Supratim, Nilanjan"
#' date: "`r format(Sys.time(), '%d %B, %Y')`" 
#' ---

#' ### Loading the libraries
#+ docPreamble, warning=FALSE, message=FALSE

library(move2) # download movement data
library(sf) # handle spatial data
library(ggplot2) # higher level plotting
library(tmap) # spatial data visualization
library(mapview) #interactive mapping
library(amt) # Movement data analysis
library(terra) #downloading elevation data
library(dplyr) # basic operations on the data


#' # Data generation
#' Download data from `Movebank` *https://www.movebank.org/cms/movebank-main* by study id and 
#' Check the columns of the data
#' ## Load telemetry data
dat <- mt_read("Hornbill telemetry in northeast India.csv")
head(dat)

dat_proj <-st_transform(dat, crs= "EPSG:32646")
coords <-st_coordinates(dat_proj)
data_df <-cbind(dat_proj, coords)


data_df_trck <-data_df |> 
  make_track( .x = X, .y = Y, .t = timestamp,
              crs= st_crs("EPSG:32646"), 
              Individual_id = individual.local.identifier)

#' ## Generate beackground points
## Function to generate the mcp
# mcpsf <- function(x, percent=95){
#   centroid <- sf::st_centroid(sf::st_union(x))
#   dist <- as.numeric(sf::st_distance(x, centroid))
#   within_percentile_range <- dist <= quantile(dist, percent/100)
#   x_filter <- st_union(x[within_percentile_range,])
#   st_convex_hull(x_filter)
# }
# 
# points <-10
# dat_sf <-st_as_sf(data_df, coords=c("location.long", "location.lat"), crs=4326)
# pop_hr_mcp<- mcpsf(dat_sf, percent=100)
# 
# pop_rand_pnt <- st_sample(pop_hr_mcp, size = nrow(data_df)*points, type = "random")
# final_dat <-as.data.frame(rbind(st_coordinates(dat_sf), st_coordinates(pop_rand_pnt)))
# final_dat$case <-rep(c(1,0), c(nrow(data_df),length(pop_rand_pnt)))
# 
# final_dat$trackId <- rep(data_df$trackId, (points+1))
# colnames(final_dat)[1:2] <-c("location.long", "location.lat")

#' ## Covariate for the model

lulc <- rast("Landuse.tif")
elev <- rast("Elevation.tif")

rsf_dat <- data_df_trck |>
  random_points()

rsf_dat$elev <-extract(elev, rsf_dat[,2:3])[,2]
rsf_dat$lulc <-extract(lulc, rsf_dat[,2:3])[,2]
  
head(rsf_dat)

#' #' # Model fitting
 modglm <-glm(case_ ~ elev+lulc  ,data = rsf_dat) #try scale(elev) also
 summary(modglm)
#' 
#' #' ## Arranging the regression output
output <- as.data.frame(summary(modglm)$coefficients)

modplot <-ggplot(output) +
  geom_point(aes(x= rownames(output), y= Estimate), col ="blue")+
  geom_linerange(aes(x= rownames(output),
                     ymin= Estimate-1.96*`Std. Error` ,
                     ymax= Estimate+1.96*`Std. Error`))+
  labs(x= "Variable", y= "Estimate")+
  theme_bw()

modplot