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
library(raster) #downloading elevation data
library(tidyverse) # basic operations on the data

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


#' ## Covariate for the model

lulc <- raster("Landuse.tif")
elev <- raster("Elevation.tif")

#' # Resource selection function
rsf_dat <- data_df_trck |>
  random_points(n=5)

rsf_dat$elev <- raster::extract(elev, rsf_dat[,2:3])
rsf_dat$lulc <- raster::extract(lulc, rsf_dat[,2:3])
  
head(rsf_dat)

#' ## Check the points plot with the rasters
#' The red points are the *available* locations and the black dots are the *used* locations.
plot(elev)
points(rsf_dat$x_, rsf_dat$y_, col = as.factor(rsf_dat$case_), pch=19)

plot(lulc)
points(rsf_dat$x_, rsf_dat$y_, col = as.factor(rsf_dat$case_), pch=19)

#' #' # Model fitting
 modglm <-glm(case_ ~ elev + as.factor(lulc), data = rsf_dat) #try scale(elev) also
 summary(modglm)

 
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

#' # Step-selection functions
#' We will focus on step-selection functions here.
#' First we have to prepare the dataset for the step-slection analysis and then extract 
#' data from the rasters 

ssf_dat <- data_df_trck |> 
  steps() |> 
  random_steps() |>
  mutate( log_sl_ = log(sl_),
          cos_ta_ = cos(ta_))
  
ssf_dat$lulc <- raster::extract(lulc, cbind(ssf_dat$x2_, ssf_dat$y2_)) 
ssf_dat$elev <- raster::extract(elev, cbind(ssf_dat$x2_, ssf_dat$y2_)) 
  
#' ## Model fitting
#' Step-selection function uses a conditional logistic regression whereas the 
#' resource-selection function utilizes a generalized logistic regression 
m1 <- ssf_dat %>% 
  fit_issf(case_ ~ as.factor(lulc) + elev +  sl_ + log_sl_ + cos_ta_ + 
             strata(step_id_), model = TRUE)

summary(m1)

#' ## Distribution of Movement parameter
#' ### Step-length parameter
updated_sl <- update_sl_distr(m1)
print(updated_sl$params)

#'   data.frame for plotting
plot_sl <- data.frame(x = rep(NA, 500))

#' x-axis is sequence of possible step lengths
plot_sl$x <- seq(from = 0, to = 1000, length.out = 500)

#' y-axis is the probability density under the given gamma distribution
# For the tentative distribution
plot_sl$tentative <- dgamma(
  x = plot_sl$x, 
  shape = m1$sl_$params$shape,
  scale = m1$sl_$params$scale)

# For the updated distribution
plot_sl$updated <- dgamma(
  x = plot_sl$x,
  shape = updated_sl$params$shape,
  scale = updated_sl$params$scale)

# Pivot from wide data to long data
plot_sl <- plot_sl %>% 
  pivot_longer(cols = -x)

# Plot
ggplot(plot_sl, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  scale_color_manual(name = "Distribution", 
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()

#' ### Turn-angle parameter

updated_ta <- update_ta_distr(m1)
print(updated_ta$params)

plot_ta <- data.frame(x = rep(NA, 100))

# x-axis is sequence of possible step lengths
plot_ta$x <- seq(from = -1 * pi, to = pi, length.out = 100)

# y-axis is the probability density under the given von Mises distribution
# For the tentative distribution
plot_ta$tentative <- circular::dvonmises(
  x = plot_ta$x, 
  mu = m1$ta_$params$mu,
  kappa = abs(m1$ta_$params$kappa))

# For the updated distribution
plot_ta$updated <- circular::dvonmises(
  x = plot_ta$x, 
  mu = updated_ta$params$mu,
  kappa = abs(updated_ta$params$kappa))

# Pivot from wide data to long data
plot_ta <- plot_ta %>% 
  pivot_longer(cols = -x)

# Plot
ggplot(plot_ta, aes(x = x, y = value, color = factor(name))) +
  geom_line(size = 1) +
  xlab("Relative Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c(expression(-pi, -pi/2, 0, pi/2, pi))) +
  scale_color_manual(name = "Distribution", 
                     breaks = c("tentative", "updated"),
                     values = c("blue", "orange")) +
  theme_bw()