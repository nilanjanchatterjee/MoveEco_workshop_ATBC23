#' ---
#' title : "Movement Ecology Workshop"
#' subtitle: "ATBC 2023"  
#' author: "Supratim, Nilanjan, Ritesh"
#' date: "`r format(Sys.time(), '%d %B, %Y')`" 
#' ---


#' There are currently hundreds of libraries to analyse movement data. For a detailed review please see Joo et al. 2019.
#' We will use some important core packages necessary for analysis. This is by no means an exhaustive list.
#' Please install the updated version of these packages or update if you have an earlier version. 
#' 

#' ### Loading the libraries
#+ docPreamble, warning=FALSE, message=FALSE

library(move2) # download movement data
library(sf) # handle spatial data
library(ggplot2) # higher level plotting
library(tmap) # spatial data visualization
library(amt) # Movement data analysis
library(adehabitatLT) #movement data analysis
library(dplyr) # basic operations on the data


#' # Data Processing
#' Download data from `Movebank` *https://www.movebank.org/cms/movebank-main* by study id and 
#' Check the columns of the data

dat <- mt_read("Hornbill telemetry in northeast India.csv")
head(dat)
glimpse(dat)

#' # Plotting the data
#' We'll use either *ggplot* or *tmap* package to plot the data
#' We will also convert the spatial points into line-strings to plot the trajectory of the animal
trck <- dat %>% 
  group_by(`individual-local-identifier`) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

ggplot()+
  geom_sf(data= trck , aes(col= `individual-local-identifier`))+
  geom_sf(data= dat , aes(col= `individual-local-identifier`), size=0.8)+
  theme_bw()+
  labs(col = "Individual_id")



#' # Creating *track* from the data
#' The coordinates were extracted from the dataset downloaded from `Movebank` *https://www.movebank.org/cms/movebank-main* 
#' and then added that to the dataset as the downloaded dataset have a single column for the geometry.
#' The **CRS** used here to convert the lat-long projection system into a lcc projection system as the 
#' calculations further (step-length) will be done in meters.

dat_proj <-st_transform(dat, crs= "ESRI:102030")
coords <-st_coordinates(dat_proj)
data_df <-cbind(dat_proj, coords)


data_df_trck <-data_df |> 
   make_track( .x = X, .y = Y, .t = timestamp,
              crs= st_crs("ESRI:102030"), Individual_id = individual.local.identifier)
 
head(data_df_trck) #Check the track object from the *amt* package

#' # Basic trajectory statistics
#' In this step, we would calculate basic summary statistics for each of the tagged individuals
#' The output table shows summary distribution of the fix intervals for each individuals, how many locations were
#' collected 

track_sum <-data_df_trck %>% 
  nest(data= -"Individual_id") %>%  
  mutate(sr = map(data, summarize_sampling_rate)) %>% 
  dplyr::select(Individual_id, sr) %>% 
  unnest(cols=sr) %>% 
  print(n=nrow(data_df_trck))

track_length <- data_df_trck |>
  nest(data= -"Individual_id") |>
  mutate(data = map(data, ~ .x |>
                      steps()))|>
  unnest(cols = data)


#' # Plotting trajectory distribution
#' We will plot the individual-wise distributions of step-length (m) and 
#' turn angle (radians) to see how individuals move
ggplot(track_length)+
  geom_density(aes(x=sl_, col = Individual_id), linewidth=0.8) + 
  xlim(0,1000)+
  labs(x= "Step_length(m)")+
  theme_bw() +
  theme(legend.position = c(0.8,0.7))
 
ggplot(track_length)+
  geom_density(aes(x=ta_, col = Individual_id), linewidth=0.8) + 
  labs(x= "Turn angle")+
  theme_bw() 

#' # Home-range Analysis
#' There are multiple `R` packages available for Home-range analysis. The *adehabitat* packages are 
#' really important for home-range and trajectory analysis
#' Here we would use the functions from the `amt` package as those can be used together with the data
dat_hr_mcp <- data_df_trck |>
  nest(data = -"Individual_id") |>
  mutate(mcp_dat = map(data, ~ .x |>
                      hr_mcp(levels = c(0.5,0.95)))) |>
  mutate(mcp_area = map(.x = mcp_dat, .f = ~hr_area(.x)))|>
  unnest(cols = mcp_area)
colnames(dat_hr_mcp)[6] <- "MCP_area (sq.mtr)"

dat_hr_kde <- data_df_trck |>
  nest(data= -"Individual_id") |>
  mutate(kde_dat = map(data, ~ .x |>
                         hr_kde(levels = c(0.5,0.95)))) |>
  mutate(area = map(.x = kde_dat, .f = ~hr_area(.x)))|>
  unnest(cols = area)
colnames(dat_hr_kde)[6] <-"KDE_area (sq.mtr)"

dat_hr <- cbind(dat_hr_mcp[,c(1,4,6)], dat_hr_kde[,6])
dat_hr

#' # Plotting the Home-range
#' # Calculate the convex hulls for each individual
hull_dat <- data_df %>%
  group_by(individual.local.identifier) %>%
  slice(chull(X, Y))

# Update the plot with a fill group, and overlay the new hulls
ggplot(data_df, aes(X, Y)) + geom_point(shape = 21)+ 
  aes(fill = individual.local.identifier) + 
  geom_polygon(data = hull_dat, alpha = 0.5)+
  labs(fill = "Individual_id")+
  theme_bw() + theme(legend.position = c(0.85,0.75))
