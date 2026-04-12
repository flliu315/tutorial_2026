# --------------------------------------------
# Script Name: geodata EDA and modeling
# Purpose: Here is the script about how to conduct EDA of geodata 
#          and model the spatial pattern, using doubs river fishes
#          as example to show how to extract env information along 
#          doubs river with the integration of R and QGIS. Also,
#          writing the code includes how to create spatial lag features 
#          using a spatial weight matrix and how to include these 
#          spatial features in a machine learning model.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-03-09
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################

# 01-access basic data of AOI (area of interest)

# A) river data and sampling point coordinates

# import the .csv file of coordinate x, y

doubs_xy <- read.csv("data/data_db/DoubsSpa.csv", 
                     row.names = 1)

# write.csv(doubs_xy, "data/geo_data/pointcoord_utm.csv")

# getting the geocoordinates of sample points using qgis

# // The 1st step  
# creating coordinates_utm.csv to an image with qgis
# Add Layer -> Add Delimited Text Layer -> 
# Project -> Export -> Export as image (sample_points.png)

# loading doubs_river.geojson created in 06_data_eda
# as reference for georeferencing the sample_points.png

# // The 2nd step 
# a) using freehand geoferencer to georeference png
# river map -> AD (adding png) -> geroreferencing
# https://www.youtube.com/watch?v=fzz8jw7Qp18 
# b) using the georeferencer  in layer
# Layer -> # Georeferencer.. -> raster
# https://www.youtube.com/watch?v=0CT3Un9v-6Q

# // The 3rd step
# installing and enabling coordinate capture plugin 

# Loading Georeferenced Image into QGIS
# Layer > Add Layer > Add Raster Layer
# click the gear ⚙️ to choose the CRS
# Layer > Create Layer > New Shapefile Layer
# Toggle -> Add Point Feature -> Save edits
# Export > Save Features As... -> AS_XY

library(tidyverse)
pointcoord_geo <- read_csv("data/geo_data/DoubsSpa_geo.csv")

# # Comparing the geo-referenced with the original
# load("data/geo_data/Doubs.RData",  Doubs <- new.env())
# ls.str(Doubs)
# latlong <- Doubs$latlong
# latlong

# Doubs <- load("data/geo_data/Doubs.RData")
# head(Doubs)

# creating sf object

library(sf)
points_sf <- read.csv("data/geo_data/pointcoord_geo.csv", 
                      sep = ",") |> # set ";" if x.y is ";"
  st_as_sf(coords=c("X","Y"), crs=4326) 

# st_write(points_sf, "data/geo_data/sample_points.shp")

# B) getting other public data
# get DEM data covered by Le Doubs river

## load the shapefile of Ld Doubs rive 

doubs_river <- st_read("data/geo_data/doubs_river.shp") # 06_eda
head(doubs_river)

# # install.packages("remotes")
# remotes::install_github("rspatial/geodata")

# library(elevatr)
# doubs_elev <- get_elev_raster(doubs_river, z = 10) # z resolution
# doubs_elev
# terra::writeRaster(doubs_elev, "data/geo_data/doubs_dem.tif",
#                    filetype = "GTiff", overwrite = TRUE)

# Visualizing river, locations and dem

library(terra)

doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
terra::plot(doubs_dem, main="doubs river elevation")

doubs_river <- sf::st_read("data/geo_data/doubs_river1.shp")
doubs_pts <- sf::st_read("data/geo_data/sample_points.shp")

plot(doubs_pts, add = TRUE, cex =1.8, col = "red")
plot(doubs_river, add = TRUE, col = "yellow")

########################################################
# 02-extracting spatial features to add as predictors
########################################################
## A) set a 5-km buffer along rive

library(terra)
library(sf)

doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
doubs_river <- sf::st_read("data/geo_data/doubs_river.shp")
doubs_pts <- sf::st_read("data/geo_data/sample_points.shp")

# re-projecting the vector data of river
doubs_river_utm <- st_transform(doubs_river, 
                                32631) 

# creating and visualizing the buffer

doubs_river_buff <- st_buffer(doubs_river_utm, dis = 8000)
plot(st_geometry(doubs_river_buff), axes = TRUE)

library(ggplot2)
ggplot() + geom_sf(data = doubs_river_buff)

# st_write(doubs_river_buff, 
#          "data/geo_data/doubs_river_buff.shp")

# B) Clip or intersect dem covered by river buffer
# reprojecting raster data
doubs_dem <- terra::rast("data/geo_data/doubs_dem.tif")
terra::crs(doubs_dem) # get CRS
utm_crs <- "EPSG:32631" # set CRS
doubs_dem_utm <- terra::project(doubs_dem,utm_crs)
terra::crs(doubs_dem_utm) # check crs

# Clip or intersect dem by doubs river

doubs_dem_utm_cropped = terra::crop(doubs_dem_utm,
                             doubs_river_buff)
plot(doubs_dem_utm_cropped)
doubs_dem_utm_masked = terra::mask(doubs_dem_utm_cropped,
                            doubs_river_buff)
plot(doubs_dem_utm_masked)
# writeRaster(doubs_dem_utm_masked, "data/geo_data/doubs_dem_crop.tif")

doubs_dem_crop <- terra::rast("data/geo_data/doubs_dem_crop.tif")
plot(doubs_dem_crop, axes = TRUE)

# C) extracting raster values of points as predictors
# https://r.geocompx.org/eco

library(qgisprocess)
qgis_configure()
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title, algorithm) |>
  head(2)

# catchment slope and catchment area
topo_total = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagawetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1, 
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

topo_select <- topo_total[c("AREA", "SLOPE")] |>
  unlist() |>
  rast() #  catchment area and slope
topo_select
names(topo_select) = c("carea", "cslope") # assign names
origin(topo_select) <- # where grid begins
  terra::origin(doubs_dem_crop) # the same origin

topo_char = c(doubs_dem_crop, topo_select) # add dem to SpatRaster

# writeRaster(topo_char, "data/geo_data/topo_char.tif", overwrite=FALSE)

# reprojecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, 32631)
dim(doubs_pts_utm)
# st_write(doubs_pts_utm,"data/geo_data/doubs_pts_utm.shp")

# extracting raster values
topo_char <- terra::rast("data/geo_data/topo_char.tif")
doubs_pts_utm <- st_read("data/geo_data/doubs_pts_utm.shp")
  
topo_points <- terra::extract(topo_char, doubs_pts_utm, ID=FALSE)
glimpse(topo_points)

# aggregating topo and water chemical env
env <- read.csv("data/data_db/DoubsEnv.csv", 
                     row.names = 1)

water_env <- env
points_env = cbind(doubs_pts_utm, topo_env, water_env) # convert dataframe to SpatRaster

sf::st_write(points_env,  paste0("data/geo_data/points_env.shp"))

Doubs <- load("data/geo_data/Doubs.RData")
Doubs
spe 
spe_clean <- spe[!(rowSums(spe) == 0),]
dim(spe_clean)

# species abundance
abund <- rowSums(spe_clean)

# # species diversity 
# library(vegan)
# N0 <- rowSums(spe_clean > 0) # Species richness
# H <- vegan::diversity(spe_clean) # Shannon entropy
# N1 <- exp(H) # Shannon diversity (number of abundant species)
# N2 <- diversity(spe_clean, "inv") # Simpson diversity (number of dominant species)
# J <- H/log(N0) # Pielou evenness
# E10 <- N1/N0 # Shannon evenness (Hill's ratio)
# E20 <- N2/N0 # Simpson evenness (Hill's ratio)
# (div <- data.frame(N0, H, N1, N2, E10, E20, J))

env_clean <- doubs_env[-8,]

env_fish <- cbind(env_clean, abund) |>
  dplyr::rename(fish_abund = abund)


# sf::st_write(env_fish, paste0("data/geo_data", "/",
#                                "env_fish.shp"),
#              append=FALSE)

# # ########################################################
# # ## Methods for ESDA and ML of spatial polygons
# # #######################################################
# # # https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html
# # 
# # A) the global spatial autocorrelation
# 
# library(spData)
# library(sf)
# library(spdep)
# library(ggplot2)
# 
# map <- st_read(system.file("shapes/columbus.shp",
#                            package = "spData"), quiet = TRUE)
# plot(st_geometry(map), border = "lightgray")
# 
# map$vble <- map$CRIME # the focusing variable
# # mapview(map, zcol = "vble")
# 
# p_vble = # create the map
#   ggplot(map) +
#   geom_sf(aes(fill = map$vble)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# p_vble
# 
# 
# library(spdep)
# nb <- poly2nb(map, queen = TRUE) # determine adjacency
# 
# library(tmap)
# # examine zero links locations
# map$rn = rownames(map)
# tmap_mode("view")
# tm_shape(map) +
#   tm_borders() +
#   tm_text(text = "rn") +
#   tm_basemap("OpenStreetMap")
# tmap_mode("plot")
# 
# # Create a line layer showing Queen's case contiguity
# gg_net <- nb2lines(nb,coords=st_geometry(st_centroid(map)),
#                    as_sf = F)
# # Plot the contiguity and the map layer
# p_adj =
#   ggplot(map) + geom_sf(fill = NA, lwd = 0.1) +
#   geom_sf(data = gg_net, col='red', alpha = 0.5, lwd = 0.2) +
#   theme_minimal() + labs(subtitle =  "Adj")
# p_adj
# 
# # spatial weights Matrix and the lagged means
# 
# nbw <- spdep::nb2listw(nb, style = "W") # compute weight matrix from nb
# nbw$weights[1:3]
# map$lagged_means <- lag.listw(nbw, map$vble) # compute lagged means
# p_lagged =
#   ggplot(map) + geom_sf(aes(fill = lagged_means)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue") +
#   theme_minimal()
# 
# cowplot::plot_grid(p_vble, p_lagged)
# 
# p_lm = # create a lagged mean plot
#   ggplot(data = map, aes(x = vble, y = lagged_means)) +
#   geom_point(shape = 1, alpha = 0.5) +
#   geom_hline(yintercept = mean(map$lagged_means), lty = 2) +
#   geom_vline(xintercept = mean(map$vble), lty = 2) +
#   geom_abline() +
#   coord_equal()
# p_lm
# 
# # create a Moran plot and statistic test using weighted list
# moran.plot(x = map$vble, listw = nbw, asp = 1)
# 
# moran.test(x = map$vble, listw = nbw) # for Moran’s I for statistic test
# 
# moran.range <- function(lw) {
#   wmat <- listw2mat(lw)
#   return(range(eigen((wmat + t(wmat))/ 2) $values))
# }
# 
# moran.range(nbw) # strongly clustered
# 
# # B) local spatial autocorrelation and clusters
# 
# # Compute the local Moran’s I
# map$lI <- localmoran(x = map$vble, listw = nbw)[, 1]
# 
# p_lisa = # create the map
#   ggplot(map) +
#   geom_sf(aes(fill= lI), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0, name = "Local\nMoran's I",
#                        high = "darkgreen", low = "white") +
#   theme_minimal()
# p_lisa # print the map
# 
# # Create the local p values
# map$pval <- localmoran(map$vble,nbw)[, 5]
# map$pval
# 
# p_lisa_pval =
#   ggplot(map) +
#   geom_sf(aes(fill= pval), lwd = 0.1) +
#   scale_fill_gradient2(midpoint = 0.05,
#                        name = "p-values",
#                        high = "red", low = "white") +
#   theme_minimal()
# 
# p_lisa_pval # print the map
# 
# cowplot::plot_grid(p_lisa + theme(legend.position = "bottom"),
#           p_lisa_pval + theme(legend.position = "bottom"),
#           ncol = 2)
# 
# index  = map$pval <= 0.05
# p_vble + geom_sf(data = map[index,], fill = NA,
#                  col = "black", lwd = 0.5)
# 
# # Getis-Ord G statistic
# 
# map$gstat <- as.numeric(localG(map$vble, nbw))
# 
# p_geto =
#   ggplot(map) +
#   geom_sf(aes(fill = gstat)) +
#   scale_fill_gradient2(midpoint = 0.5, low = "red", high = "blue",
#                        name = "G Statistic")+
#   theme_minimal()
# p_geto
# 
# # C) Incorporating spatial AC and heterogeneity into ML
# 
# # a) Simple Linear Regression
# 
# formula <- "vble ~ AREA + PERIMETER + HOVAL + INC + OPEN + X + Y"
# # compute model
# model1 <- lm(formula = formula, data = map)
# # view model statistics
# summary(model1)
# 
# # b) Spatial Regress Models accounting for spatial AC
# 
# model2 <- spatialreg::lagsarlm( # lag model
#   formula = formula,
#   data = map,
#   listw = nbw
# )
# 
# model3 <- spatialreg::errorsarlm( # error model
#   formula = formula,
#   data = map,
#   listw = nbw
# )
# 
# jtools::export_summs(model1, model2, model3) # compare to linear model
# 
# spdep::moran.test(model2$residuals, nbw) # model2 and models Moran's I test
# spdep::moran.test(model3$residuals, nbw)
# 
# # c) Geographically Weighted Regress accounting for heterogeneity
# # load packages
# library(SpatialML)
# library(GWmodel)
# map_sp <- map %>% # convert to sp object
#   as_Spatial()
# 
# library(tidyverse)
# library(sf)
# system.file("gpkg/nc.gpkg", package="sf") |>
#   read_sf() -> nc
# 
# nc1 <- nc |> mutate(SID = SID74/BIR74, NWB = NWBIR74/BIR74)
# lm(SID ~ NWB, nc1) |>
#   predict(nc1, interval = "prediction") -> pr
# bind_cols(nc, pr) |> names()

########################################################
# 03-ESDA of spatial dependence and heterogeneity for ML
#######################################################
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# A) calculating the lagged mean and visualizing it
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html

library(sf)
library(spdep)
library(ggplot2)
library(tidyverse)

env_fish <- st_read("data/geo_data/env_fish.shp")
plot(st_geometry(env_fish))

env_fish_xy <- env_fish %>%
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]) 

# creating voronoi polygons and calculating nb and w

library(deldir)
library(sp)
vtess <- deldir(env_fish_xy$x, 
                env_fish_xy$y) # voronoi polygons
class(vtess)
plot(vtess, wlines = "tess", lty=1)

voronoipolygons_sp = function(thiess) {# voronoi polygons to sp
  w = tile.list(thiess)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = sp::Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(
    SP, 
    data=data.frame(
      dummy = seq(length(SP)), 
      row.names=sapply(slot(SP, 'polygons'), 
                       function(x) slot(x, 'ID'))))
}

vtess_sp <- voronoipolygons_sp(vtess)
plot(vtess_sp)

vtess_sf <- st_as_sf(vtess_sp) # converting sp to sf 
plot(vtess_sf$geometry)

st_queen <- function(a, b = a) { # Queen Contiguity Function
  st_relate(a, b, pattern = "F***T****") # DE-9IM pattern
}

queen_sgbp <- st_queen(vtess_sf) # Sparse Geometry Binary Predicate
as_nb_sgbp <- function(x, ...) {# converting sgbp to nb
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

queen_nb <- as_nb_sgbp(queen_sgbp) #  Convert sgbp to nb

queen_w <- spdep::nb2listw(queen_nb, style = "W") # from nb to weights
queen_w$weights[1:3]
summary(queen_w)

# computing the lagged means of fish_abund 
# https://bookdown.org/lexcomber/GEOG3195/spatial-models-spatial-autocorrelation-and-cluster-analysis.html

env_fish_xy$lagged_means_fishabund <- 
  lag.listw(queen_w, env_fish_xy$fish_abund)

p_lagged_mean = 
  ggplot(data = env_fish_xy, 
         aes(x = fish_abund, y = lagged_means_fishabund)) +
  geom_point(shape = 1, alpha = 0.5) +
  geom_hline(yintercept = mean(env_fish_xy$lagged_means_fishabund), lty = 2) +
  geom_vline(xintercept = mean(env_fish_xy$fish_abund), lty = 2) +
  geom_abline() +
  coord_equal()
p_lagged_mean

p_original = # the focused variable
  ggplot(env_fish_xy) +
  geom_sf(aes(fill = fish_abund), size = 2) +
  scale_fill_viridis_c(option = "inferno", name = "original Value") +
  theme_minimal()
p_original 

p_lagged <- ggplot(env_fish_xy) + 
  geom_sf(aes(fill = lagged_means_fishabund), size = 2) +
  scale_fill_viridis_c(option = "inferno", name = "Lagged Value") +
  theme_minimal()
p_lagged

cowplot::plot_grid(p_original, p_lagged)

# B) Global Moran's I and test if statistically significant
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
# https://rpubs.com/laubert/SACtutorial

moran.plot(x = env_fish_xy$fish_abund, listw = queen_w, 
           asp = 1) 
title(main = "Global Moran's Scatter Plot")

# moran.plot(x = env_fish_xy$fish_abund, listw = queen_w,
#            asp = 1,  xlab = "实际观察值", ylab = "空间滞后值")
# library(svglite)
# svglite("data/geo_data/moran.plot.svg")
# dev.off



# statistic test by zscore or range

gI <- moran.test(x = env_fish_xy$fish_abund, listw = queen_w) # for Moran’s I for statistic test

# for the dash lines
gI$estimate
gI
#Calculate Z-score
mI <- gI$estimate[[1]] # global moran's Index
eI <- gI$estimate[[2]] # Expected moran's index
var <- gI$estimate[[3]] # Variance of values
zscore <- (mI-eI)/var**0.5 
# -1.96 <zscore <1.96, no spatial correlation
zscore 

# random if between min-max, else cluster or dispersed
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/ 2) $values))
}

moran.range(queen_w) 


# C) Local Spatial Autocorrelation and test
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html#spatial-autocorrelation
# https://www.kaggle.com/code/jankuper192/spatial-regression

lI <- localmoran(env_fish_xy$fish_abund, 
                       queen_w,
                       zero.policy = TRUE, 
                       na.action = na.omit)

head(lI)

# Extracting Moran’s I and appending to sf 

env_fish_xy$lI <- lI[,1]
env_fish_xy$ElI <- lI[,2]
env_fish_xy$VarlI <- lI[,3]
env_fish_xy$ZlI <- lI[,4] # standard deviate of lI
env_fish_xy$PlI <- lI[,5]

# derive the cluster/outlier types 
significanceLevel <- 0.05
meanVal <- mean(env_fish_xy$fish_abund)

library(magrittr)
lisaRslt <- lI |>  
  tibble::as_tibble() |>
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr()")) |>
  dplyr::mutate(coType = dplyr::case_when(
    `Pr()` > 0.05 ~ "Insignificant",
    `Pr()` <= 0.05 & Ii >= 0 & env_fish_xy$fish_abund >= meanVal ~ "HH",
    `Pr()` <= 0.05 & Ii >= 0 & env_fish_xy$fish_abund < meanVal ~ "LL",
    `Pr()` <= 0.05 & Ii < 0 & env_fish_xy$fish_abund >= meanVal ~ "HL",
    `Pr()` <= 0.05 & Ii < 0 & env_fish_xy$fish_abund < meanVal ~ "LH"
  ))

print(lisaRslt, n =29)

# Now add this coType to the original sf
env_fish_xy$coType <- lisaRslt$coType |> 
  tidyr::replace_na("Insignificant")

# Standardize the variable and its spatial lag
env_fish_xy$z_var <- 
  (env_fish_xy$fish_abund - mean(env_fish_xy$fish_abund)) / sd(env_fish_xy$fish_abund)
env_fish_xy$z_lag <- 
  (env_fish_xy$lagged_means_fishabund - mean(env_fish_xy$lagged_means_fishabund)) / sd(env_fish_xy$lagged_means_fishabund)

# Create a 'quadrant' variable to classify points based on z_var and z_lag
env_fish_xy$quadrant <- with(env_fish_xy, 
                            case_when(
                              z_var >= 0 & z_lag >= 0 ~ "High-High (HH)",
                              z_var < 0 & z_lag >= 0 ~ "Low-High (LH)",
                              z_var >= 0 & z_lag < 0 ~ "High-Low (HL)",
                              z_var < 0 & z_lag < 0 ~ "Low-Low (LL)"
                            )
)

ggplot(env_fish_xy, 
       aes(x = z_var, y = z_lag)) + # Create LISA plot
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_point(aes(color = quadrant), shape = 16, alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c(
    "High-High (HH)" = "#E41A1C",
    "High-Low (HL)" = "#377EB8",
    "Low-High (LH)" = "#4DAF4A",
    "Low-Low (LL)" = "#984EA3"
  )) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  coord_equal() +
  labs(
    title = "LISA Scatter Plot with Quadrants",
    x = "Standardized Value (z-score)",
    y = "Standardized Spatial Lag",
    color = "LISA Type"
  ) +
  theme_minimal()


ggplot(env_fish_xy) +
  geom_sf(aes(color = coType), size = 2) +  # use color, not fill
  scale_color_manual(values = c('red', 'lightgray', 'blue', 'yellow'), 
                     name = 'Clusters & \nOutliers') +
  labs(title = "Shannon Diversity of Fishes") +
  theme_minimal()

########################################################
# 04-spatial autocorrelation and heterogeneity into ML
#######################################################
# A) Simple Linear Regression with X and Y coordinates
# https://rpubs.com/zulfiqar_stat/1131164

# Loading data and packages
library(sf)
library(sp)
library(tidyverse)

spe_env_spa <- st_read("data/geo_data/spe_env_spa.shp")

ggplot(data = spe_env_spa) +
  geom_sf() +  # Plot the spatial data
  theme_minimal()  # Use a minimal theme
              
env_fish_xy <- spe_env_spa %>%
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]
    ) 

env_fish_df <- env_fish_xy |>
  st_drop_geometry() |> # remove geometry
  na.omit() |> # omit NA
  subset(select = -points) |>
  relocate(c("x","y"))

str(env_fish_df)

# removing correlated factors from doubs env
# env_factors <- env_fish_df |>
#   dplyr::select(-fish_abund)
# PerformanceAnalytics::chart.Correlation(env_factors, 
#                                         histogram = TRUE,  
#                                         pch = 19)

cor_matrix <- env_fish_df |>
  subset(select = -spe_abund) |>
  cor(use = "complete.obs", method = "pearson")

threshold <- 0.8
highly_correlated_vars <- 
  caret::findCorrelation(cor_matrix, 
                         cutoff = threshold, 
                         verbose = TRUE)

model_vars <- env_fish_df[, -highly_correlated_vars]
str(model_vars)

library(spatialreg)
formula = spe_abund ~.
model_lm <- lm(formula = formula, data = model_vars)
summary(model_lm) # degree of freedom: m =independent, n-m-1

# B) spatial random forest with only buffer distances

# a) making a prediction grid (SpatialPixelsDataFrame)

env_fish_sp <- env_fish_df
coordinates(env_fish_sp) <- ~ x + y # df -> sp
class(env_fish_sp)
proj4string(env_fish_sp) <- CRS("+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")
plot(env_fish_sp)

res <- 1000 # Set resolution
# Round bounding box to resolution
x_min <- bbox(env_fish_sp)[1,1] %/% res * res
x_max <- (bbox(env_fish_sp)[1,2] + res) %/% res * res
y_min <- bbox(env_fish_sp)[2,1] %/% res * res
y_max <- (bbox(env_fish_sp)[2,2] + res) %/% res * res
str(env_fish_sp@data)

# Creating the grid coordinates
grid_df <- expand.grid(x = seq(x_min, x_max, by = res),
                       y = seq(y_min, y_max, by = res))
str(grid_df)

# converting the grid into a SpatialPixelsDataFrame

grid_sp <- grid_df
grid_sp$dummy <- 1 # Assigning dummy data for SpatialPixelsDataFrame
coordinates(grid_sp) <- ~ x + y # Converting to the SpatialPoints
gridded(grid_sp) <- TRUE # creating SpatialPixelsDataFrame
class(grid_sp)

# b) clipping the grid region of doubs river
# Convert SpatialPixelsDataFrame to sf Object

grid_sf <- st_as_sf(grid_sp)
sf::st_crs(grid_sf) <- 32631

river <- sf::st_read("data/geo_data/doubs_river.shp")
river_utm <- st_transform(river, 32631) 
river_buff <- st_buffer(river_utm, dis = 8000)
plot(st_geometry(river_buff), axes = TRUE)

# clipping the grid limited to the river buffer area
clipped_grid <- st_intersection(grid_sf, river_buff)
plot(st_geometry(clipped_grid), axes = TRUE)
class(clipped_grid)
glimpse(clipped_grid)

# st_write(clipped_grid, 
#          "data/geo_data/clipped_grid.shp",
#          append=FALSE)

# back to the SpatialPixelsDataFrame format

clipped_grid <- st_read("data/geo_data/clipped_grid.shp")

clipped_grid_sp <- as(clipped_grid, "Spatial")
gridded(clipped_grid_sp) <- TRUE
class(clipped_grid_sp) # Check result

plot(clipped_grid_sp, pch = 20, cex = 0.5, # verify
     main = "Prediction Grid")

# saveRDS(clipped_grid_sp, "data/geo_data/clipped_grid_sp.rds")

# c) Target quantiles  and distances to each quantile

(q_abund <- quantile(env_fish_df$spe_abund, 
                     seq(0,1,by=0.0625)))
classes_q_abund <- cut(env_fish_df$spe_abund, 
                                breaks=q_abund, 
                                ordered_result=TRUE, 
                                include.lowest=TRUE)
levels(classes_q_abund)


grid_dist <- landmap::buffer.dist(env_fish_sp["spe_abund"],
                                  clipped_grid_sp, 
                                  classes_q_abund)
head(grid_dist)
dim(grid_dist)

# save(grid_dist, # Save as R data object
#      file = "data/geo_data/grid_dist.RData")

# saveRDS(grid_dist, # Save as RDS file
#         file = "data/geo_data/grid_dist.rds")

load("data/geo_data/grid_dist.RData")
summary(grid_dist)

plot(raster::stack(grid_dist))

# exacting distance to each point
buffer_dists <- over(env_fish_sp, grid_dist)
dim(buffer_dists)
dim(env_fish_sp)
buffer_dists[1,]

env_fish_dist <- cbind(env_fish_sp@data, buffer_dists)
str(env_fish_dist)

# write.csv(env_fish_dist, "data/geo_data/env_fish_dist.csv")

# d) buffer distances-based Spatial random forest 

env_fish_dist <- read.csv("data/geo_data/env_fish_dist.csv", row.names = 1)
str(env_fish_dist)

set.seed(123)
dn <- paste(names(grid_dist), collapse="+")
# str(grid_dist@data)
(fm <- as.formula(paste("spe_abund ~", dn)))

library(randomForest)
set.seed(123)
(model_rf <- randomForest(fm, # rf for predicting obs
                    env_fish_dist, 
                    importance=TRUE, 
                    min.split=5, 
                    mtry=5, 
                    ntree=800))

pred_rf <- predict(model_rf, newdata=env_fish_dist) 
plot(model_rf)
varImpPlot(model_rf, type=1)
plot(env_fish_sp$spe_abund ~ pred_rf, 
     asp=1, 
     pch=20, 
     xlab="Random forest fit", 
     ylab="Actual value", 
     main="fish abundance")
abline(0,1); grid(nx=30,ny=30)
(rmse_rf <- 
    sqrt(sum((pred_rf-env_fish_dist$fish_abund )^2)/length(pred_rf)))

# e) mapping the prediction on the distance grid 

pred_grid <- predict(model_rf, 
                     newdata=grid_dist@data)
str(grid_dist@data)


clipped_grid_sp$model_rf <- pred_grid
str(clipped_grid_sp@data)
breaks <- seq(2, 90, by=.5)
p1 <- spplot(clipped_grid_sp, # SpatialPixelsDataFrame of grid
            zcol="model_rf", 
             main="fish abund", 
             sub="RRF 16 distance buffers", 
             at=breaks)
print(p1)

# D) Spatial random forest on buffer distances and cocovariates

# extracting raster values
topo_char <- terra::rast("data/geo_data/topo_char.tif")

grid_topo <- terra::extract(topo_char, clipped_grid, 
                                    ID=FALSE)
str(grid_topo)
grid_topo_dist <- cbind(grid_topo, grid_dist)
str(grid_topo_dist)

grid_topo_sp <- grid_topo_dist
coordinates(grid_topo_sp) <- ~ x + y # Converting to the SpatialPoints
gridded(grid_topo_sp) <- TRUE # creating SpatialPixelsDataFrame

# spatial random forest on buffer distances and co_vars 

(covars <- paste(intersect(names(env_fish_sp@data), 
                           names(grid_topo_sp)), 
                 collapse="+"))

(fm_covars <- as.formula(paste("spe_abund ~", 
                               dn, 
                               "+", 
                               covars)))

(model_rf_covars <- randomForest(fm_covars, 
                                 env_fish_dist,
                                 importance=TRUE, 
                                 min.split=5, 
                                 mtry=5, 
                                 ntree=1000))

pred_rf_covars <- predict(model_rf_covars, 
                          newdata=env_fish_dist) 
plot(model_rf_covars)
varImpPlot(model_rf_covars, type=1)
plot(env_fish_dist$spe_abund ~ pred_rf_covars, 
     asp=1, 
     pch=20, 
     xlab="Random forest fit with covars", 
     ylab="Actual value", 
     main="fish abundance")
abline(0,1); grid(nx=30,ny=30)
(rmse_rf_covars <- 
    sqrt(sum((pred_rf_covars-env_fish_dist$fish_abund )^2)/length(pred_rf_covars)))

# mapping the prediction on the distance grid 

pred_grid_covars <- predict(model_rf_covars, 
                     newdata=grid_topo_sp@data)

grid_topo_sp$model_rf_covars <- pred_grid_covars
str(grid_topo_sp@data)
breaks <- seq(2, 90, by=.5)
p2 <- spplot(grid_topo_sp, # SpatialPixelsDataFrame of grid
            zcol="model_rf_covars", 
            main="fish abund", 
            sub="RRF 16 distance buffers", 
            at=breaks)
print(p2)
