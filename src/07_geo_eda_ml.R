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
# Date:       2026-04-12
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################
# 01- the vector and raster data
#################################################
## A) load the shapefile of Doubs rive 
library(sf)
doubs_river <- st_read("data/gisdata/doubs_river.shp") # 06_eda
class(doubs_river)
st_crs(doubs_river)
st_crs(doubs_river)$proj4string
st_crs(doubs_river)$epsg

library(ggplot2)
ggplot(data = doubs_river) +
  geom_sf()

# B) the digital elevation model (dem) of the doubs river
# # install.packages("remotes")
# remotes::install_github("rspatial/geodata")

# library(elevatr)
# doubs_elev <- get_elev_raster(doubs_river, z = 10) # z resolution
# doubs_elev
# terra::writeRaster(doubs_elev, "data/gisdata/doubs_dem.tif",
#                    filetype = "GTiff", overwrite = TRUE)

library(terra)
doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
crs(doubs_dem)
terra::crs(doubs_dem, proj=TRUE)

# C) getting the CRS of the sample points using qgis

data(doubs, package = "ade4")
doubs_xy <- doubs$xy
# write.csv(doubs_xy, "data/gisdata/pointcoord_utm.csv")

# // The 1st step  

# importing coordinates_utm.csv to make an image using qgis
# Add Layer -> Add Delimited Text Layer -> 
# Project -> Export -> Export as image (sample_points.png)

# // The 2nd step 
# loading basic OSM map and doubs_river.shp (epsg=4326)  
# as reference for georeferencing the sample_points.png

# AD (sample_points.png) -> geroreferencing for an exact image
# https://www.youtube.com/watch?v=fzz8jw7Qp18 
# or Layer -> # Georeferencer.. -> raster for an exact image
# https://www.youtube.com/watch?v=XV62QEk0Cxg&t=106s

# // The 3rd step
# extracting the long and lat from the referenced image 

# Layer -> Create Layer -> New Shapefile Layer 
# Toggle -> Add Point Feature (sampling) -> save layer edits
# processing -> textbox -> add geometry attributes
# added geometry info -> Export -> save features as
# -> comma seperated value [csv] 
# https://www.youtube.com/watch?v=y8JKVciv26g

# D) visualizing river data and sampling points
par(mfrow = c(1,1))
library(sf)

doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
doubs_dem
terra::plot(doubs_dem) # plot() from different packages

doubs_pts <- read_sf("data/gisdata/sample_points.shp")
doubs_pts
names(doubs_pts)
plot(doubs_pts, add = TRUE, cex =1.8, col = "red")

doubs_river <- read_sf("data/gisdata/doubs_river.shp")
doubs_river
names(doubs_river)
plot(st_geometry(doubs_river), # specifying the geometry column
     add = TRUE, col = "yellow")

dem_df <- as.data.frame(doubs_dem, xy = TRUE, na.rm = TRUE)
colnames(dem_df)
ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = doubs_dem)) +
  geom_sf(data = doubs_pts, aes(geometry = geometry), 
          color = "red", size = 1) +
  geom_sf(data = doubs_river, aes(geometry = geometry), 
          color = "yellow", size = 0.5) +
  theme_minimal()

########################################################
# 02-extracting spatial features as predictors
########################################################
## A) setting a 5-km buffer along rive

library(terra)
library(sf)

doubs_dem <- terra::rast("data/gisdata/doubs_dem.tif")
doubs_river <- sf::st_read("data/gisdata/doubs_river.shp")
doubs_pts <- sf::st_read("data/gisdata/sample_points.shp")

# re-projecting the vector data of river
doubs_river_utm <- st_transform(doubs_river, 32631) 

# creating and visualizing the buffer
doubs_river_buff <- st_buffer(doubs_river_utm, dis = 8000)
names(doubs_river_buff)
plot(st_geometry(doubs_river_buff), axes = TRUE)

# ggplot(doubs_river_buff) +
#   geom_sf(fill = "blue", color = "black")
# st_write(doubs_river_buff,
#          "data/gisdata/doubs_river_buff.geojson")

# B) Clipping or intersecting dem covered by the river buffer
# re_projecting raster data

terra::crs(doubs_dem, proj = TRUE) # get CRS
utm_crs <- "EPSG:32631" # set CRS
doubs_dem_utm <- terra::project(doubs_dem,utm_crs) # for sf using st_transform()
crs(doubs_dem_utm, proj = TRUE)# check crs

# Clipping or intersecting dem by the doubs river

doubs_dem_utm_cropped = terra::crop(doubs_dem_utm,
                             doubs_river_buff)
plot(doubs_dem_utm_cropped)
doubs_dem_utm_masked = terra::mask(doubs_dem_utm_cropped,
                            doubs_river_buff)
plot(doubs_dem_utm_masked)
# writeRaster(doubs_dem_utm_masked, "data/gisdata/doubs_dem_masked.tif")

# C) extracting raster values of points as predictors
# https://r.geocompx.org/eco

library(qgisprocess)
qgis_configure()
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title, algorithm) |>
  head(2)

# catchment slope and catchment area
topo = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagawetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1, 
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)
str(topo)

topo_slo_area <- c(qgis_as_terra(topo$AREA), 
                   qgis_as_terra(topo$SLOPE))
names(topo_slo_area) <- c("carea", "cslope")

topo_dem_slo_area <- c(doubs_dem_utm_masked, topo_slo_area)

writeRaster(topo_dem_slo_area,
 "data/gisdata/topo_dem_slo_area.tif",
 overwrite=FALSE)

# re-projecting points to utm

doubs_pts_utm <- sf::st_transform(doubs_pts, utm_crs)

# st_write(doubs_pts_utm,"data/gisdata/doubs_pts_utm.geojson")

# extracting raster values
topo_dem_slo_area <- rast("data/gisdata/topo_dem_slo_area.tif")
doubs_pts_utm <- st_read("data/gisdata/doubs_pts_utm.geojson")
  
doubs_pts_topo <- terra::extract(topo_dem_slo_area, 
                                 doubs_pts_utm, ID=FALSE)
glimpse(doubs_pts_topo)

# aggregating topo and acquatic chemical env

doubs_pts_env = cbind(doubs_pts_utm, doubs_pts_topo, doubs$env) # convert dataframe to SpatRaster

# st_write(doubs_pts_env, "data/gisdata/doubs_pts_env.geojson",
# append=TRUE)

# the final spe-env data with spatial attributes
fish <- doubs$fish
fish$abund <- rowSums(fish) 
fish_clean <- fish[fish$abund != 0, ]

doubs_pts_env_clean <- 
  doubs_pts_env[fish$abund != 0, ]

env_fish_xy <- doubs_pts_env_clean %>%
  mutate(fish_abund = fish_clean$abund) %>%
  relocate(fish_abund, .before = geometry) %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  relocate(x, y, .before = doubs_dem)

# st_write(env_fish_xy, "data/gisdata/env_fish_xy.gpkg") # gpkg = spatial sqite

########################################################
# 03- the Exploratory Spatial Data analysis (ESDA)
########################################################
# The key step of building a machine learning model is to
# do EDA analysis for detecting the correlation among the 
# predictors, as well as them with response. Here the step
# will be skipped.

# # 1) the EDA analysis on the table-data part
# 
# library(sf)
# library(ggplot2)
# library(tidyverse)
# 
# env_fish_xy <- st_read("data/gisdata/env_fish_xy.gpkg")
# ggplot(env_fish_xy) +
#   geom_sf()
# 
# # A) checking the target
#  
# summary(env_fish_xy$abund)
# 
# hist(env_fish_xy$abund, breaks = 10, main = "Abund distribution")
# plot(density(env_fish_xy$abund), main = "Density of abund")
# boxplot(env_fish_xy$abund, main = "Boxplot of abund")
# shapiro.test(env_fish_xy$abund)
# 
# env_fish_xy_logabund <- env_fish_xy %>%
#   mutate(logabund = log1p(abund)) %>%
#   select(-abund)
# 
# # B) checking the predictors
# 
# library(dplyr)
# env_fish_xy_df <- sf::st_drop_geometry(env_fish_xy)
# vars <- env_fish_xy_df[, -c(1:3, 18)]
# boxplot(vars)
# 
# replace_outliers <- function(x) {
#   if (!is.numeric(x)) return(x)
#   
#   Q1 <- quantile(x, 0.25, na.rm = TRUE)
#   Q3 <- quantile(x, 0.75, na.rm = TRUE)
#   IQR <- Q3 - Q1
#   lower <- Q1 - 1.5 * IQR
#   upper <- Q3 + 1.5 * IQR
#   x[x < lower | x > upper] <- NA
#   return(x)
# }
# 
# vars_outliers_NA <- vars %>%
#   mutate(across(everything(), replace_outliers))
# vars_outliers_NA
# 
# vars_inliers <- vars_outliers_NA %>%
#   mutate(across(where(is.numeric),
#                 ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
# vars_inliers 
# 
# library(car)
# 
# model <- lm(env_fish_xy_df$abund ~ ., data = vars_inliers)
# car::vif(model)
# 
# library(usdm)
# v <- vifstep(vars_inliers)
# v
# env_fish_xy_modified <- env_fish_xy %>%
#   select(-c(dfs, alt))
# 
# # st_write(env_fish_xy_modified,
# #          "data/gisdata/env_fish_xy_modified.gpkg",
# #          append=FALSE)

# 2) the spatial dependence and heterogeneity

# A) calculating the lagged mean and visualizing it
# https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-1.html
# points -> voronoi polygons -> nb -> w

library(sf)
library(spdep)
library(ggplot2)
library(tidyverse)

env_fish_xy <- st_read("data/gisdata/env_fish_xy.gpkg")
plot(st_geometry(env_fish_xy))

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

# B) Global Moran's I and test if statistically significant
# http://www.geo.hunter.cuny.edu/~ssun/R-Spatial/spregression.html
# https://rpubs.com/laubert/SACtutorial

moran.plot(x = env_fish_xy$fish_abund, listw = queen_w, 
           asp = 1) 
title(main = "Global Moran's Scatter Plot")

# statistic test by zscore or range

gI <- moran.test(x = env_fish_xy$fish_abund, 
                 listw = queen_w) # for Moran’s I for statistic test
gI
# for the dash lines
gI$estimate

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
  labs(title = "Abundance of Fishes") +
  theme_minimal()

########################################################
# 04-building ML model with spatial autocorrelation 
#######################################################
# A) Simple Linear Regression with X and Y coordinates
# https://rpubs.com/zulfiqar_stat/1131164

# Loading data and packages
library(sf)
library(sp)
library(tidyverse)

env_fish_xy <- st_read("data/gisdata/env_fish_xy.gpkg")

ggplot(data = env_fish_xy) +
  geom_sf() +  # Plot the spatial data
  theme_minimal()  # Use a minimal theme
              
env_fish_xy_df <- env_fish_xy %>%
  st_drop_geometry() %>%
  subset(select = -id)

str(env_fish_xy_df)

# removing correlated env factors from env_fish_xy_df

cor_matrix <- env_fish_xy_df %>%
  subset(select = -fish_abund) %>%
  cor(use = "complete.obs", 
      method = "pearson")
threshold <- 0.9
highly_correlated_vars <- 
  caret::findCorrelation(cor_matrix, 
                         cutoff = threshold, 
                         verbose = TRUE)

model_vars <- env_fish_xy_df[, -highly_correlated_vars]
str(model_vars)

library(randomForest)
model_rf1 <- randomForest(fish_abund ~., data = model_vars)
print(model_rf1) 

# B) spatial random forest with the buffer distances

# a) creating a prediction grid (sf)

env_fish_xy <- st_read("data/gisdata/env_fish_xy.gpkg")
names(env_fish_xy)

par(mfrow = c(1,1))
res <- 1000  # 1000 m × 1000 m
grid <- st_make_grid(
  env_fish_xy,  
  cellsize = res,
  what = "centers"
)
class(grid)

grid_sf <- st_sf(geometry = grid) # equal to data.frame + geometry

plot(grid_sf)
plot(grid_sf)

# b) clipping the grid region of doubs river
doubs_river <- st_read("data/gisdata/doubs_river.shp")
doubs_river_utm <- st_transform(doubs_river, 
                                st_crs(env_fish_xy))
doubs_river_buff <- st_buffer(doubs_river_utm, 
                              dist = 8000)
plot(st_geometry(doubs_river_buff))
clipped_grid <- st_filter(grid_sf, doubs_river_buff)
plot(st_geometry(clipped_grid))

# c) each sample points distance to each quantile

# fish_abund divided to 16 quantiles
env_fish_xy_df <- env_fish_xy %>%
  st_drop_geometry() 

q_abund <- quantile(env_fish_xy_df$fish_abund, 
                    seq(0, 1, by = 0.0625), na.rm = TRUE)

classes_q_abund <- cut(env_fish_xy_df$fish_abund,
                       breaks = q_abund,
                       include.lowest = TRUE,
                       ordered_result = TRUE)

env_fish_xy_df$q_class <- as.character(classes_q_abund)
env_fish_xy_df

# calculating the min distance of a grid to each quantile
points_sf <- st_as_sf(env_fish_xy_df, # convert to sf 
                      coords = c("x", "y"),
                      crs = st_crs(clipped_grid))

grid_dist_list <- list()
for (cls in unique(points_sf$q_class)) {
  pts_sub <- points_sf[points_sf$q_class == cls, ]
  dmat <- st_distance(clipped_grid, pts_sub)
  dmat <- as.matrix(dmat)
  min_dist <- apply(dmat, 1, min)
  grid_dist_list[[cls]] <- min_dist
}

grid_dist_df <- as.data.frame(grid_dist_list)
colnames(grid_dist_df) <- paste0("dist_Q", 
                                 seq_len(ncol(grid_dist_df)))
clipped_grid <- cbind(clipped_grid, grid_dist_df)
str(grid_dist_df)
head(grid_dist_df)

# exacting the buffer distance for each sample point

idx <- st_nearest_feature(points_sf, clipped_grid)
buffer_dists <- clipped_grid[idx, ] %>% 
  st_drop_geometry()

dim(buffer_dists)
head(buffer_dists)

env_fish_dist <- cbind(
  st_drop_geometry(points_sf),
  buffer_dists
)

head(env_fish_dist)

# write.csv(env_fish_dist, "data/gisdata/env_fish_dist.csv")

# d. buffer distances-based Spatial random forest 

set.seed(123)
dn <- paste(names(grid_dist_df), collapse="+")
(fm <- as.formula(paste("fish_abund ~", dn)))
(model_rf2 <- randomForest(fm, # rf for predicting obs
                          env_fish_dist, 
                          importance=TRUE, 
                          min.split=5, 
                          mtry=5, 
                          ntree=800))
print(model_rf2)
pred_rf <- predict(model_rf2, newdata=env_fish_dist) 

varImpPlot(model_rf2, type=1)

plot(env_fish_dist$fish_abund ~ pred_rf, 
     asp=1, 
     pch=20, 
     xlab="Random forest fit", 
     ylab="Actual value", 
     main="fish abundance")
abline(0,1); grid(nx=30,ny=30)
rmse_rf <- sqrt(mean((pred_rf - env_fish_dist$fish_abund)^2))
rmse_rf
r2_rf <- cor(pred_rf, env_fish_dist$fish_abund)^2
r2_rf

# e. mapping the prediction on the distance grid 

pred_grid <- predict(model_rf2, 
                     newdata=grid_dist_df)

clipped_grid$model_rf2 <- pred_grid
str(clipped_grid)

library(ggplot2)
ggplot(clipped_grid) +
  geom_sf(aes(color = model_rf2)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(
    title = "Predicted Fish Abundance",
    subtitle = "Random Forest with Distance-to-Quantile Features",
    color = "Abundance"
  )

# f. spatial random forest on buffer distances and co_vars 
dn_vec <- names(grid_dist_df)
covars_vec <- setdiff(names(doubs_env),
                      c("fish_abund", "x", "y", "id"))
all_vars <- c(dn_vec, covars_vec)

fm_covars <- as.formula(
  paste("fish_abund ~", paste(all_vars, collapse = "+"))
)

fm_covars

set.seed(123)
model_rf3 <- randomForest(
  fm_covars,
  data = env_fish_dist,
  importance = TRUE,
  ntree = 1000
)

print(model_rf3)

# mapping the prediction on the distance grid 
pred_rf3 <- predict(model_rf3, newdata = env_fish_dist)
plot(env_fish_dist$fish_abund ~ pred_rf3,
     asp = 1,
     pch = 20,
     col = "steelblue",
     xlab = "Predicted (RF + covars)",
     ylab = "Observed",
     main = "Fish abundance")

abline(0, 1, col = "red", lwd = 2)
grid()

rmse_rf3 <- sqrt(mean((pred_rf3 - env_fish_dist$fish_abund)^2))
rmse_rf3

r2_rf3 <- cor(pred_rf3, env_fish_dist$fish_abund)^2
r2_rf3
