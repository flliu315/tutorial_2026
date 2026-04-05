# --------------------------------------------
# Script Name: Exploratory data analysis on doubs
# Purpose: This section is to show how to do EDA for 
#          finding eco-patterns. The data analysis is 
#          to take the dataset of doubs as an example,
#          and do EDA on the data of a community.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-04-05
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

##################################################
# 01-Extracting doubs river data and Creating a map
##################################################

# A) using qgis to download the data of doubs river 
# https://www.youtube.com/watch?v=gahG3OAdZQs
# // installing plugins: quickmapservices and quickOSM
# quickservice -> metasearch -> add default (basic map)

# // filling key-value in QUICKOSM for downloading 
# quickOSM -> waterway and river -> doubs →runs

# // obtaining Le Doubs river by selection features
# open attribution table -> selection by Expression 
# "name" LIKE '%Le Doubs%'

# // copying and pasting features for saving the river
# Edit -> Copy -> paste features as

# //saving the doubs river data to postgresql
# https://www.youtube.com/watch?v=H9o0wme0nuk

# B) using R
# asking chatGPT for downloading from OSM

"based on openstreetmap data, write R code to find Le Doubs
in France-Switzerland, and use mapview to visulize it on a map"

# Installing and loading necessary packages

library(osmdata) # downloading data from OSM
library(mapview) # interactively visualizing spatial data

# getting the bounding box for Le Doubs river

bbox <- c(left = 5.5, bottom = 46.5, right = 7.5, top = 48)

# Query OSM for  "Le Doubs" waterways in a bounding box
doubs_query <- opq(bbox = bbox) |>
  add_osm_feature(key = "waterway", value = "river") |>
  add_osm_feature(key = "name", value = "Le Doubs") |>
  osmdata_sf() 

doubs_query

# Visualizing Le Doubs on a map
mapview(doubs_query$osm_lines)

# Converting LINE to MULTILINES and binding to a sf object
# https://ourcodingclub.github.io/tutorials/spatial-vector-sf/
# 
library(dplyr)

?bind_rows
?st_cast
library(sf)
river_sf <- dplyr::bind_rows(
  sf::st_cast(doubs_query$osm_lines, 
              "MULTILINESTRING"), # from lines to MULTILINES
  doubs_query$osm_multilines) |>
  select(name, osm_id, role)

head(river_sf)

# plot doubs_sf
plot(river_sf)

unique(river_sf$role)

# Filtering out unneeded shapes to make sure shapes are valid
river_sf_clean <-
  river_sf |>
  filter(is.na(role) == FALSE) |> # remove role NAs
  rename(doubs_type = role) |> # rename role to doubs_type
  st_make_valid()

unique(river_sf_clean$doubs_type)

st_write(river_sf_clean, "data/gisdata/DOUBS_RIVER.shp")

# st_write(river_sf_clean,
#          dsn = "data/DOUBS_RIVER.gpkg", # file path
#          layer="doubstype", # layer name
#          layer_options = c(paste0("DESCRIPTION=Contains multilines for doubs, ",
#                                   "Copyright OpenStreetMap constibutors. ODbL ",
#                                   "https://www.openstreetmap.org/copyright")),
#          # add layer description
#          delete_dsn = TRUE
# )

# DOUBS_RIVER <- st_read(dsn = "data/DOUBS_RIVER.gpkg",
#                       layer="doubstype")

# DOUBS_RIVER <- st_read("data/DOUBS_RIVER.shp")

# library(ggplot2)
# DOUBS_RIVER <- ggplot(DOUBS_RIVER) +
#   geom_sf(color="blue")
# DOUBS_RIVER
# 
# ggsave("data/DOUBS_RIVER.png", DOUBS_RIVER, 
#        width = 8, height = 6.5)

##################################################
# 02-loading the Doubs data and cleaning them
##################################################
# A) loading the Doubs data from postgresql

# library(DBI) # helps connecting R to database
# library(RPostgreSQL) # provides an interface with SQLite

doubsdata <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = 'doubs',
                          host = 'localhost',
                          port = 5432,
                          user = 'doubs',
                          password = 'xxxx')
library(DBI)
dbListTables(doubsdata)
dbListFields(doubsdata, "doubs_env") # List fields of the table

# ?dbReadTable
doubs_spe <- dbReadTable(conn = doubsdata,"doubs_spe")

doubs_env <- dbReadTable(conn = doubsdata,"doubs_env")
doubs_spa <- dbReadTable(conn = doubsdata,"doubs_spa")
dbDisconnect(doubsdata)
dbGetInfo(doubsdata)

# B) pre-Processing of the Doubs datasets

names(doubs_spe) # Names of objects
dim(doubs_spe) # dimensions
str(doubs_spe) # structure of objects
summary(doubs_spe) # summary statistics
head(doubs_spe) # first 6 rows


row_sums <- rowSums(doubs_spe)
which(row_sums == 0)
spe_clean <- doubs_spe[-8,] # remove site with no fish

names(doubs_env) # Names of objects
dim(doubs_env) # dimensions
str(doubs_env) # structure of objects
summary(doubs_env) # summary statistics
head(doubs_env) # first 6 rows
env_clean <- doubs_env[-8,] # remove site with no fish

spa_clean <- doubs_spa[-8,] # remove site with no fish

# C) Exploring the fish community dataset

library(vegan) 

dim(spe_clean) # Dimensions of the data frame (rows, columns)
colnames(spe_clean) # Column labels (descriptors = species)
rownames(spe_clean) # Row labels (objects = sites)
summary(spe_clean) # Descriptive statistics for columns

# frequency distribution

range(spe_clean) 
(ab <- table(unlist(spe_clean))) # df→vector→table→output
barplot(ab,
        las = 1, # set labels to horizontal
        xlab = "Abundance class",
        ylab = "Frequency",
        col = gray(5 : 0 / 5)
)

sum(spe_clean == 0) / (nrow(spe_clean) * ncol(spe_clean))

## Visualizing fish distribution

plot(spa_clean, # using plot(…) function with lines(…), 
     # points(…), text(…), polygon(…) etc. 
     # to build sophisticated graphs.
     asp = -1,
     type = "p", # a plot point data
     main = "Sampling sites",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)"
)

lines(spa_clean, col = "light blue") # Add a line/labels/text 
text(spa_clean, row.names(spa_clean), cex = 1.0, col = "red")

text(40, 20, "Upstream", cex = 1.2, col = "blue")
text(15, 120, "Downstream", cex = 1.2, col = "blue")

par(mfrow=c(2,2))
# Plot four species
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"
plot(spa_clean, asp=1, col="brown", cex=spe_clean$LOC, 
     main="Stone loach", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$CHA, 
     main="European bullhead", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$BAR, 
     main="Barbel", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, col="brown", cex=spe_clean$BCO, 
     main="Common bream", xlab=xl, ylab=yl)
lines(spa_clean, col="light blue", lwd=2)
par(mfrow=c(1,1))

# visualizing Environmental Variables

par(mfrow=c(1,4))
plot(spa_clean, asp=1, main="Altitude", pch=21, col="white",
     bg="red", cex=5*env_clean$alt/max(env_clean$alt), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Discharge", pch=21, col="white",
     bg="blue", cex=5*env_clean$deb/max(env_clean$deb), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Oxygen", pch=21, col="white",
     bg="green3", cex=5*env_clean$oxy/max(env_clean$oxy), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Nitrate", pch=21, col="white",
     bg="brown", cex=5*env_clean$nit/max(env_clean$nit), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)

############################################
# 03- Q- and R-mode analysis on fish and env
###########################################
# A) Data transformation and standardizing 
# Transforming the fish community data

library(vegan)
spe_pa <- decostand(spe_clean, method = "pa")
spe_hel <- decostand(spe_clean, method = "hellinger") 
spe_log <- decostand(spe_clean,method = "log")

# standardizing env variables (z-scores) 

env_z <- decostand(env_clean, "standardize")
apply(env_z, 2, mean) # means = 0
apply(env_z, 2, sd) # standard deviations = 1

# B) Q-mode (dissimilarity among sites or rows)

source("src/coldiss.R")
source("src/panelutils.R") # https://figshare.com/s/65782d019a0e2af49cf6?file=4975841

# a) based on species abundance data

# bray-curtis dissimilarity with raw data
spe_db <- vegan::vegdist(spe_clean)	# Bray-Curtis as default
# same to vegdist(spe_clean, "bray")	
spe_db
coldiss(spe_db,byrank = FALSE,diag = TRUE)

# chord dissimilarity using norm data
spe_norm <- decostand(spe_clean, "nor") # normalizing each row
spe_dc <- stats::dist(spe_norm)

# Hellinger dissimilarity 
spe_hel <- decostand(spe_clean, "hel")
spe_dh <- stats::dist(spe_hel)
# spe_dhel <- vegdist(spe_hel, method="euclidean") 
coldiss(spe_dh,byrank = FALSE,diag = TRUE)

# b) based on presence-absence data

# Jaccard matrix
spe_dj <- dist(spe_clean, "binary") 
# same to vegdist(spe_clean, "jac", binary=TRUE)
spe_dj 
coldiss(spe_dj, byrank = FALSE, diag = TRUE)

# Sorensen matrix
spe_ds <- vegdist(spe_clean, "bray", binary=TRUE) 
spe_ds

# C) R-mode (relationships among evns or columns)
# https://www.rpubs.com/dvallslanaquera/pca
PerformanceAnalytics::chart.Correlation(env_clean, 
                                        histogram = TRUE, 
                                        pch = 19)

###################################################
# 04-Clustering and ordination
###################################################
# A) Clustering based on transformed data of species
library(vegan)
spe_dhel <- vegdist(spe_hel, method="euclidean") 

par(mfrow=c(1,1))
spe_dhel_single<-hclust(spe_dhel, method="single")
plot(spe_dhel_single, main="Single linkage clustering", 
     hang =-1)

spe_dhel_complete <- hclust(spe_dhel, method = "complete")
plot(spe_dhel_complete, main="Complete linkage clustering", 
     hang=-1)

# B) unconstrained ordination---PCA
# https://www.rpubs.com/dvallslanaquera/pca
# The assumptions to be considered here are:
# Independence between samples; No outliers nor NA values;
# Multivariant normality; Linear relation between samples.

# a) for species data
spe_hel <- decostand(spe_clean, "hellinger")
dim(spe_hel)
vegan::decorana(spe_hel) # model selection

spe_pca <- rda(spe_hel) # run pca with rda() 
summary(spe_pca, scaling = 2) 
summary(spe_pca, scaling = 1) 

spe_ev <- spe_pca$CA$eig # # Selecting PC
spe_ev[spe_ev>mean(spe_ev)]
n <- length(spe_ev)
barplot(spe_ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(spe_ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

biplot(spe_pca, scaling =1, main="PCA scaling=1") 
biplot(spe_pca, scaling =2, main="PCA scaling=2") 

# for env data to compare sites or how correlated
library(RVAideMemoire)
mshapiro.test(env_clean) # normality test
vegan::decorana(env_clean) # model selection

env_pca <- rda(env_clean, # run PCA, same to rda(env_z)
               scale = TRUE) # calls a standardization

summary(env_pca, scaling = 2) # By default scaling 2
summary(env_pca, scaling = 1)

(env_ev <- env_pca$CA$eig) # Selecting PC
env_ev[env_ev > mean(env_ev)] 
env_n <- length(env_ev)
barplot(env_ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(env_ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

plot(env_pca) # explainable variance of the axis
biplot(env_pca, scaling =1, main="scaling=1: object similarity") 
biplot(env_pca, scaling =2, main="scaling=2: importance & corr") 

# C) constrained ordination---RDA
 
spe_hel <- decostand(spe_clean, "hellinger")
env_z <- decostand(env_clean, "standardize") #  centers and scales variables

env_spe_rda <- rda(spe_hel ~ ., 
                   data = env_z) # Scaling = 2 (by default)
vif.cca(env_spe_rda)
summary(env_spe_rda)

env_spe_ev <- env_spe_rda$CA$eig # Selecting PC
env_spe_ev[env_spe_ev>mean(env_spe_ev)]

n <- length(env_spe_ev)
barplot(env_spe_ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(env_spe_ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

anova(env_spe_rda, step = 1000) # Goodness of fit
anova(env_spe_rda, by = "axis", step = 1000)
anova(env_spe_rda, by = "margin", step = 1000)

RsquareAdj(env_spe_rda)

# Triplot: sites, response and explanatory variables

plot(env_spe_rda, scaling=1, main="scaling 1")# Scaling 1
# env_spe_sc <- scores(env_spe_rda, choices=1:2, scaling=1, display="sp")
# arrows(0,0,env_spe_sc[,1], env_spe_sc[,2], length=0, lty=1, col='red')

plot(env_spe_rda, main="scaling 2")# Scaling 2
# env_spe2_sc <- scores(env_spe_rda, choices=1:2, display="sp")  
# arrows(0,0,env_spe2_sc[,1], env_spe2_sc[,2], length=0, lty=1, col='red')

vif.cca(env_spe_rda) # Selecting explanatory variables
env_spe_rda_all <- rda(spe_hel ~., data=env_z) # RDA with all explanatory variables  
RsquareAdj(env_spe_rda_all)$adj.r.squared

(step.forward1 <- # selecting explanatory variables
  ordistep(rda(spe_hel ~ 1, data = env_z), # lower model limit (simple)
                         trace = 0,
                         scope = formula(env_spe_rda_all), # upper model limit (full)
                         direction = "forward", 
                         pstep = 1000))


(step.forward2 <- 
    ordiR2step(rda(spe_hel ~ 1, data = env_z), 
               trace = 0,
               scope = formula(env_spe_rda_all), 
               direction = "forward",
               pstep = 1000))

RsquareAdj(rda(spe_hel ~ das, data = env_z))$adj.r.squared
RsquareAdj(rda(spe_hel ~ das + oxy, data = env_z))$adj.r.squared
RsquareAdj(rda(spe_hel ~ das + oxy + dbo, data = env_z))$adj.r.squared

env_spe_rda_pars <- rda(spe_hel ~ # Parsimonious model
                          alt + oxy + dbo, env_z) 
anova.cca(env_spe_rda_pars, step=1000)
anova(env_spe_rda_pars, step = 1000, by = "axis")
anova(env_spe_rda_pars, step = 1000, by = "margin")

vif.cca(env_spe_rda_all) # VIF comparison
vif.cca(env_spe_rda_pars)

par(mfrow = c(1, 2)) # plot final triplot

plot(env_spe_rda_pars, scaling = 1, display = c("sp", "lc", "cn"), 
     main = "Scaling 1: object similarity") # Scaling 1
# env_spe3_sc <- scores(env_spe_rda_pars, choices = 1:2, scaling = 1, display = "sp")
# arrows(0, 0, env_spe3_sc[, 1], env_spe3_sc[, 2], length = 0, lty = 1, col = "red")

plot(env_spe_rda_pars, 
     display = c("sp", "lc", "cn"), # sp(species), lc(location),cn(Constraints)
     main = "Scaling 2:  species-environment") # Scaling 2
# env_spe4_sc <- scores(env_spe_rda_pars, choices = 1:2, display = "sp")
# arrows(0, 0, env_spe4_sc[,1], env_spe4_sc[,2], length = 0, lty = 1, col = "red")