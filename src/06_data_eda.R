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
# 01-getting doubs OMS data for a map
##################################################

# A) using qgis 
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

# //saving the doubs OSM data to postgresql
# https://www.youtube.com/watch?v=H9o0wme0nuk

# B) using R codes
# asking chatGPT for downloading river data from OSM

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

# Visualizing Le Doubs (sf) on a map
mapview(doubs_query$osm_lines)

# Converting LINE to MULTILINES and binding to a sf object
# https://ourcodingclub.github.io/tutorials/spatial-vector-sf/
# 
# ?bind_rows
# ?st_cast
library(sf)
river_sf <- dplyr::bind_rows(
  sf::st_cast(doubs_query$osm_lines, "MULTILINESTRING"), # from lines to MULTILINES
  doubs_query$osm_multilines) |>
  select(name, osm_id, role)

class(river_sf)
head(river_sf)

# plot doubs_sf
plot(river_sf)

# unique(river_sf$role)

# Filtering out unneeded shapes to make sure shapes are valid
river_sf_clean <-
  river_sf |>
  filter(is.na(role) == FALSE) |> # remove role NAs
  rename(doubs_type = role) |> # rename role to doubs_type
  st_make_valid()

unique(river_sf_clean$doubs_type)

st_write(river_sf_clean, "data/gisdata/DOUBS_river.shp")
DOUBS_river <- st_read("data/gisdata/DOUBS_river.shp")

st_write(river_sf_clean, "data/gisdata/DOUBS_river.gpkg")
DOUBS_river <- st_read("data/gisdata/DOUBS_river.gpkg")

library(ggplot2)
DOUBS_river <- ggplot(DOUBS_river) +
  geom_sf(color="blue")
DOUBS_river

ggsave("data/gisdata/DOUBS_river.png", 
       plot = DOUBS_river,
       width = 8, height = 6.5)

##################################################
# 02-loading fish-env data and pre-Processing them
##################################################
# 1) loading the Doubs data from postgresql

# library(DBI) # helps connecting R to database
# library(RPostgreSQL) # provides an interface with SQLite
library(DBI)
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                          dbname = 'doubs',
                          host = 'localhost',
                          port = 5432,
                          user = 'doubs',
                          password = 'xxxx')

dbListTables(con)
dbListFields(con, "doubs_env") # List fields of doubs_env table

# ?dbReadTable
doubs_spe <- dbReadTable(conn = con,"doubs_spe")
doubs_env <- dbReadTable(conn = con,"doubs_env")
doubs_spa <- dbReadTable(conn = con,"doubs_spa")
dbDisconnect(con)
dbGetInfo(con)

# 2) pre-Processing of the fishes and env data

# A) about the data of fish community

# a. deleting the rows without fishes

str(doubs_spe) # structure of objects
head(doubs_spe) # first 6 rows
summary(doubs_spe) # summary statistics
dim(doubs_spe) # dimensions
names(doubs_spe) # Names of objects

row_sums <- rowSums(doubs_spe)
which(row_sums == 0)
spe_clean <- doubs_spe[-8,] # remove the sites with no fish

# spe_clean <- doubs_spe %>%
#   filter(rowSums(.) != 0)

str(doubs_env) # structure of objects
summary(doubs_env) # summary statistics
head(doubs_env) # first 6 rows
dim(doubs_env) # dimensions
names(doubs_env) # Names of objects

env_clean <- doubs_env[-8,] # remove site with no fish
spa_clean <- doubs_spa[-8,] # remove site with no fish

# b. frequency distribution and transformation

library(vegan) 

range(spe_clean) 
(ab <- table(unlist(spe_clean))) # df→vector→table→output
barplot(ab,
        las = 1, # set labels to horizontal
        xlab = "Abundance class",
        ylab = "Frequency",
        col = gray(5 : 0 / 5)
)

sum(spe_clean == 0) / (nrow(spe_clean) * ncol(spe_clean))

# Transforming the fish community data

# spe_pa <- decostand(spe_clean, method = "pa")
spe_hel <- decostand(spe_clean, method = "hellinger") 
spe_log <- decostand(spe_clean,method = "log")

# c. the amounts of rare or Dominant species 

colSums(spe_clean > 0) # the number of sites by each species (abund)
rowSums(spe_clean > 0) # species richness

abund <- colSums(spe_clean)
abund_sorted <- sort(abund, decreasing = TRUE)
plot(abund_sorted,
     type = "b",
     log = "y",
     main = "Rank-Abundance Curve",
     xlab = "Species rank",
     ylab = "Abundance (log scale)")

# df2 <- data.frame(
#   rank = 1:length(abund_sorted),
#   abundance = abund_sorted
# )
# 
# ggplot(df2, aes(x = rank, y = abundance)) +
#   geom_line() +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(title = "Rank-Abundance Curve")

apply(spe_clean, 2, max) # check the max value for each column

# d. the double-zero problem
# avoiding the dist() from the stats package
library(vegan)
fish_comm <- spe_clean[, colSums(spe_clean > 0) >= 3]
dim(fish_comm)
spe_hel <- decostand(fish_comm, method = "hellinger") # Hellinger
dist_mat <- vegdist(spe_hel, method = "euclidean") # euclidean
dist_mat <- vegdist(spe_hel, method = "bray") # Bray-Curtis

# B) detecting and replacing outliers in the env columns

# # detecting outlier for each variable
# dfs <- env_clean$dfs
# 
# Q1 <- quantile(dfs, 0.25) 
# Q3 <- quantile(dfs, 0.75) 
# IQR <- Q3 - Q1
# # Lower and Upper Bounds 
# lower_bound <- Q1 - 1.5 * IQR
# upper_bound <- Q3 + 1.5 * IQR 
# outliers <- dfs[dfs < lower_bound | dfs > upper_bound] 
# print(outliers)
# 
# par(mfrow= c(1, 2))
# boxplot(dfs, ylab = "dfs")
# boxplot(dfs, ylab = "dfs", horizontal = TRUE)
# par(mfrow= c(1, 1))

# a. detecting all columns of a dataframe and replacing with NA
library(dplyr)
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

outlier <- env_clean %>%
  mutate(across(where(is.numeric), detect_outliers))
outlier

boxplot(env_clean, horizontal = TRUE, 
        main = "Boxplot for all variables")

library(dplyr)
replace_outliers <- function(x) {
  if (!is.numeric(x)) return(x)
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x[x < lower | x > upper] <- NA
  return(x)
}

env_cleanNA <- env_clean %>%
  mutate(across(everything(), replace_outliers))
env_cleanNA

# b. filled NA using mean for each column

env_filled <- env_cleanNA %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

env_filled

# 3) pre-exploration of the relationship begtween fishes and env
# A) the distriution of sampling locations

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

# B) the distribution of indicative fishes
par(mfrow=c(2,2)) # Plot four species
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

# the distribution of typic Environmental Variables

par(mfrow=c(1,4))
plot(spa_clean, asp=1, main="Altitude", pch=21, col="white",
     bg="red", cex=5*env_filled$alt/max(env_filled$alt), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Discharge", pch=21, col="white",
     bg="blue", cex=5*env_filled$flo/max(env_filled$flo), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Oxygen", pch=21, col="white",
     bg="green3", cex=5*env_filled$oxy/max(env_filled$oxy), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
plot(spa_clean, asp=1, main="Nitrate", pch=21, col="white",
     bg="brown", cex=5*env_filled$nit/max(env_filled$nit), xlab="x", ylab="y")
lines(spa_clean, col="light blue", lwd=2)
par(mfrow=c(1,1))

############################################
# 03- Q- and R-mode analysis on fish and env
###########################################
# 1) for the env data

# A) R-mode (relationships among variables or columns)
# https://www.rpubs.com/dvallslanaquera/pca

# standardizing env variables (z-score) 
library(vegan)
env_z <- decostand(env_filled, "standardize")
# apply(env_z, 2, mean) # means = 0
# apply(env_z, 2, sd) # standard deviations = 1
# env_z equal to env_scaled <- scale(env_filled)
# apply(env_scaled, 2, mean) # means = 0
# apply(env_scaled, 2, sd) # standard deviations = 1

# the correlation analysis

PerformanceAnalytics::chart.Correlation(env_z, 
                                        histogram = TRUE, 
                                        pch = 19) # collinearity


# B) Q-mode (dissimilarity among sites or rows)

par(mfrow = c(1,1))
env_d <- dist(env_z)
env_d_single <- hclust(env_d, method = "single")
plot(env_d_single) 

# C) R- and Q-modes of PCA

pca_env1 <- prcomp(env_z)
summary(pca_env1) # variance explanation
pca_env1$rotation # variable contribution
pca_env1$x # sample scores
biplot(pca_env1) # dot=pca$x; arrow=pca$ration
biplot(pca_env1, scale = 1) 
biplot(pca_env1, scale = 2)

# using rda() from vegan package

vegan::decorana(env_clean) # model selection
env_pca <- rda(env_clean, # run PCA, same to rda(env_z)
               scale = TRUE) # calls a standardization

summary(env_pca, scaling = 2) # By default scaling 2
summary(env_pca, scaling = 1)

# (env_ev <- env_pca$CA$eig) # Selecting PC
# env_ev[env_ev > mean(env_ev)] 
# env_n <- length(env_ev)
# barplot(env_ev, main="Eigenvalues", col="grey", las=2)
# abline(h=mean(env_ev), col="red") 
# legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

plot(env_pca) # explainable variance of the axis
biplot(env_pca, scaling =1, # Q-mode
       main="scaling=1: object similarity") 
biplot(env_pca, scaling =2, # R-mode
       main="scaling=2: importance & corr") 

# 2) for the spe data
# A) R-mode analysis

spe_hel <- decostand(spe_clean, "hellinger")
cor_mat <- cor(spe_hel) # co-occurrence
heatmap(cor_mat)

# B) Q-mode analysis

# a. for the quantitative data

# bray-curtis dissimilarity with raw data
spe_db <- vegan::vegdist(spe_clean)	# Bray-Curtis as default
# equal to vegdist(spe_clean, "bray")	
spe_db

# hellinger dissimilarity 
spe_hel <- decostand(spe_clean, "hel")
spe_dh <- vegdist(spe_hel, method="euclidean") 

# b. for the presence-absence data

# Jaccard matrix
spe_dj <- dist(spe_clean, "binary") 
# equal to vegdist(spe_clean, "jac", binary=TRUE)
spe_dj 

spe_db_single <- hclust(spe_db, method = "single")
plot(spe_db_single) 

spe_db_complete <- hclust(spe_db, method = "complete") 
plot(spe_db_complete) 

spe_db_ward <- hclust(spe_db, method = "ward.D2") 
plot(spe_db_ward) 

library(vegan)
spe_dh <- vegdist(spe_hel, method="euclidean") 
par(mfrow=c(1,1))
spe_dh_single<-hclust(spe_dhel, method="single")
plot(spe_dh_single, main="Single linkage clustering", 
     hang =-1)

spe_dh_complete <- hclust(spe_dh, method = "complete")
plot(spe_dh_complete, main="Complete linkage clustering", 
     hang=-1) 

# C) R- and Q-modes of PCA

pca_spe1<- prcomp(spe_hel)
summary(pca_spe1) 
pca_spe1$rotation 
pca_spe1$x 
biplot(pca_spe1) 

# using rda() from vegan package

spe_hel <- decostand(spe_clean, "hellinger")
vegan::decorana(spe_hel) # DCA1 >4 Unimodal, DCA1 <3 linear 

spe_pca <- rda(spe_hel) # run pca with rda() 
summary(spe_pca, scaling = 2) 
summary(spe_pca, scaling = 1) 

# spe_ev <- spe_pca$CA$eig # # Selecting PC
# spe_ev[spe_ev>mean(spe_ev)]
# n <- length(spe_ev)
# barplot(spe_ev, main="Eigenvalues", col="grey", las=2)
# abline(h=mean(spe_ev), col="red") 
# legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

biplot(spe_pca, scaling =1, main="PCA scaling=1") # Q-mode
biplot(spe_pca, scaling =2, main="PCA scaling=2") # R-mode

# 3) RDA--the relationship between spe and env

spe_hel <- decostand(spe_clean, "hellinger")
env_z <- decostand(env_filled, "standardize") #  centers and scales variables

env_spe_rda <- rda(spe_hel ~ ., 
                   data = env_z) # Scaling = 2 (by default)
vif.cca(env_spe_rda) # multi-collinearity
summary(env_spe_rda)

anova(env_spe_rda, permutations = 1000) # Goodness of fit
anova(env_spe_rda, by = "axis", permutations = 1000)
anova(env_spe_rda, by = "term", permutations = 1000)

plot(env_spe_rda, scaling=1, main="scaling 1")# Scaling 1
plot(env_spe_rda, main="scaling 2")# Scaling 2

# for futher optimizing RDA
vif.cca(env_spe_rda) # deleting vif >10 variables

env_spe_rda_null <- rda(spe_hel ~ 1, data = env_z)
env_spe_rda_all <- rda(spe_hel ~ ., data = env_z)
(step_forward <- 
    ordiR2step(env_spe_rda_null, 
               scope = formula(env_spe_rda_all), 
               direction = "forward",
               pstep = 1000))
RsquareAdj(step_forward)$adj.r.squared

env_spe_rda_pars <- step_forward # Parsimonious model
                          
anova.cca(env_spe_rda_pars, permutations = 1000)
anova(env_spe_rda_pars, permutations = 1000, by = "axis")
anova(env_spe_rda_pars, permutations = 1000, by = "term")

vif.cca(env_spe_rda_all) # VIF comparison
vif.cca(env_spe_rda_pars)

par(mfrow = c(1, 2)) # plot final triplot

plot(env_spe_rda_pars, scaling = 1, display = c("sp", "lc", "cn"), 
     main = "Scaling 1") # Scaling 1

plot(env_spe_rda_pars, 
     display = c("sp", "lc", "cn"), # sp(species), lc(location),cn(Constraints)
     main = "Scaling 2") # Scaling 2
