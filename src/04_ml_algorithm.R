# --------------------------------------------
# Script Name: Machine learning
# Purpose:     The script will show how machine learning
#              works and how to build a model with data.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-03-21
# --------------------------------------------
cat("\014") # Clears the R console
rm(list = ls()) # Remove all variables

##############################################
# 01-From statistic models to machine learning
##############################################
# A) least square algorithm (statistic model)

x <- c(100,120,140,160,180,200,220,240,260,280)
y <- c(55,60,62,64,68,70,80,85,90,95)
df1 <- data.frame(x,y)
df1

plot(y ~ x)
abline(lm(y ~ x)) # check linear model

boxplot(x, main="x", sub=paste("Outlier rows: ", # check the outliers
                               boxplot.stats(x)$out))
boxplot(y, main="y", sub=paste("Outlier rows: ",
                               boxplot.stats(y)$out))

library(e1071) # check whether the data meet normality distrib
plot(density(x), main = "Density Plot: x", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y), 2)))
plot(density(y), main = "Density Plot: y", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y), 2)))

linearMod <- lm(y ~ x, data = df1)  # build a linear model
print(linearMod)
summary(linearMod) # examine the significance

# B) gradient descent algorithm (machine learning) 

x <- c(100,120,140,160,180,200,220,240,260,280)
x_mean <- mean(x) # standardizing the data
x_sd <- sd(x)
x_std <- (x - x_mean) / x_sd

X <- cbind(1,x_std) # # add a column of 1's as intercept

y <- c(55,60,62,64,68,70,80,85,90,95)

cost <- function(X, y, theta){ # cost function
  sum((X %*% theta -y)^2/2*length(y))
}


alpha <- 0.01 # learning rate and iteration limit
num_iters <- 1000

cost_history <- rep(0, num_iters) # keep history
theta_history <- list(num_iters)

theta <- matrix(c(0,0), nrow = 2) # initialize coefficients

for(i in 1:num_iters){ # gradient descent
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

plot(x_std, y, main = "Linear regression by gradient descent")

for (i in c(1,3,6,10,14,seq(50,num_iters,by=50))) {
  abline(coef=theta_history[[i]])
}
abline(coef=theta, col='red')

#################################################
## 02-CRAT for classification and regression
#################################################
# A) CRAT for classification
# https://medium.com/@justindixon91/decision-trees-afc984d161bf
# https://www.causalmlbook.com/classification-and-regression-trees-cart.html

y <- c(0,0,1,0,1,2,2,2,2,2) # 3 class vector
x1 <- c(0.6,0.8,1.2,1.3,1.7,2.3,2.5,2.9,3.1,3.2) # feature1
x2 <- c(0.8,1.8,2.7,0.4,2.2,0.7,2.4,1.6,2.1,0.2) # feature2
df2 <- data.frame(y, x1, x2) 
df2

df2 <- df2 %>%
  mutate(pch_vals = case_when(
    y == 0 ~ 16,    # 16 = solid circle
    y == 1 ~ 2,    # 17 = triangle
    y == 2 ~ 1      # 1 = hollow circle
  ))

# Plot with different shapes based on `pch_vals`
plot(df2$x1, df2$x2,
     pch = df2$pch_vals, lwd = 2,
     ylab = "x2", xlab = "x1")

#  the optimal cutoff point/split on x1 should be 2.0
abline(v = 2.0, lty = 5, lwd = 2)
abline(h = 2.0, lty = 5, lwd = 2)

# calculate Gini Impurity to decide The potential splits
min(x1)
max(x1)
Predictor1test <- seq(from = 0, to = 4, by = 0.1) # < min(x1) and >max(x1)
length(Predictor1test)
Predictor2test <- seq(from =0, to = 3, by = 0.1) 
length(Predictor2test)

# Function to calculate the proportion of obs in the split
CalculateP <- function(i, index, m, k) { 
  if(m=="L") { # region (m) which match to class (k) 
    Nm <- length(df2$y[which(df2[,index] <= i)]) # The number of obs in the region Rm
    Count <- df2$y[which(df2[,index] <= i)] == k # The number of obs that match the class k
  } else {
    Nm <- length(df2$y[which(df2[,index] > i)])
    Count <- df2$y[which(df2[,index] > i)] == k
  } 
  P <- length(Count[Count==TRUE]) / Nm # Proportion calculation
  return(c(P,Nm)) # Returns the proportion and the number of obs
}

CalculateGini <- function(x, index) { # calculate the Gini Impurity
  Gini <- NULL # Create the Gini variables
  for(i in x) {
    pl0 <- CalculateP(i, index, "L", 0) # Proportion in the left region with class 0
    pl1 <- CalculateP(i, index, "L", 1)
    GiniL <- pl0[1]*(1-pl0[1]) + pl1[1]*(1-pl1[1]) # The Fini for the left region
    pr0 <- CalculateP(i, index, "R", 0)
    pr1 <- CalculateP(i, index, "R", 1)
    GiniR <- pr0[1]*(1-pr0[1]) + pr1[1]*(1-pr1[1])
    Gini <- rbind(Gini, sum(GiniL * pl0[2]/(pl0[2] + pr0[2]),GiniR * pr0[2]/(pl0[2] + pr0[2]), na.rm = TRUE)) # Need to weight both left and right Gini scores when combining both
  }
  return(Gini)
}

Gini <- CalculateGini(Predictor1test, 2)
Gini
Predictor1test_gini <- cbind.data.frame(Predictor1test, Gini)
Predictor1test_gini

library(ggplot2)

ggplot(data = Predictor1test_gini, aes(x = Predictor1test, 
                                       y = Gini)) +
  geom_line() 

Gini <- CalculateGini(Predictor2test, 3)
Predictor2test_gini<- cbind.data.frame(Predictor2test, Gini)
Predictor2test_gini
ggplot(data = Predictor2test_gini, aes(x = Predictor2test, y = Gini)) +
  geom_line() 

# train a classification tree using rpart
library(rpart)
tree_class = rpart(y ~ ., data = df2, method = "class", 
                   control = rpart.control(minsplit = 2))
print(tree_class)
summary(tree_class)
library(rpart.plot)
rpart.plot(tree_class, main = "Classification Tree")

## B) CART for regresssion
# data and plot
x <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df3 <- data.frame(x, y)
plot(x, y, pch=21)

# the first point for partitioning
library(tree)
thresh <- tree(y ~ x)
print(thresh)
a <- mean(y[x<2115])
b <- mean(y[x>=2115])
lines(c(80, 2115, 2115, 7000),
      c(a, a, b, b))

lines(c(80, 2115, 2115, 7000), 
      c(a, a, b, b), col = "white", lwd = 2) 

# the final tree

model <- tree(y ~ x)
z <- seq(80, 7000)
y <- predict(model, list(x =z))
lines(z, y)

tree_regres <- rpart(y ~ ., data = df3, method = "anova", 
                      control = rpart.control(minsplit = 10))
print(tree_regres)

rpart.plot(tree_regres,main = "Regression Tree")

######################################################
## 03- Ensemble Learning for classification and regression
######################################################
# 1) Bagging algorithm for classification

# classification for the df2
y <- c(0, 0, 1, 0, 1, 2, 2, 2, 2, 2)  # labels
x1 <- c(0.6, 0.8, 1.2, 1.3, 1.7, 2.3, 2.5, 2.9, 3.1, 3.2)  # feature1
x2 <- c(0.8, 1.8, 2.7, 0.4, 2.2, 0.7, 2.4, 1.6, 2.1, 0.2)  # feature2
df2 <- data.frame(y, x1, x2)

# 
clr <- c("pink", "red", "blue", "yellow", "darkgreen",
         "orange", "brown", "purple", "darkblue")

n <- nrow(df2)

# set layout of 3x3 
par(mfrow = c(3, 3))

# training 9 trees (B = 9)
for(i in 1:9) {
  set.seed(123) 
  idx <- sample(n, n, replace = TRUE)  # Bootstrap sampling
  tr <- df2[idx, ]
  
  cart <- rpart(
    y ~ x1 + x2,
    data = tr,  
    method = "class", 
    control = rpart.control(minsplit = 2),
    cp = 0  # unpruned
  )
  
  prp(cart, box.col = clr[i])
}

par(mfrow = c(1, 1))

# Bagging algorithm for regression

library(ggplot2)
library(caret) # evaluation performance

data(mtcars)
set.seed(123)
n <- nrow(mtcars)
B <- 50  # trainig 50 trees

pred_list <- matrix(NA, nrow = n, 
                    ncol = B) # save pred

for(i in 1:B) {
  # Bootstrap sampling
  idx <- sample(n, n, replace = TRUE) 
  tr <- mtcars[idx, ] 
  
  tree_model <- rpart(mpg ~ ., data = tr, 
                      method = "anova", 
                      control = rpart.control(minsplit = 2, cp = 0))
  
  pred_list[, i] <- predict(tree_model, newdata = mtcars)
}


bagging_pred <- rowMeans(pred_list)
mse <- mean((mtcars$mpg - bagging_pred)^2)
cat("Bagging Model MSE:", mse, "\n")

ggplot(mtcars, aes(x = mpg, y = bagging_pred)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Actual MPG") +
  ylab("Predicted MPG") +
  ggtitle("Bagging Model: Actual vs Predicted MPG") +
  theme_minimal()

# 2) randomforest algorithm

library(randomForest)
set.seed(123) 
rf_model <- randomForest(mpg ~ ., data = mtcars, ntree = 500)
print(rf_model)

rf_model$mse
rf_model$rsq

rf_pred <- predict(rf_model, newdata = mtcars)
ggplot(mtcars, aes(x = mpg, y = rf_pred)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Actual MPG") +
  ylab("Predicted MPG") +
  ggtitle("Random Forest: Actual vs Predicted MPG") +
  theme_minimal()


## 3) the "boosting tree" for regression
# A) run one round by one round to understand the "boosting"

library(tree) # calculating residuals in decision tree 
library(caret) # calculating mean squared error
library(ggplot2) # visualizating
library(randomForest) # comparing two building models

data()
df4 <- mtcars
df4
x_vars1 <- names(df4[2:ncol(df4)])
x_vars <- paste(x_vars1, collapse = " + ") # for convince
x_vars
# ROUND 1
df4$pred_1 <- mean(df4$mpg)
df4

df4$resd_1 <- (df4$mpg - df4$pred_1)
head(df4)

# ROUND 2
mdl <-eval(
  parse(text = 
          paste0(
            "tree(resd_1~", x_vars, ", data=df4)"
          ) # creating string with paste0
  )  # changing to expression with parse
) # evaluating the expression with eval

df4$pred_2 <- predict(mdl, df4)
head(df4)

# df4$resd_2 <- df4$mpg- (df4$pred_1 + df4$pred_2)

df4$pred_1 + (0.1*df4$pred_2) # using LR=0.1 to avoid overfitting
df4$resd_2 <- (df4$mpg- (df4$pred_1 + (0.1*df4$pred_2)))
head(df4)

# ROUND 3
mdl <-eval(parse(text = paste0("tree(resd_2~", x_vars, ", data=df4)")))
df4$pred_3 <- predict(mdl, df4)
df4
LR=0.1
df4$resd_3 <- (df4$mpg- (df4$pred_1 + (LR*df4$pred_2) + (LR*df4$pred_3)))
head(df4)

# B) writing a "for" loop to complete a "boosting" process

library(tree)
library(caret) 
library(ggplot2)
library(randomForest)

LR <- 0.15
nrounds <- 50

df4 <- mtcars
x_vars1 <- names(df[2:ncol(df4)])
x_vars <- paste(x_vars1, collapse = " + ")

prediction <- NaN
df4 <- cbind(df4[1], prediction, df4[2:ncol(df4)])
head(df4)

# ROUND 1
df4$pred_1 <- mean(df4$mpg)
df4$prediction <- df4$pred_1
df4$resd_1 <- (df4$mpg - df4$prediction)
df4

rmse <- RMSE(df4$mpg, df4$prediction) # RMSE() of caret
results <- data.frame("Round" = c(1), "RMSE" = c(rmse))

# a for loop from ROUND 2

for (i in 2:nrounds){
  mdl <-eval(parse(text = paste0("tree(resd_", i-1, "~", x_vars, ", 
                                 data=df4)")))
  df4[[paste0("pred_", i)]] <- predict(mdl, df4)
  
  df4$prediction <- df4$prediction + # here includes ROUND 1
    (LR*df4[[paste0("pred_", i)]])
  df4[[paste0("resd_", i)]] <- (df4$mpg- df4$prediction)
  
  rmse <- RMSE(df4$mpg, df4$prediction)
  results <- rbind(results, list("Round" = i, "RMSE" = rmse))
}

results

# C) compare the boosting algorithm to tree and rf models
# tree model
tree_mdl <-eval(parse(text = paste0("tree(mpg~", x_vars, ", 
                                    data=df4)")))
prediction <- predict(tree_mdl, df4)
tree_rmse <- RMSE(df4$mpg, prediction)

# rf model
rf_mdl <-eval(parse(text = paste0("randomForest(mpg~", x_vars, ", 
                                  data=df4)")))
prediction <- predict(rf_mdl, df4)
rf_rmse <- RMSE(df4$mpg, prediction)

ggplot() +
  geom_line(data = results, aes(x=Round, y=RMSE)) +
  geom_hline(yintercept = tree_rmse, color = "red", linetype = "dashed") +
  geom_hline(yintercept = rf_rmse, color = "blue", linetype = "dashed") 


################################################
## 04-build models and optimize their parameters
##    to obtain high performance
################################################
rm(list = ls())

data() 
data("mtcars")
?mtcars

# 1) for a decision tree model

# A) Split data into train and test (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) find the most optimum parameters for a tree model
# https://danstich.github.io/stich/classes/BIOL217/12_cart.html
# https://rpubs.com/mpfoley73/529130

library(rpart)
?rpart
library(rpart.plot)
fulltree <- rpart(mpg ~ ., data = train_data, 
                  method = "anova",
                  minsplit = 2, minbucket = 1,
                  xval = 5) # 5-fold cross-validation
printcp(fulltree)
plotcp(fulltree)
opt_index <- which.min(fulltree$cptable[,"xerror"])
opt_cp <- fulltree$cptable[opt_index, "CP"]
opt_cp
prunedtree <- prune(fulltree, cp = opt_cp)
rpart.plot(prunedtree)

# C) model evaluation on test_data using R2 and RMSE
tree_pred <- predict(prunedtree, test_data, type = "vector") 
library(caret)
tree_R2 = R2(tree_pred, test_data$mpg)
tree_R2 
tree_rmse = RMSE(tree_pred, test_data$mpg)
tree_rmse 

# 2) for a random forest model

# A) Split data for proper evaluation (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) find the most optimum parameters for a tree model
# https://www.geeksforgeeks.org/r-machine-learning/how-to-calculate-the-oob-of-random-forest-in-r/
library(randomForest)
set.seed(123)

rf_model <- randomForest(
  mpg ~ ., 
  data = mtcars,
  ntree = 500,
  mtry = 3,   # 初始值（p=10 → p/3≈3）
  importance = TRUE
)

plot(rf_model)

# C) optimal mtry based on oob

tune <- tuneRF(
  x = mtcars[, -1],  
  y = mtcars$mpg,
  stepFactor = 1.5,  
  improve = 0.01,    
  ntreeTry = 500,
  trace = TRUE,
  plot = TRUE
)
best_mtry <- tune[which.min(tune[,2]), 1]
best_mtry

# D) using optimal mtry to re-train rf model

rf_best <- randomForest(
  mpg ~ ., 
  data = mtcars,
  ntree = 500,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_best)

# further optimizing ntree

rf_temp <- randomForest(mpg ~ ., data = mtcars, ntree = 1000)

plot(rf_temp$mse, type = "l", xlab = "Number of Trees", ylab = "OOB MSE")

# optimal nodesize 

nodesize_vals <- c(3, 5, 10)
results <- data.frame()

for (n in nodesize_vals) {
  rf <- randomForest(
    mpg ~ ., data = mtcars,
    ntree = 500,
    mtry = best_mtry,
    nodesize = n
  )
  
  res <- rbind(results, data.frame(
    nodesize = n,
    OOB_MSE = rf$mse[500]
  ))
}

res

# # view the importance of features
# importance(rf_best)
# varImpPlot(rf_best)

# Evaluation on test data

rf_model <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

rf_model

rf_pred <- predict(rf_model, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$mpg - rf_pred)^2))
rf_R2 <- 1 - sum((test_data$mpg - rf_pred)^2) /
  sum((test_data$mpg - mean(test_data$mpg))^2)

rf_rmse
rf_R2


# build a boosting tree
library(gbm)
boost_model <- gbm(
  mpg ~ ., 
  data = train_data,
  distribution = "gaussian",   # for regres
  n.trees = 500,              
  interaction.depth = 3,      
  shrinkage = 0.01,            
  n.minobsinnode = 2           
)

boost_pred <- predict(boost_model, newdata = test_data)
boost_rmse <- RMSE(test_data$mpg, boost_pred)
boost_rmse 

cat("Tree RMSE: ", tree_rmse, "\n")
cat("Boosting RMSE: ", boost_rmse, "\n")
cat("RF RMSE: ", rf_rmse, "\n")

results <- data.frame(
  Actual = test_data$mpg,
  RF_Pred = rf_pred,
  Boost_Pred = boost_pred,
  Tree_Pred = tree_pred
)

results_long <- reshape(results, 
                        varying = c("RF_Pred", "Boost_Pred", "Tree_Pred"), 
                        v.names = "Prediction", 
                        timevar = "Model", 
                        times = c("RF", "Boosting", "Tree"),
                        direction = "long")

ggplot(results_long, aes(x = Actual, y = Prediction, color = Model)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Model Comparison: Actual vs Predicted MPG",
       x = "Actual MPG", y = "Predicted MPG") +
  theme(legend.position = "top")

##########################################
## 05-build machine learning models by caret
##########################################
# https://r.qcbs.ca/workshop04/book-en/multiple-linear-regression.html


library(caret)
# 1) taking a look at the algorithms
modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames

modelLookup("rpart")
modelLookup("rf")
modelLookup("gbm")

# 2) training regression models 
# A) load and split data 
df5 <- read.csv("data/dickcissel.csv", 
                 stringsAsFactors = TRUE)
str(df5)
head(df5)

set.seed(123)
Index <- createDataPartition(df5$abund, p = 0.7, 
                                  list = FALSE, 
                                  times = 1) # a partition
data_train <- df5[Index,]
data_test <- df5[-Index,]


# # B) self-defining pre-processing of training data
# 
# # a. one-hot encoding（categories → numeric）
# 
# dmy <- dummyVars(~ ., data = train_data)
# 
# train_x <- predict(dmy, train_data)
# test_x  <- predict(dmy, test_data)
# 
# train_x <- as.data.frame(train_x)
# test_x  <- as.data.frame(test_x)
# 
# # b. impute missing if having missing data
# 
# library(skimr)
# skim(train_x)
# skim(test_x)
# 
# pre <- preProcess(train_x, 
#                   method = c("medianImpute", "center", "scale"))
# 
# train_x <- predict(pre, train_x)
# test_x  <- predict(pre, test_x)
# 
# # c. pre-processing usually includes center and scale data
#  
# train_x_stded <- preProcess(train_x, method = c("center", "scale"))

data_train_stded <- preProcess(data_train, method = c("center", "scale"))

# C) self-defining re-sampling process for validation, and 
# citing it in train() by the parameter trControl

fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 2)    # repeated two times

# ml_rpart <- train(...
#                   trControl = fitControl,
#                   ...
#                   ) 

# D) self-defining way for finding hyperparameters 

# the ways include tunelength (automatically),
# tuneGrid (manually) and search = “random”,

# E) training and evaluating models
# a. a decision tree 
model_rpart <- train(abund ~ ., data = data_train, 
                     method = "rpart", # the tree algorithm
                     trControl = fitControl,
                     preProcess = c('scale', 'center'),
                     tuneLength = 5,# find an optimal cp based on its 5 values
                     metric="RMSE") 

# sum(is.na(data_train))  # number of missing values
# data_train <- na.omit(data_train) # Remove the rows with missing values
# or use imputation
# preProcess(data_train, method = c("medianImpute"))
# 
# fitControl <- trainControl(method = "repeatedcv",   
#                            number = 5) # reduce size of folds

# Predict on the test data
predictions_rpart <- predict(model_rpart, newdata = data_test)

# evaluate regression performance
Metrics::rmse(data_test$abund, predictions_rpart)

# b. training a rf regression

model_rf <- train(abund ~ ., data = data_train, 
                  method = "rf",# rf algorithm
                  trControl = fitControl,
                  preProcess = c('scale', 'center'),
                  tuneLength = 5,
                  metric="RSE") 

predictions_rf <- predict(model_rf, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_rf)

# c. training a boosting regression

model_gbm <- train(abund ~ ., data = data_train, 
                   method = "gbm", # boosting algorithm
                   trControl = fitControl,
                   preProcess = c('scale', 'center'),
                   tuneLength = 5,
                   metric="RMSE")  

predictions_gbm <- predict(model_gbm, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_gbm)

# d. Compare the models' performances for final picking
models_compare <- resamples(list(TREE=model_rpart, 
                                 RF=model_rf, 
                                 GBM=model_gbm))
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free"))
bwplot(models_compare, scales=scales)

# 3) building classification models

# the models from caret
model_info <- getModelInfo()
names(model_info)
model_info[["rf"]]$parameters

# A) loading and spliting data

data(iris) 
head(iris)

set.seed(123)
index <- createDataPartition(iris$Species, p=0.8, list=FALSE) # 
train_data <- iris[index,]
test_data <- iris[-index,]

# B) feature selection
featurePlot(x = iris[, 1:4], y = iris$Species, plot = "density",
            scales = list(x = list(relation = "free"), y = list(relation="free")),
            pch = "|",
            layout = c(4, 1),
            auto.key = list(columns = 3))


set.seed(123) 
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x = iris[, 1:4], y = iris$Species, rfeControl = ctrl)
lmProfile

# C) training a model with rf
# a. using default trainControl for optimal mtry
# i.e. trainControl(method = "boot", number = 25)
set.seed(123)
rf_fit1 <- train(Species~., 
                 data = train_data, 
                 method="rf")  

# rf_fit1 <- train(Species~., 
#                  data = train_data, 
#                  method="rf",
#                  trControl = trainControl(method = "boot", 
#                                           number = 25))  

rf_fit1
plot(rf_fit1)


# b. using self-defined trainControl way for optimal mtry
fitControl <- trainControl(method = "repeatedcv", number = 5, 
                           repeats=3) 

set.seed(123)
rf_fit2 <- train(Species ~ ., data = train_data, method = "rf",
                 trControl = fitControl) 

rf_fit2

library(ModelMetrics)  
library(MLmetrics)

# c. self-defined optimal parameters
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics

rf_fit3 <- train(Species ~ ., data = train_data, method = "rf", 
                 tuneLength = 5, # optimal mtry
                 trControl = fitControl,
                 verbose = FALSE)

rf_fit3

# rf_pred <- predict(rf_fit3, test_data)
# rf_pred
# caret::confusionMatrix(reference = test_data$Species, data = rf_pred, # 用test评估模型
#                        mode = "everything")
# library(MLeval) 
# x <- evalm(rf_fit3)
# x$roc

tune_grid <- expand.grid(mtry = c(1, 2, 3, 4))
set.seed(123) 
rf_fit4 <- train(Species ~ ., data = train_data,  method = "rf",
                 tuneGrid = tune_grid,
                 trControl = fitControl,
                 metric = "Accuracy")
rf_fit4

# d. adding data preProcess 
set.seed(123)
rf_fit5 <- train(Species ~ .,
                 data = train_data, 
                 method = "rf",
                 preProcess = c("nzv", "center", "scale", "knnImpute", "BoxCox"),
                 na.action = na.pass, 
                 trControl = fitControl,
                 tuneLength=5) 
rf_fit5

# D) comparison of several algorithms

library(caretEnsemble)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = TRUE,
  classProbs = TRUE
)

algorithmList <- c('rf', 'rpart', 'gbm')

set.seed(123)
options(na.action = na.pass)
models <- caretList(
  Species ~ ., 
  data = train_data, 
  trControl = fitControl,
  methodList = algorithmList,
  preProcess = c("nzv", "center", "scale", "knnImpute", "BoxCox")
)

# Resample results
results <- resamples(models)
summary(results)

# Plot the results
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)
