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

x1 <- c(100,120,140,160,180,200,220,240,260,280)
y1 <- c(55,60,62,64,68,70,80,85,90,95)
df1 <- data.frame(x,y)
df1

plot(y1 ~ x1)
abline(lm(y1 ~ x1)) # check linear model

boxplot(x1, main="x", sub=paste("Outlier rows: ", # check the outliers
                               boxplot.stats(x1)$out))
boxplot(y1, main="y", sub=paste("Outlier rows: ",
                               boxplot.stats(y1)$out))

library(e1071) # check whether the data meet normality distrib
plot(density(x1), main = "Density Plot: x", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(x1), 2)))
plot(density(y1), main = "Density Plot: y", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y1), 2)))

lm_model1 <- lm(y1 ~ x1, data = df1)  # build a linear model
print(lm_model1)
summary(lm_model1) # examine the significance


# if the dataset is df3, building a statistic model 
# may not be suitable

x2 <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y2 <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df2 <- data.frame(x2, y2)
plot(y2 ~ x2, df2)
abline(lm(y2 ~ x2))

lm_model2 <- lm(y2 ~ x2, data = df2)  # build a linear model
print(lm_model2)
summary(lm_model2) # examine the significance

# B) gradient descent algorithm (machine learning) 

x1 <- c(100,120,140,160,180,200,220,240,260,280)
x1_mean <- mean(x1) # standardizing the data
x1_sd <- sd(x1)
x1_std <- (x1 - x1_mean) / x1_sd

X <- cbind(1,x1_std) # # add a column of 1's as intercept

y1 <- c(55,60,62,64,68,70,80,85,90,95)

cost <- function(X, y1, theta){ # cost function
  sum((X %*% theta -y1)^2/2*length(y1))
}


alpha <- 0.01 # learning rate and iteration limit
num_iters <- 1000

cost_history <- rep(0, num_iters) # keep history
theta_history <- list(num_iters)

theta <- matrix(c(0,0), nrow = 2) # initialize coefficients

for(i in 1:num_iters){ # gradient descent
  error <- (X %*% theta - y1)
  delta <- t(X) %*% error / length(y1)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y1, theta)
  theta_history[[i]] <- theta
}

print(theta)

plot(x1_std, y1, main = "Linear regression by gradient descent")

for (i in c(1,3,6,10,14,seq(50,num_iters,by=50))) {
  abline(coef=theta_history[[i]])
}
abline(coef=theta, col='red')


#################################################
## 02-CRAT for classification and regression
#################################################
## 1) CART for regression
# https://medium.com/@justindixon91/decision-trees-afc984d161bf
# https://www.causalmlbook.com/classification-and-regression-trees-cart.html

x1 <- c(100,120,140,160,180,200,220,240,260,280)
y1 <- c(55,60,62,64,68,70,80,85,90,95)
df1 <- data.frame(x1, y1)
plot(x1, y1, pch = 21)

# A) the CART algorithm principle

# defined MSE function
mse_split <- function(x, y, s){
  left <- y[x < s]
  right <- y[x >= s]
  
  if(length(left)==0 || length(right)==0) return(Inf)
  
  mean_left <- mean(left)
  mean_right <- mean(right)
  
  sum((left - mean_left)^2) + sum((right - mean_right)^2)
}

# first split candidates（for all x1 and y1 values）
splits <- sort(unique(x1))

# calculate MSE for each split
mse_values <- sapply(splits, function(s) mse_split(x1, y1, s))

# finding the best split
best_s <- splits[which.min(mse_values)]
best_s

c1 <- mean(y1[x1 < best_s])   
c2 <- mean(y1[x1 >= best_s])  

plot(x1, y1, pch = 21, bg = "lightblue")
lines(c(min(x1), best_s, best_s, max(x1)),
      c(c1, c1, c2, c2),
      col = "red", lwd = 2)
abline(v = best_s, col = "blue", lty = 2)

# second split candidates (for left and right)
left_x <- x1[x1 < best_s]
left_y <- y1[x1 < best_s]
splits_left <- sort(unique(left_x))
mse_values_left <- sapply(splits_left, function(s) mse_split(left_x, left_y, s))
best_s_left <- splits_left[which.min(mse_values_left)]
c1_left <- mean(left_y[left_x < best_s_left])  
c2_left <- mean(left_y[left_x >= best_s_left]) 

right_x <- x1[x1 >= best_s]
right_y <- y1[x1 >= best_s]
splits_right <- sort(unique(right_x))
mse_values_right <- sapply(splits_right, function(s) mse_split(right_x, right_y, s))
best_s_right <- splits_right[which.min(mse_values_right)]
c1_right <- mean(right_y[right_x < best_s_right])  
c2_right <- mean(right_y[right_x >= best_s_right]) 

plot(x1, y1, pch = 21, bg = "lightblue")

lines(c(min(x1), best_s, best_s, max(x1)),
      c(c1, c1, c2, c2),
      col = "red", lwd = 2)

lines(c(min(left_x), best_s_left, best_s_left, max(left_x)),
      c(c1_left, c1_left, c2_left, c2_left),
      col = "green", lwd = 2)

lines(c(min(right_x), best_s_right, best_s_right, max(right_x)),
      c(c1_right, c1_right, c2_right, c2_right),
      col = "blue", lwd = 2)

abline(v = c(best_s, best_s_left, best_s_right), col = "blue", lty = 2)

# B) building a tree using the tree package

library(tree)

tree_model <- tree(
  y1 ~ x1,
  control = tree.control(length(y1), mincut = 1, minsize = 2)
)

summary(tree_model)
tree_model
tree_model$frame
plot(tree_model)
text(tree_model)

z <- seq(min(x1), max(x1), length = 200)
y_pred <- predict(tree_model, newdata = data.frame(x1 = z))
plot(x1, y1, pch = 21, bg = "lightblue")
lines(z, y_pred, type = "s", col = "red", lwd = 2)

# C) building a tree using the rpart package
# data and plot
x2 <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y2 <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df2 <- data.frame(x2, y2)
plot(x2, y2, pch=21)

# the first point for partitioning
library(tree)
thresh <- tree(y2 ~ x2)
print(thresh)
a <- mean(y2[x2<2115])
b <- mean(y2[x2>=2115])
lines(c(80, 2115, 2115, 7000),
      c(a, a, b, b))

lines(c(80, 2115, 2115, 7000), 
      c(a, a, b, b), col = "white", lwd = 2) 

# the final tree

tree_model <- tree(y2 ~ x2)
z <- seq(80, 7000)
y2 <- predict(tree_model, list(x2 =z))
lines(z, y2)

tree_regres <- rpart(y2 ~ ., data = df2, method = "anova", 
                     control = rpart.control(minsplit = 10))
print(tree_regres)

rpart.plot(tree_regres,main = "Regression Tree")

# 2) using CRAT algorithm for classification
# A) the CART algorithm priciple for classification

y <- c(0,0,1,0,1,2,2,2,2,2) # 3 class vector
x1 <- c(0.6,0.8,1.2,1.3,1.7,2.3,2.5,2.9,3.1,3.2) # feature1
x2 <- c(0.8,1.8,2.7,0.4,2.2,0.7,2.4,1.6,2.1,0.2) # feature2
df3 <- data.frame(y, x1, x2) 
df3

df3 <- df3 %>%
  mutate(pch_vals = case_when(
    y == 0 ~ 16,    # 16 = solid circle
    y == 1 ~ 2,    # 17 = triangle
    y == 2 ~ 1      # 1 = hollow circle
  ))

# Plot with different shapes based on `pch_vals`
plot(df3$x1, df3$x2,
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

# B) training a classification tree using rpart
library(rpart)
tree_class = rpart(y ~ ., data = df3, method = "class", 
                   control = rpart.control(minsplit = 2))
print(tree_class)
summary(tree_class)
library(rpart.plot)
rpart.plot(tree_class, main = "Classification Tree")

######################################################
## 03- Ensemble Learning for classification and regression
######################################################
# 1) Bagging algorithm 

# A) for regression

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

# B) for classification

# classification for the df2
y <- c(0, 0, 1, 0, 1, 2, 2, 2, 2, 2)  # labels
x1 <- c(0.6, 0.8, 1.2, 1.3, 1.7, 2.3, 2.5, 2.9, 3.1, 3.2)  # feature1
x2 <- c(0.8, 1.8, 2.7, 0.4, 2.2, 0.7, 2.4, 1.6, 2.1, 0.2)  # feature2
df3 <- data.frame(y, x1, x2)

# 
clr <- c("pink", "red", "blue", "yellow", "darkgreen",
         "orange", "brown", "purple", "darkblue")

n <- nrow(df3)

# set layout of 3x3 
par(mfrow = c(3, 3))

# training 9 trees (B = 9)
for(i in 1:9) {
  set.seed(123) 
  idx <- sample(n, n, replace = TRUE)  # Bootstrap sampling
  tr <- df3[idx, ]
  
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

# 2) randomforest algorithm

# A) for regression

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

# for classification

# 3) the "boosting tree" for regression
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

# 4) compare the boosting algorithm to tree and rf models
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
str(mtcars)
?mtcars

# 1) for a tree model between mpg and others

# A) Split data into train and test (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]
head(test_data)

# B) find the most optimum parameters for a tree model
# https://danstich.github.io/stich/classes/BIOL217/12_cart.html
# https://rpubs.com/mpfoley73/529130

library(rpart)
?rpart
library(rpart.plot)
rpart_full <- rpart(mpg ~ ., data = train_data, 
                  method = "anova",
                  minsplit = 2, minbucket = 1,
                  xval = 5) # 5-fold cross-validation
print(rpart_full)

printcp(rpart_full)
plotcp(rpart_full)
opt_index <- which.min(rpart_full$cptable[,"xerror"])
opt_cp <- rpart_full$cptable[opt_index, "CP"]
opt_cp
rpart_pruned <- prune(rpart_full, cp = opt_cp)
print(rpart_pruned)

rpart.plot(rpart_pruned)
rpart.plot(rpart_full)

# C) model evaluation on test_data using R2 and RMSE
rpart_pred <- predict(rpart_pruned, test_data, type = "vector") 
library(caret)
rpart_R2 = R2(rpart_pred, test_data$mpg)
rpart_R2 
rpart_rmse = RMSE(rpart_pred, test_data$mpg)
rpart_rmse 

# 2) for a rf model between mpg and others

# A) Split data for proper evaluation (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) pre-train a rf model

library(randomForest)
set.seed(123)
rf_model <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = 500,
  mtry = 3,   # 初始值（p=10 → p/3≈3）
  importance = TRUE
)

print(rf_model) # Mean of squared residuals → OOB MSE
plot(rf_model)

# C) find the most optimum parameters for a rf model
# mtry → ntree → nodesize（using OOB）
# https://www.geeksforgeeks.org/r-machine-learning/how-to-calculate-the-oob-of-random-forest-in-r/

# optimize mtry based on OOB 

mtry_res <- tuneRF(
  x = train_data[, -1],  
  y = train_data$mpg,
  stepFactor = 1.5,  
  improve = 0.01,    
  ntreeTry = 500,
  trace = TRUE,
  plot = TRUE
)

mtry_res

best_mtry <- tune[which.min(mtry_res[,2]), 1]
best_mtry

# re-train a rf model with optimal mtry

rf_best <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = 500,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_best)

# optimize ntree after best_mtry

rf_temp <- randomForest(mpg ~ ., data = train_data, mtry=best_mtry,
                        ntree = 1000)

plot(rf_temp$mse, type = "l", xlab = "Number of Trees", ylab = "OOB MSE")

mse_vals <- rf_temp$mse
diff_mse <- abs(diff(mse_vals))
threshold <- 1e-4
best_ntree <- which(diff_mse < threshold)[1]
best_ntree

# re-train rf with best_mtry and best_ntree

rf_best <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  importance = TRUE
)

print(rf_best)

# search optimal nodesize with OOB 

nodesize_vals <- c(3, 5, 10)
nodesize_res <- data.frame()

for (n in nodesize_vals) {
  rf <- randomForest(
    mpg ~ ., data = train_data,
    ntree = best_ntree,
    mtry = best_mtry,
    nodesize = n
  )
  
  nodesize_res <- rbind(nodesize_res, data.frame(
    nodesize = n,
    OOB_MSE = oob_mse <- rf$mse[rf$ntree] # the final tree
  ))
}

nodesize_res

best_nodesize <- nodesize_res$nodesize[which.min(res$OOB_MSE)]
best_nodesize

plot(nodesize_res$nodesize, res$OOB_MSE, type = "b",
     xlab = "nodesize", ylab = "OOB MSE",
     main = "Nodesize Tuning")

# re-train rf with optimal mtry, ntree, nodesize

rf_final <- randomForest(
  mpg ~ ., 
  data = train_data,
  ntree = best_ntree,
  mtry = best_mtry,
  nodesize = best_nodesize,
  importance = TRUE
)

# D) evaluation on test data

rf_pred <- predict(rf_final, newdata = test_data)
rf_rmse <- sqrt(mean((test_data$mpg - rf_pred)^2))
rf_R2 <- 1 - sum((test_data$mpg - rf_pred)^2) /
  sum((test_data$mpg - mean(test_data$mpg))^2)

rf_rmse
rf_R2

# E) view the importance of features
importance(rf_final)
varImpPlot(rf_final)

# 3) for a boosting tree between mpg and others

# A) Split data for proper evaluation (70/30 split)

set.seed(123)  # Reproducibility
ind <- sample(1:nrow(mtcars), size = 0.7 * nrow(mtcars))
train_data <- mtcars[ind, ]
test_data <- mtcars[-ind, ]

# B) find the most optimum parameters for a gbm model
# shrinkage → depth → n.trees（using CV）

library(gbm)
gbm_model <- gbm(
  mpg ~ ., 
  data = train_data,
  distribution = "gaussian",   # for regres
  n.trees = 2000,              
  interaction.depth = 3,      
  shrinkage = 0.01,            
  n.minobsinnode = 3,
  cv.folds=5)

# best n.trees
best_iter <- gbm.perf(gbm_model, method = "cv")
best_iter

# best depth and nodesize
library(gbm)

shrinkage_vals <- c(0.05, 0.01)
depth_vals <- c(1, 2, 3)
minobs_vals <- c(1, 2, 3)

gbm_res <- data.frame()

set.seed(123)

for (s in shrinkage_vals) {
  for (d in depth_vals) {
    for (m in minobs_vals) {
      
      model <- tryCatch({
        gbm(
          mpg ~ .,
          data = train_data,
          distribution = "gaussian",
          n.trees = ifelse(s == 0.05, 1500, 3000),  # ✅ 根据学习率调整树数
          interaction.depth = d,
          shrinkage = s,
          n.minobsinnode = m,
          bag.fraction = 0.8,   
          cv.folds = 3,         
          verbose = FALSE
        )
      }, error = function(e) return(NULL))
      
      if (is.null(model)) next  
      
      best_iter <- gbm.perf(model, method = "cv", plot.it = FALSE)
      
      gbm_res <- rbind(gbm_res, data.frame(
        shrinkage = s,
        depth = d,
        minobs = m,
        trees = best_iter,
        error = min(model$cv.error)
      ))
    }
  }
}

gbm_res

best_params <- gbm_res[which.min(gbm_res$error), ]
best_params

# re-train a gbm model with best parameters
gbm_final <- gbm(
  mpg ~ .,
  data = train_data,
  distribution = "gaussian",
  n.trees = best_params$trees,
  interaction.depth = best_params$depth,
  shrinkage = best_params$shrinkage,
  n.minobsinnode = best_params$minobs
)

# C) evaluation on test data

gbm_pred <- predict(gbm_final, newdata = test_data)
gbm_rmse <- RMSE(test_data$mpg, gbm_pred)
gbm_rmse 

cat("rpart RMSE: ", rpart_rmse, "\n")
cat("gbm RMSE: ", gbm_rmse, "\n")
cat("rf RMSE: ", rf_rmse, "\n")

results <- data.frame(
  Actual = test_data$mpg,
  rf_Pred = rf_pred,
  gbm_Pred = gbm_pred,
  rpart_Pred = rpart_pred
)

results_long <- reshape(results, 
                        varying = c("rf_Pred", "gbm_Pred", "rpart_Pred"), 
                        v.names = "Prediction", 
                        timevar = "Model", 
                        times = c("rf", "gbm", "rpart"),
                        direction = "long")

ggplot(results_long, aes(x = Actual, y = Prediction, color = Model)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Model Comparison: Actual vs Predicted MPG",
       x = "Actual MPG", y = "Predicted MPG") +
  theme(legend.position = "top")

#############################################
## 05-build machine learning models by caret
#############################################
# https://r.qcbs.ca/workshop04/book-en/multiple-linear-regression.html
rm(list = ls())

library(caret)

modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames

modelLookup("rpart")
modelLookup("rf")
modelLookup("gbm")

#######################################################
# 1) train reg models between mpg and others of mtcars
#######################################################

# A) load and split data 
data("mtcars")

set.seed(123)
Index <- createDataPartition(mtcars$mpg, p = 0.7, list = FALSE) 
train <- mtcars[Index,]
test <- mtcars[-Index,]

# # B) pre-Processing the data setting for training
##########################################################
# skipping the whole B) step and running the C) step
##########################################################
# # a. one-hot encoding（categories → numeric）

library(tidyverse)
dmy <- dummyVars(~ ., data = train)
train_dmy <- predict(dmy, train) %>% 
  as.data.frame()
class(train_dmy)
test_dmy  <- predict(dmy, test) %>% 
  as.data.frame()

# b. impute missing and center/scale 

library(skimr)
skim(train_dmy)
skim(test_dmy)

preproc <- preProcess(train_dmy,
                  method = c("medianImpute", "center", "scale"))

train_preproced <- predict(preproc, train_dmy)
head(train_preproced)
test_preproced  <- predict(preproc, test_dmy)

# c. deleting the variables with zero variance 

nzv_cols <- nearZeroVar(train_preproced)
cols_keep <- setdiff(seq_along(train_preproced), nzv_cols)
train_final <- train_preproced[, cols_keep, drop = FALSE]
test_final  <- test_preproced[, cols_keep, drop = FALSE]
dim(train_final)
dim(test_final)

# d. self-defining re-sampling process for validation

fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 3)    # repeated two times

# ml_rpart <- train(...
#                   trControl = fitControl,
#                   ...
#                   ) 

# e. self-defining way for finding hyperparameters 

# the ways include tunelength (automatically),
# tuneGrid (manually) and search = “random”,

##############################################################
# C) training and evaluating models with caret
# a. a tree model

data("mtcars")

set.seed(123)
Index <- createDataPartition(mtcars$mpg, p = 0.7, list = FALSE) 
train <- mtcars[Index,]
test <- mtcars[-Index,]


fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 3)    # repeated two times

model_ranger <- train(mpg ~ ., data = train,
                     method = "ranger", # the tree algorithm
                     trControl = fitControl,
                     preProcess = c('medianImpute', 'nzv','scale', 'center'),
                     tuneLength = 5,# searching five cp
                     metric="RMSE") 

# Predict on the test data
pred_ranger <- predict(model_ranger, newdata = test)

# evaluate regression performance
rmse_ranger <- caret::RMSE(test$mpg, pred_ranger)
R2_ranger <- caret::R2(test$mpg, pred_ranger)

# b. a rf regression

model_rf <- train(mpg ~ ., data = train, 
                  method = "rf",# rf algorithm
                  trControl = fitControl,
                  preProcess = c('medianImpute', 'nzv','scale', 'center'),
                  tuneLength = 5,
                  metric="RMSE") 

pred_rf <- predict(model_rf, newdata = test)
rmse_rf <- caret::RMSE(test$mpg, pred_rf)
R2_rf <- caret::R2(test$mpg, pred_rf)

# c. a boosting regression

model_gbm <- train(mpg ~ ., data = train, 
                  method = "gbm",# gbm algorithm
                  trControl = fitControl,
                  tuneGrid = expand.grid(
                    n.trees = 30,
                    interaction.depth = 1,
                    shrinkage = 0.1,
                    n.minobsinnode = 3),
                  metric="RMSE") 

pred_gbm <- predict(model_gbm, newdata = test)
rmse_gbm <- caret::RMSE(test$mpg, pred_gbm)
R2_gbm <- caret::R2(test$mpg, pred_gbm)

# d. Compare the models' performances for final picking
models_compare <- resamples(list(RANGER=model_ranger, 
                                 RF=model_rf, 
                                 GBM=model_gbm))
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free"))
bwplot(models_compare, scales=scales)

#################################################
# 2) building classification models for iris
#################################################
rm(list = ls())

# A) loading and splitting data

data(iris) 
head(iris)

set.seed(123)
index_iris <- createDataPartition(iris$Species, p=0.8, list=FALSE) # 
train_dat <- iris[index_iris,]
test_dat <- iris[-index_iris,]

###############################################
# skipping the whole B) and running the C)
###############################################
# B) training a model with diff optimal mtry
# feature selection
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

# a. using default trainControl for optimal mtry
# i.e. trainControl(method = "boot", number = 25)
set.seed(123)
rf_fit1 <- train(Species~., 
                 data = train_dat, 
                 method="rf")  

# rf_fit1 <- train(Species~., 
#                  data = train_dat, 
#                  method="rf",
#                  trControl = trainControl(method = "boot", 
#                                           number = 25))  

rf_fit1
plot(rf_fit1)

# b. using self-defined trainControl for optimal mtry

fitControl <- trainControl(method = "repeatedcv", number = 5, 
                           repeats=3) 

set.seed(123)
rf_fit2 <- train(Species ~ ., data = train_dat, method = "rf",
                 trControl = fitControl) 

rf_fit2


# c. self-defined tuneLength for optimal mtry
fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics

rf_fit3 <- train(Species ~ ., data = train_dat, method = "rf", 
                 tuneLength = 3, # optimal mtry
                 trControl = fitControl,
                 verbose = FALSE)

rf_fit3

# rf_pred <- predict(rf_fit3, test_dat)
# rf_pred
# caret::confusionMatrix(reference = test_dat$Species, 
#                        data = rf_pred, # 用test评估模型
#                        mode = "everything")

# d. self-defined tuneGrid for optimal mtry
fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics

tune_grid <- expand.grid(mtry = c(1, 2, 3, 4))
set.seed(123) 
rf_fit4 <- train(Species ~ ., data = train_dat,  method = "rf",
                 tuneGrid = tune_grid,
                 trControl = fitControl,
                 metric = "Accuracy")
rf_fit4
##########################################################

# C) training a model with preProcess, trControl and optimal mtry

fitControl <- trainControl(method = 'repeatedcv', 
                           number = 5, 
                           repeats =3,
                           savePredictions = 'final', # keep results
                           classProbs = TRUE, # prob values                
                           summaryFunction=multiClassSummary) # metrics

set.seed(123)
rf_fit5 <- train(Species ~ .,
                 data = train_dat, 
                 method = "rf",
                 preProcess = c("nzv", "center", "scale", "knnImpute"),
                 na.action = na.pass, 
                 trControl = fitControl,
                 tuneLength=5,
                 metric = "Accuracy") 
rf_fit5

# D) simultaneously running several algorithms for classification

library(caretEnsemble)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  savePredictions = "final", 
  classProbs = TRUE # calculating ROC
)

set.seed(123)
multi_models <- caretList(
  Species ~ ., 
  data = train_dat, 
  trControl = fitControl,
  methodList = c('ranger', 'rf',  'gbm'),
  preProcess = c("nzv", "center", "scale", "knnImpute"),
  tuneLength=3,
  metric = "Accuracy"
)

names(multi_models)
multi_models$rf

# pred_rf <- multi_models$rf$pred
# cm_rf <- confusionMatrix(data = pred_rf$pred, 
#                          reference = pred_rf$obs)
# print(cm_rf) 
# 
# pred_ranger <- multi_models$ranger$pred
# cm_ranger <- confusionMatrix(data = pred_ranger$pred, 
#                              reference = pred_ranger$obs)
# print(cm_ranger)

# Resample results
resamples_list <- resamples(multi_models)
summary(resamples_list)

# Plot the results

dotplot(resamples_list, metric = "Accuracy")
bwplot(resamples_list)

##########################################
## 06-visualizing machine learning models
##########################################
data(mtcars)
head(mtcars)
set.seed(123)
idx <- createDataPartition(mtcars$mpg, p=0.8, list = FALSE)
train <- mtcars[idx, ]
test  <- mtcars[-idx, ]
train_control <- trainControl(method="cv", number=3)

# 1) training and visualizing models

# A) linear regression model

library(caret)
caret_lm_mdl <- train(mpg ~ wt, data = train, method = "lm",
                      trControl = train_control)
print(caret_lm_mdl)
pred <- predict(caret_lm_mdl, test)
rmse <- RMSE(test$mpg, pred) # Root Mean Squared Error
R2 <- R2(test$mpg, pred)

plot(mtcars$wt, mtcars$mpg, main="caret_lm Models")
abline(stat_lm_mdl, col="blue")

# B) a decision tree regression
# a. training and retraining with optimal cp
library(caret)
rpart_mdl <- train(
  mpg ~ wt,
  data = train,
  method = "rpart",
  trControl = train_control,
  tuneLength = 10,              
  control = rpart.control(minsplit = 2)
)

print(rpart_mdl)
plot(rpart_mdl)

best_cp <- rpart_mdl$bestTune$cp
best_cp

final_rpart_mdl <- train(
  mpg ~ wt,
  data = train,
  method = "rpart",
  trControl = trainControl(method = "none"),  # not necessary
  tuneGrid = data.frame(cp = best_cp),       # fix cp
  control = rpart.control(minsplit = 2)
)

# b. visualizing the tree diagram 
library(rpart.plot)
rpart.plot(
  final_rpart_mdl$finalModel,
  type = 2,       
  extra = 101,   
  under = TRUE   
)

# c. visualizing the step plot 
wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200)
pred <- predict(final_rpart_mdl$finalModel, 
                newdata = data.frame(wt = wt_grid))
plot(train$wt, train$mpg,
     pch = 16, col = "blue",
     xlab = "wt", ylab = "mpg",
     main = "Regression Tree as Step Function")

lines(wt_grid, pred, type = "s", col = "red", lwd = 2)

# C) random forest 
# a. training a rf model
rf_mdl <- train(mpg ~ wt, data = train, method = "rf")  # mtry
print(rf_mdl)

# b. visualizing individual trees
library(rpart)
library(rpart.plot)  # prp()

n <- nrow(train)
clr <- rainbow(6) 
par(mfrow = c(2,3))

# for the front 6 trees
for(i in 1:6) {
  set.seed(123 + i) 
  idx <- sample(n, n, replace = TRUE)  # Bootstrap sample
  tr <- df[idx, ]

  cart <- rpart(
    mpg ~ wt,          
    data = tr,  
    method = "anova",  
    control = rpart.control(minsplit = 2),
    cp = 0             
  )
  
  prp(cart, box.col = clr[i], main = paste("Tree", i))
}

par(mfrow = c(1,1))

# c. visualizing the smooth curve

wt_grid <- seq(min(train$wt), max(train$wt), length.out = 200)
rf_pred <- predict(rf_mdl, newdata = data.frame(wt = wt_grid))

plot(train$wt, train$mpg, pch = 16)
lines(wt_grid, rf_pred, lwd = 2)

# D) boosting tree
# a. training and retraining the model
library(caret)
library(gbm)  

tuneGrid = expand.grid(
  interaction.depth = c(1, 2),
  n.trees = c(50, 100),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = c(2, 3, 5)  
)

set.seed(123)
gbm_mdl <- train(mpg ~ wt, data = train, method = "gbm",
  trControl = train_control,
  tuneGrid = tuneGrid,
  bag.fraction = 0.8,   # 或 1
  verbose = FALSE
)

print(gbm_mdl)
plot(gbm_mdl)       
gbm_mdl$bestTune

gbm_final <- train(mpg ~ wt, data = train, method = "gbm",
  trControl = trainControl(method = "none"), # 
  tuneGrid = gbm_mdl$bestTune,
  verbose = FALSE)

# b. visualizing the smooth curve

wt_grid <- seq(min(train_clean$wt), max(train_clean$wt), length.out = 200)
pred <- predict(gbm_final, newdata = data.frame(wt = wt_grid))
plot(train_clean$wt, train_clean$mpg, pch = 16, col = "blue",
     xlab = "Weight (wt)", ylab = "MPG",
     main = "Boosted Regression Tree (GBM) Fit")
lines(wt_grid, pred, col = "red", lwd = 2)

# 2) the variable importance and effects
importance(rf_model)
varImpPlot(rf_model)

library(pdp)
partial(rf_model, pred.var = "wt", plot = TRUE)
partial(rf_model, pred.var = c("wt","hp"), plot = TRUE)

# 3) evaluating and selecting models 
pred <- predict(rf_model, mtcars)
plot(mtcars$mpg, pred)
abline(0,1,col="red") # y = x, 1 = slope

# residual analysis to examine if captured
residuals <- mtcars$mpg - pred
plot(pred, residuals) # random distribution 
abline(h=0,col="red") 
