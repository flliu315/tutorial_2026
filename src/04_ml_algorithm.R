# --------------------------------------------
# Script Name: Machine learning
# Purpose:     The script will show how machine learning
#              works and how we build with our own data.

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
example1 <- read.csv("data/ml_data/example1.csv", row.names = 1)
x <- example1$x
y <- example1$y
plot(y ~ x, data = example1, main="Scatter Plot with Line", 
     xlab = "X-axis", ylab = "Y-axis")
abline(lm(y ~ x))

lm_model <- lm(y~x, data = example1)
lm_model
summary(lm_model)

# From a statistic model to the machine learning

x <- c(100,120,140,160,180,200,220,240,260,280)
y <- c(55,60,62,64,68,70,80,85,90,95)
data <- data.frame(x,y)
data

plot(y ~ x)
abline(lm(y ~ x))

boxplot(x, main="x", sub=paste("Outlier rows: ",
                               boxplot.stats(x)$out))
boxplot(y, main="y", sub=paste("Outlier rows: ",
                               boxplot.stats(y)$out))

library(e1071)
plot(density(x), main = "Density Plot: x", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y), 2)))
plot(density(y), main = "Density Plot: y", ylab = "Frequency",
     sub= paste("Skewness: ", round(e1071::skewness(y), 2)))

linearMod <- lm(y ~ x)  # 构建拟合线性模型
print(linearMod)


summary(linearMod) #模型显著性检验

x <- c(100,120,140,160,180,200,220,240,260,280)
x_mean <- mean(x)
x_sd <- sd(x)
x_std <- (x - x_mean) / x_sd
X <- cbind(1,x_std)
y <- c(55,60,62,64,68,70,80,85,90,95)

cost <- function(X, y, theta){
  sum((X %*% theta -y)^2/2*length(y))
}

alpha <- 0.01
num_iters <- 1000
cost_history <- rep(0, num_iters)
theta_history <- list(num_iters)
theta <- matrix(c(0,0), nrow = 2)

for(i in 1:num_iters){
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
abline(coef=theta, col='black')

#################################################
## building a tree model

x <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680, 2720, 2790, 2880, 2976, 3870, 3910, 3960, 4320, 6770, 6900)
y <- c(6.176, 3.434, 3.638, 3.497, 3.178, 3.497, 4.205, 3.258, 2.565, 4.605, 3.738, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792, 2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792, 1.792, 2.565, 1.386, 1.946, 1.099)
plot(x, y, pch=21)
library(tree)
thresh <- tree(y~x)
print(thresh)
a <- mean(y[x < 2115])
b <- mean(y[x >= 2115])
lines(c(80,2115,2115,7000),c(a,a,b,b))


#################################################

set.seed(123)
sample(1:10, 5)


# building a classification model with randomForest
data(iris)
head(iris)

# Split data into training and test sets

n <- nrow(iris) # Determine the number of rows
train_samples <- round(0.75 * n) # the number of train samples
train_index <- sample(1:n, train_samples) # Sample row indices
train <- iris[train_index, ]
test <- iris[-train_index, ]

# Train a rf model 
library(randomForest)
set.seed(1234)
rf_model <- randomForest(Species ~ ., 
                         data = train, 
                         importance = T)

# View the model
print(rf_model)
plot(rf_model)

# Algorithm Tune (tuneRF) for mtry
set.seed(123)
bestmtry <- tuneRF(x= train[,-5], y= train[,5], 
                   stepFactor = 0.5, 
                   plot = TRUE, 
                   ntreeTry = 300,
                   trace = TRUE, 
                   improve = 0.05)
print(bestmtry)

rf_final <- randomForest(Species~., data=train, ntree = 300, 
                         mtry = 2, importance = TRUE)

print(rf_final)
plot(rf_final)

# Evaluating the Model 
predictions <- predict(rf_final, test) # make predictions
confusion_matrix <- table(test$Species, predictions) # Create a confusion matrix
print(confusion_matrix) # View the confusion matrix

# Get feature importance
importance(rf_final) 
varImpPlot(rf_final)

###################################################
# The principle of the boosting tree algorithm

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(5.56, 5.7, 5.91, 6.4, 6.8, 7.05, 8.9, 8.7, 9, 9.05)
df <- data.frame(x, y)

# ROUND 1
df$pred_1 <- mean(df$y)
df

df$resd_1 <- (df$y - df$pred_1)
head(df)

# ROUND 2
library(tree)
model1 <- tree(resd_1 ~ x, data = df)

df$pred_2 <- predict(model1, df)
head(df)

df$pred_1 + df$pred_2
df$pred_1 + (0.1*df$pred_2) # using LR=0.1 to avoid overfitting
df$resd_2 <- (df$y- (df$pred_1 + (0.1*df$pred_2)))
head(df)

# ROUND 3
model2 <- tree(resd_2 ~ x, data=df)
df$pred_3 <- predict(model2, df)
df

LR=0.1
df$resd_3 <- (df$y- (df$pred_1 + (LR*df$pred_2) + (LR*df$pred_3)))
head(df)

# writing a "for" loop to complete a "boosting" process

library(tree)
library(caret) 
library(ggplot2)
library(randomForest)

LR <- 0.15
nrounds <- 6

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(5.56, 5.7, 5.91, 6.4, 6.8, 7.05, 8.9, 8.7, 9, 9.05)
df <- data.frame(x, y)

prediction <- NaN
df <- cbind(df[1], prediction, df[2:ncol(df)])
head(df)

# ROUND 1
df$pred_1 <- mean(df$y)
df$prediction <- df$pred_1
df$resd_1 <- (df$y - df$prediction)
df

rmse <- RMSE(df$y, df$prediction) # RMSE() of caret
results <- data.frame("Round" = c(1), "RMSE" = c(rmse))
results 

# a for loop from ROUND 2

for (i in 2:nrounds){
  # Fit regression tree on previous residuals
  formula_str <- paste0("resd_", i - 1, " ~ x")
  mdl <- tree(as.formula(formula_str), data = df)
  
  # Predict residuals
  df[[paste0("pred_", i)]] <- predict(mdl, df)
  
  # Update final prediction
  df$prediction <- df$prediction + # here includes ROUND 1
    (LR*df[[paste0("pred_", i)]])
  
  
  # Compute new residuals
  df[[paste0("resd_", i)]] <- (df$y- df$prediction)
  
  # Compute RMSE
  rmse <- RMSE(df$y, df$prediction)
  results <- rbind(results, list("Round" = i, "RMSE" = rmse))
}

results

ggplot(results, aes(x = Round, y = RMSE)) +
  geom_line(color = "blue") +
  geom_point() +
  theme_minimal() +
  labs(title = "RMSE over Boosting Rounds", y = "RMSE")

# The comparison among tree-based models
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(5.56, 5.7, 5.91, 6.4, 6.8, 7.05, 8.9, 8.7, 9, 9.05)
df <- data.frame(x, y)

# Initialization
df$pred <- mean(df$y)       
df$pred_1 <- df$pred            
df$resd_1 <- df$y - df$pred     

rmse <- RMSE(df$y, df$pred)
results <- data.frame(Round = 1, RMSE = rmse)

nrounds <- 6

# Boosting loop
library(tree)
for (i in 2:nrounds) {
  formula_str <- paste0("resd_", i - 1, " ~ x")
  mdl <- tree(as.formula(formula_str), data = df)
  
  df[[paste0("pred_", i)]] <- predict(mdl, df)
  df$pred <- df$pred + df[[paste0("pred_", i)]]  
  df[[paste0("resd_", i)]] <- df$y - df$pred     
  
  rmse <- RMSE(df$y, df$pred)
  results <- rbind(results, data.frame(Round = i, RMSE = rmse))
}

results

# compare the boosting algorithm to tree and rf models

# tree model
tree_mdl <-tree(y ~ x, data=df)
prediction <- predict(tree_mdl, df)
tree_rmse <- RMSE(df$y, prediction)

# rf model
rf_mdl <-randomForest(y ~ x, data=df)
prediction <- predict(rf_mdl, df)
rf_rmse <- RMSE(df$y, prediction)

ggplot() +
  geom_line(data = results, aes(x=Round, y=RMSE)) +
  geom_hline(yintercept = tree_rmse, color = "red", linetype = "dashed") +
  geom_hline(yintercept = rf_rmse, color = "blue", linetype = "dashed") 

################################################
# building a classification model with caret
library(caret)
data(iris) 
head(iris)

set.seed(123)
index <- createDataPartition(iris$Species, p=0.8, list=FALSE) # 
train_data <- iris[index,]
test_data <- iris[-index,]

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

model_info <- getModelInfo()
names(model_info)


set.seed(123)
rf_fit1 <- train(Species~., data = train_data, method="rf")
rf_fit1
plot(rf_fit1)


fitControl <- trainControl(method = "repeatedcv", number = 5, 
                           repeats=3) 

set.seed(123)
rf_fit2 <- train(Species ~ ., data = train_data, method = "rf",
                 trControl = fitControl) 

rf_fit2

library(ModelMetrics)  
library(MLmetrics)
fitControl <- trainControl(method = 'repeatedcv', number = 5, repeats =3,
                           savePredictions = 'final', 
                           classProbs = TRUE,                 
                           summaryFunction=multiClassSummary) 

model_info[["rf"]]$parameters

rf_fit3 <- train(Species ~ ., data = train_data, method = "rf", 
                 tuneLength = 5,
                 trControl = fitControl,
                 verbose = FALSE)
rf_fit3$results

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
rf_fit4$results

library(dplyr)
cor_matrix <- train_data %>%
  select(where(is.numeric), -Species) %>%  # 只选数值列，排除 Species
  cor(use = "pairwise.complete.obs")  # 计算相关矩阵，去掉 NA
cor_matrix

to_remove <- findCorrelation(cor_matrix, # 查找并删除相关性高的变量
                             cutoff = 0.6, verbose = T, names = T, exact = T) 
print(to_remove)
clean_01 <- train_data[, !names(train_data) %in% to_remove]


df_01 <- data.frame(x1 = c("A", "A", "B","B", "C","C"),
                    x2 = c(2.5, 1.2, 1.9, 2.3, 2.5, 1.9),
                    y = c(3.8, 2.2, 2.7, 3.0,2.8, 2.2))
dummy <- dummyVars(y ~ ., data = df_01) # 用y列定义独热编码函数
df_02 <- predict(dummy, newdata = df_01) %>% # 对数据集进行独热编码
  as.data.frame()
df_02$y <- df_01$y # 重新增加编码过程中消失的y列

nzv <- nearZeroVar(clean_01, saveMetrics= T)
nzv_to_remove <- nzv %>% 
  filter(zeroVar==T | nzv==T) %>% 
  tibble::rownames_to_column("col_names") # 列举近零方差变量
clean_02 <-  clean_01 %>% 
  select(-pull(nzv_to_remove, col_names)) # 删除近零方差变量
med_impute <-  preProcess(clean_02, method="medianImpute") 
clean_03 <- predict(med_impute, newdata = clean_02)


set.seed(123)
rf_fit5 <- train(Species ~ .,
                 data = train_data, 
                 method = "rf",
                 preProcess = c("nzv", "center", "scale", "knnImpute", "BoxCox"),
                 na.action = na.pass, 
                 trControl = fitControl,
                 tuneLength=5) 
rf_fit5

# comparison of several models

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

# B) Gradient descent algorithm (machine learning)

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)

# plot data and converging fit
plot(x,y, main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]])
}
abline(coef=theta, col='blue')

#################################################
## 02-CRAT for classification and regression
#################################################
# A) CRAT for classification
# https://medium.com/@justindixon91/decision-trees-afc984d161bf
Class <- as.factor(c(0,0,0,0,0,1,1,1,1,1)) # 2 class vector
X1 <- c(0.6,0.8,1.2,1.3,1.7,2.3,2.5,2.9,3.1,3.2) # feature1
X2 <- c(0.8,1.8,2.7,0.4,2.2,0.7,2.4,1.6,2.1,0.2) # feature2
df <- cbind.data.frame(Class, X1, X2) 


plot(X1,X2,col="white")
points(X1[Class=="0"], X2[Class=="0"], col="blue", pch=19)
points(X1[Class=="1"], X2[Class=="1"], col="red", pch=19)

# calculate Gini Impurity to decide The potential splits
min(X1)
max(X1)
Predictor1test <- seq(from = 0, to = 4, by = 0.1) # < min(x1) and >max(x1)
length(Predictor1test)
Predictor2test <- seq(from =0, to = 3, by = 0.1) 
length(Predictor2test)

# Function to calculate the proportion of obs in the split
CalculateP <- function(i, index, m, k) { 
  if(m=="L") { # region (m) which match to class (k) 
    Nm <- length(df$Class[which(df[,index] <= i)]) # The number of obs in the region Rm
    Count <- df$Class[which(df[,index] <= i)] == k # The number of obs that match the class k
  } else {
    Nm <- length(df$Class[which(df[,index] > i)])
    Count <- df$Class[which(df[,index] > i)] == k
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
Predictor1test_gini <- cbind.data.frame(Predictor1test, Gini)
Predictor1test_gini

library(ggplot2)

ggplot(data = Predictor1test_gini, aes(x = Predictor1test, y = Gini)) +
  geom_line() 

Gini <- CalculateGini(Predictor2test, 3)
Predictor2test_gini<- cbind.data.frame(Predictor2test, Gini)
Predictor2test_gini
ggplot(data = Predictor2test_gini, aes(x = Predictor2test, y = Gini)) +
  geom_line() 

# plot the tree with one root node
library(tree)
tree_df = tree(Class ~ ., data = df)
plot(tree_df)
text(tree_df, pretty = 0)
title(main = "Classification Tree")

## B) CART for regresssion
# data and plot
x <- c(84, 100, 180, 253, 264, 286, 400, 130, 480, 1000, 
       1990, 2000, 2110, 2120, 2300, 1610, 2430, 2500, 2590, 2680,
       2720, 2790,2880, 2976, 3870, 3910, 3960, 4320, 6670, 6900)
y <- c(6.176, 3.434, 3.683, 3.479, 3.178, 3.497, 4.205, 3.258,
       2.565, 4.605, 3.783, 2.833, 3.091, 2.565, 1.792, 3.045, 1.792,
       2.197, 1.792, 2.197, 2.398, 2.708, 2.565, 1.386, 1.792,
       1.792, 2.565, 1.386, 1.946, 1.099)

df1 <- cbind.data.frame(x, y)
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

#############################################################
## 03- the "boosting tree" for regression
#############################################################
# A) run one round by one round to understand the "boosting"

library(tree) # calculating residuals in decision tree 
library(caret) # calculating mean squared error
library(ggplot2) # visualizating
library(randomForest) # comparing two building models

df <- mtcars
df
x_vars1 <- names(df[2:ncol(df)])
x_vars <- paste(x_vars1, collapse = " + ") # for convince

# ROUND 1
df$pred_1 <- mean(df$mpg)
df

df$resd_1 <- (df$mpg - df$pred_1)
head(df)

# ROUND 2
mdl <-eval(
  parse(text = 
          paste0(
            "tree(resd_1~", x_vars, ", data=df)"
          ) # creating string with paste0
  )  # changing to expression with parse
) # evaluating the expression with eval

df$pred_2 <- predict(mdl, df)
head(df)

df$pred_1 + df$pred_2
df$pred_1 + (0.1*df$pred_2) # using LR=0.1 to avoid overfitting
df$resd_2 <- (df$mpg- (df$pred_1 + (0.1*df$pred_2)))
head(df)

# ROUND 3
mdl <-eval(parse(text = paste0("tree(resd_2~", x_vars, ", data=df)")))
df$pred_3 <- predict(mdl, df)
df
LR=0.1
df$resd_3 <- (df$mpg- (df$pred_1 + (LR*df$pred_2) + (LR*df$pred_3)))
head(df)

# B) writing a "for" loop to complete a "boosting" process

library(tree)
library(caret) 
library(ggplot2)
library(randomForest)

LR <- 0.15
nrounds <- 50

df <- mtcars
x_vars1 <- names(df[2:ncol(df)])
x_vars <- paste(x_vars1, collapse = " + ")

prediction <- NaN
df <- cbind(df[1], prediction, df[2:ncol(df)])
head(df)

# ROUND 1
df$pred_1 <- mean(df$mpg)
df$prediction <- df$pred_1
df$resd_1 <- (df$mpg - df$prediction)
df

rmse <- RMSE(df$mpg, df$prediction) # RMSE() of caret
results <- data.frame("Round" = c(1), "RMSE" = c(rmse))

# a for loop from ROUND 2

for (i in 2:nrounds){
  mdl <-eval(parse(text = paste0("tree(resd_", i-1, "~", x_vars, ", 
                                 data=df)")))
  df[[paste0("pred_", i)]] <- predict(mdl, df)
  
  df$prediction <- df$prediction + # here includes ROUND 1
    (LR*df[[paste0("pred_", i)]])
  df[[paste0("resd_", i)]] <- (df$mpg- df$prediction)
  
  rmse <- RMSE(df$mpg, df$prediction)
  results <- rbind(results, list("Round" = i, "RMSE" = rmse))
}

results


# C) compare the boosting algorithm to tree and rf models

# tree model
tree_mdl <-eval(parse(text = paste0("tree(mpg~", x_vars, ", data=df)")))
prediction <- predict(tree_mdl, df)
tree_rmse <- RMSE(df$mpg, prediction)

# rf model
rf_mdl <-eval(parse(text = paste0("randomForest(mpg~", x_vars, ", data=df)")))
prediction <- predict(rf_mdl, df)
rf_rmse <- RMSE(df$mpg, prediction)

ggplot() +
  geom_line(data = results, aes(x=Round, y=RMSE)) +
  geom_hline(yintercept = tree_rmse, color = "red", linetype = "dashed") +
  geom_hline(yintercept = rf_rmse, color = "blue", linetype = "dashed") 

##########################################
## 04-build tree models with caret package
##########################################
# https://r.qcbs.ca/workshop04/book-en/multiple-linear-regression.html
library(caret)
# first take a look at the algorithms
modelnames <- paste(names(getModelInfo()), collapse=',')
modelnames

modelLookup("rpart")
modelLookup("rf")
modelLookup("gbm")

# load data
data <- read.csv("data/ml_data/dickcissel.csv", 
                 stringsAsFactors = TRUE)
dim(data)
head(data)

# Split the data into training and testing sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(data$abund, p = 0.7, 
                                  list = FALSE, 
                                  times = 1) # a partition
data_train <- data[trainIndex,]
data_test <- data[-trainIndex,]

# training a tree regression

# A) self-defining pre-processing of training data

# pre-processing usually includes center and scale data
# by defining preProcess = c('scale', 'center'), and put  
# it into train()

# B) self-defining resampling process for vilidation, and 
# putting it outside train(), citing it in train() by the
# parameter trControl

fitControl <- trainControl(method = "repeatedcv",   
                           number = 5,     # number of folds
                           repeats = 2)    # repeated two times

# C) self-defining way for finding hyperparameters 

# there ways include tunelength (automatically),
# tuneGrid (manually) and search = “random”, and 
# select one in train()

# Train a decision tree model
model_rpart <- train(abund ~ ., data = data_train, 
                     method = "rpart",
                     trControl = fitControl,
                     preProcess = c('scale', 'center'),
                     tuneLength = 5,# find an optimal cp based on its 5 values
                     metric="RMSE") 

# sum(is.na(data_train))  # number of missing values
# # data_train <- na.omit(data_train) # Remove the rows with missing values
# # or use imputation
# # preProcess(data_train, method = c("medianImpute"))
# 
# fitControl <- trainControl(method = "repeatedcv",   
#                            number = 5) # reduce size of folds

# Predict on the test data
predictions_rpart <- predict(model_rpart, newdata = data_test)

# evaluate regression performance
Metrics::rmse(data_test$abund, predictions_rpart)

# training a rf regression

model_rf <- train(abund ~ ., data = data_train, 
                  method = "rf",
                  trControl = fitControl,
                  preProcess = c('scale', 'center'),
                  tuneLength = 5,
                  metric="RSE") 

predictions_rf <- predict(model_rf, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_rf)

# training a boosting regression

model_gbm <- train(abund ~ ., data = data_train, 
                   method = "gbm",
                   trControl = fitControl,
                   preProcess = c('scale', 'center'),
                   tuneLength = 5,
                   metric="RMSE")  

predictions_gbm <- predict(model_gbm, newdata = data_test)

Metrics::rmse(data_test$abund, predictions_gbm)

# Compare the models' performances for final picking
models_compare <- resamples(list(TREE=model_rpart, 
                                 RF=model_rf, 
                                 GBM=model_gbm))
summary(models_compare)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), 
               y=list(relation="free"))
bwplot(models_compare, scales=scales)