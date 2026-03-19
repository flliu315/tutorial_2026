# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This provide an overview of RStudio and show
#          how to do some basic operations on R console, 
#          including installing and loading packages.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2026-03-11
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables


.# https://bookdown.org/manishpatwal/bookdown-demo/list-in-r.html

#######################################################
################ I. R and RStudio #####################
#######################################################
# 01-starting RStudio and configure its appearance
# and introduce RStudio panes and their functions

# getwd() # current working directory
# setwd() # Session > set working directory
# list.files() or dir()
# rm(list = ls())

# 02-basic operations via R console (one way)
# after the prompt type the commmands and hit

# Airthmetic Operations
7 + 4 # Addition
7 - 4 # Substraction
7 * 2 # Multiplication
7 / 2 # Division

3^2 # Square
sqrt(2)
5%/%2 # Integer Division
5%%2 # Modulus

# Logical Operations

!TRUE # "!" is Logical NOT
TRUE & FALSE # "&" is Logical AND
TRUE | FALSE # “|” is for Logical OR

# 03-basic operation via RStudio (another way)

# open editor: File > new file > R script
# repeat above in the RStuido editor and run
# save the script in a given folder: Desktop/myClass
 
##################################################
########## II. R objects and operation ###########
##################################################

# 01- R Variables
# https://www.geeksforgeeks.org/r-language/r-variables/
# Variables are objects in R that are used to store values
rm(list = ls())
# A) Assign a string using "=", "<-" or "->" operator
(var1 = "Hello Geeks") # input objects into R environment
print(var1) 

x <- 5 #  assigned x value of 5
y <- 7 # assigned y value of 7
9 -> z # assigned z value of 9

# B) naming variable objects and operating on them
# the names are case sensitive 

o <- 10  # assign/set o to 10
O <-  5  # assign/set O to  5

o * O  
o * 0  # Note: O differs from 0
o / O
O / 0

length(o) # Object shape
dim(o)

mode(o) # Object type


# using char, number and underscore to name objects to avoid spaces 
# naming tea_pot instead of tea pot

var2 <- 10
var.name <- "2.91"
var_name <- TRUE

value1 <- 100
Value2 <- 200

var <- 10
.var <- "Hello"

# C) the followings are not permitted

var$1 <- 5  #  not permitted
var#1 <- 10 # not permitted

2var <- 5    
_var <- 10
.3var <- 10 

TRUE <- 1    
function <- 10  

# 02- data types and data objects
# https://www.geeksforgeeks.org/r-data-types/

# 1) data types: numeric, integer, logical, character, complex

# Double

x1 <- 5.23 # Assign a decimal value to x1

is.numeric(x1) # check the data type
is.integer(x1)
is.double(x1) # the same as is.numeric()

class(x1) # check the data type
typeof(x1)
mode(x1)

# Integer

x2 <- 5L 

# Logical

x3 <- TRUE

# Character

x4 <- "elevated"
is.character(x4)
is.numeric(x4)

x5 <- "3.14"
is.character(x5)
is.numeric(x5)

# complex

x6 <- 1 + 2i
class(x6)

# 2) data objects
# https://www.geeksforgeeks.org/r-objects/

# A) Vectors/scalars

Num_variable <- c(1,4,7) # assign this to a variable for future use
print(Num_variable)
(Num_variable <- c(1,4,7)) # assign and print(s) 

# Basic information 
length(Num_variable)  # How many elements
typeof(Num_variable)  # Of which type
is.vector(Num_variable) # Data structure
is.list(Num_variable)
str(Num_variable) # numeric vector, its length =3, the three elements

a <- 100
is.vector(a)
length(a)

# Accessing elements
Num_variable[2]
Num_variable[-2]
which(Num_variable == "4") # know elements for location

# Indexing by subset()

(v <- c(1:2, NA, 4:6, NA, 8:10))
v[v > 5] # missing/NA values are preserved, 
subset(v, v > 5)  # missing/NA values are lost; v[(v > 5) & !is.na(v > 5)]

# B) Rectangular objects

# a) matrices/Arrays

(m0 <- matrix(data = 1:9, nrow = 3, byrow = TRUE))

x <- 1:3 # Creating 3 vectors
y <- 4:6
z <- 7:9

(m1 <- rbind(x, y, z)) 

# matrix attributions

mode(m1)
typeof(m1)
length(m1)
is.vector(m1)
is.matrix(m1)
dim(m1)

# Indexing matrices

m1[2, 3] #  rows, or columns of matrices
m1 > 5  # returns a matrix of logical values
sum(m1) # Computations with matrices
max(m1)
mean(m1)
colSums(m1)
rowSums(m1)
t(m1)

# b) data frames/tibbles

name   <- c("Adam", "Bertha", "Cecily", "Dora", "Eve", "Nero", "Zeno")
gender <- c("male", "female", "female", "female", "female", "male", "male")
age    <- c(21, 23, 22, 19, 21, 18, 24)
height <- c(165, 170, 168, 172, 158, 185, 182)

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = TRUE)
df  

is.matrix(df)
is.data.frame(df)
dim(df)

tb <- tibble::as_tibble(df) # Turn to a tibble tb 
dim(tb) 

# work with df

df[5, 3]  # numeric indexing 
df[6, ]  
df[ , 4] 

names(df)    
names(df)[4] 

df$gender  
df$age 

df$gender == "male"
sum(df$gender == "male") 

df$age < 21
df$age[df$age < 21] 
df$name[df$age < 21]

subset(df, age > 20) # Subsetting rectangular tables
subset(df, gender == "male")
subset(df, age > 20 | gender == "male")

df[age > 20, ]
df[gender == "male", ]
df[age > 20 | gender == "male", ] 
df[age > 20 & gender == "male", ]

# c) categories and factors

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = FALSE) # the default (as of R 4.0.0)
df
df$gender
is.character(df$gender)  
is.factor(df$gender)
all.equal(df$gender, gender) 

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = TRUE)

df$gender  
is.factor(df$gender)
typeof(df$gender)
unclass(df$gender)

df$gender <- as.factor(df$gender) # convert to a "factor"
df$gender

# d) Lists

l_1 <- list(1, 2, 3) # 3 elements (all numeric scalars)
l_1

vector <- c(1,2,3)
vector

l_2 <- list(1, c(2, 3))  # 2 elements (different lengths)
l_2

l_3 <- list(1, "B", 3)
l_3

# Inspecting lists

is.list(l_3)  # a list
is.list(1:3)  # a vector
is.list("A")
str(l_3)

# Accessing list elements

l_2[2] 
l_2[[2]] 

x <- list(1:3)
x[[1]][3] # The 3rd element at the first position

# C) conversion among objects

df$gender <- as.factor(df$gender) 
df$gender

# convert matrix to data.frame
m2 = matrix(c(1,2,3,4,5,6,7,8), nrow=4) # default byrow=FALSE
print(m2) 
class(m2)
df2 = as.data.frame(m2) # convert the matrix into dataframe
print(df2)
class(df2)

df3 <- data.frame(a = 1:3, b = letters[10:12],
                  c = seq(as.Date("2004-01-01"), by = "week", length.out = 3),
                  stringsAsFactors = TRUE)
df3
class(df3)

m3 <- data.matrix(df3[1:2]) # column
m3
class(m3)

# convert a dataframe to an array

df4 <- data.frame(x = 1:5, y = 5:1)
df4

df5 <- data.frame(x = 11:15,y = 15:11)
df5

?unlist
array1 <- array(data = c(unlist(df4),  unlist(df5)), # unlist
                dim = c(5, 2, 2), # row-col-layer
                dimnames = list(rownames(df4),
                                colnames(df4)))
array1 

# 03-R function objects and operations 

# 1) Built-in Functions
# https://www.datacamp.com/doc/r/functions

# Numeric Functions
abs(-12)
log(12)
sqrt(121)
exp(15)
floor(8.9)
ceiling(8.9)
round(8.4)

# Character Functions

x <- "abcdef"
?substr
substr(x , 2, 4)
substr(x = "television", start = 5, stop = 10)

text_vector <- c("DataScience", "datascience", "DATA", "science", 
                 "Science")
grep("science", text_vector, ignore.case = TRUE)

strsplit("abc", "")

paste("y",1:3,sep="")

x <- "abcdef"
toupper(x)

# Statistical Functions

x <- c(2, 4, 6, 100)
mean(x)
mean(x, trim = 0.25) # mean(x, trim=0, na.rm= FALSE )

sum(x)
range(x)

# Other things

sum(c(1, 2))
sum(1, 2, 3, NA, na.rm = TRUE)
paste0("hell", "o ", "world", "!") # note the "o "

# 2) installing and loading packages

# --from CRAN
# Install packages by IDE or using install.packages()

install.packages('readr')
install.packages(c('readr', 'ggplot2', 'tidyr'))

# --from GitHub

# install.packages('devtools')
# devtools::install_github('rstudio/shiny')

# --from special Repositories
# install.packages('furrr',
#                  repos='http://cran.us.r-project.org',
#                  dependencies=TRUE)

# --from Zip files
# Installing R Packages from Zip Files by IDE 


library(tidyverse)

?filter()  # 2 different packages 

# Indicating the filter() function of a given package: 
?stats::filter
?dplyr::filter

# 3) self-defined functions
# # https://rpubs.com/NateByers/functions
# 
# # Writing functions
myMean <- function(x){
  total_count_of_values <- length(x)
  total_sum_of_values <- sum(x)
  average_of_values <- total_sum_of_values/total_count_of_values
  average_of_values
}


source("myMean.R") 

my_vector <- c(1, 3, 5, 2, 6, 9, 0)
vector_mean <- myMean(x = my_vector)
vector_mean

# add_three <- function(x){
#   y <- x + 3
#   return(y)
# }

source("add_three.R")
add_three(5)

quadratic <- function(a, b, c){
  root1 <- (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  root2 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
  root1 <- paste("x =", root1)
  root2 <- paste("x =", root2)
  ifelse(root1 == root2, return(root1), return(c(root1, root2)))
}

quadratic(1, 6, 9)
quadratic(1, -8, 15)


##############################################
########### III. Best Practices for R ########
##############################################
# https://swcarpentry.github.io/r-novice-inflammation/06-best-practices-R.html
# https://www.r-bloggers.com/2024/06/writing-r-code-the-good-way/

# 01-Link a Rstudio project with github 

# https://happygitwithr.com/rstudio-git-github
# Link myClass to a tutorial_2026


# 02- well-organized project 
# A well-organized directory structure helps navigating 
# the project efficiently. It separates data, scripts, and 
# results, making it easier to locate and manage files

# project/
#   ├── data/
#   ├── scripts/
#   └── results/


# 03- Customizing Snippets for tracking
# Starting with an annotated description of who write the
# code and what the code does for track when you have to 
# look at or change it in the future
# https://blog.devgenius.io/how-to-automatically-create-headers-in-r-scripts-be69152ac23f


# 04- Defining a relative path for import files into R and
# export them out R environment. For example:

input_file <- "data/data.csv" 
output_file <- "data/results.csv"

# 05- annotating and marking code using # or #- to set off 
#  code sections or separate the function definitions. 

input_data <- read.csv(input_file) # read input 
sample_number <- nrow(input_data) # get number of samples 
results <- some_other_function(input_file, 
                               sample_number) 

# 06-Proper indentation and spacing make code more readable 
#  and maintainable

vec <- c(1, 2, 3)

# 07- Pipes are used for streamline code by chaining 
#  operations in a readable manner

library(dplyr)
data %>%
  filter(x > 1) %>%
  summarise(mean_y = mean(y))
