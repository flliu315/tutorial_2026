# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This provide an overview of RStudio and show
#          how to do some basic operations on R console, 
#          including installing and loading packages.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-03-05
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# https://bookdown.org/manishpatwal/bookdown-demo/list-in-r.html

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
# save the script in a given folder: Desktop/myclass
 
##################################################
########## II. R data objects and operation ######
##################################################

# 01- R Variables
# https://www.geeksforgeeks.org/r-language/r-variables/
# Variables are objects in R that are used to store values

# Assign a string using "=", "<-" or "->" operator
var1 = "Hello Geeks"
print(var1)

x <- 5 #  assigned x value of 5
y <- 7 # assigned y value of 7

9 -> z

# Rules for Naming Variables in R

var1 <- 10
var.name <- "R"
var_name <- TRUE

value <- 100
Value <- 200

var$1 <- 5   
var#1 <- 10 

var <- 10
.var <- "Hello"

2var <- 5    
_var <- 10

.3var <- 10 

TRUE <- 1    
function <- 10  


# 01- data types and data objects
# https://www.geeksforgeeks.org/r-data-types/

# numeric, integer, logical, character, complex

# Double

x <- 5.23 # Assign a decimal value to x

is.numeric(x) # check the data type
is.integer(x)
is.double(x) # the same as is.numeric()

class(x) # check the data type
typeof(x)
mode(x)

# Integer

x <- 5L 

# Logical

x <- c(TRUE, TRUE, FALSE)

# Character

x <- "elevated"
is.character(x)
is.numeric(x)

x <- "3.14"
is.character(x)
is.numeric(x)

# complex

x <- 1 + 2i
class(x)

# These operations can be repeated in the RStudio script
# editor:

# click File –> New File –> R Script

# 02- data objects
# https://www.geeksforgeeks.org/r-objects/

# A) Vectors/scalars

c(1,4,7)
Num_variable <- c(1,4,7) # assign this to a variable for future use
print(Num_variable)
(Num_variable <- c(1,4,7)) # assign and print(s) 

# Basic information 
length(Num_variable)  # How many elements
typeof(Num_variable)  # Of which type
is.vector(Num_variable) # Data structure
is.list(Num_variable)
names(Num_variable)
str(Num_variable) 

a <- 100
is.vector(a)
length(a)

# Accessing elements
Num_variable[2]
Num_variable[-2]
which(Num_variable == "4") # know elements for location

# Indexing by subset()

(v <- c(1:2, NA, 4:6, NA, 8:10))
v[v > 5] # missing/NA values are preserved
subset(v, v > 5)  # missing/NA values are lost 

# B) Rectangular objects

# a) matrices/Arrays

(m0 <- matrix(data = 1:9, nrow = 3, byrow = TRUE))

x <- 1:3 # Creating 3 vectors
y <- 4:6
z <- 7:9

(m1 <- rbind(x, y, z)) 

# matrix attributions

mode(m0)
typeof(m0)
length(m0)

is.vector(m0)
is.matrix(m0)
dim(m0)

# Indexing matrices

m0[2, 3] #  rows, or columns of matrices
m0 > 5  # returns a matrix of logical values
sum(m0) # Computations with matrices
max(m0)
mean(m0)
colSums(m0)
rowSums(m0)
t(m0)

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
matrix_data=matrix(c(1,2,3,4,5,6,7,8), nrow=4) # default byrow=FALSE
print(matrix_data) 
class(matrix_data)
dataframe_data=as.data.frame(matrix_data) # convert the matrix into dataframe
print(dataframe_data)
class(dataframe_data)

dataframe_data1 <- data.frame(a = 1:3, b = letters[10:12],
                              c = seq(as.Date("2004-01-01"), by = "week", 
                                      length.out = 3),
                              stringsAsFactors = TRUE)
dataframe_data1 
class(dataframe_data1)

matrix_data1 <- data.matrix(dataframe_data1[1:2]) # column
class(dataframe_data1[1:2])

matrix_data1
class(matrix_data1)

matrix_data2 <- data.matrix(dataframe_data1)
matrix_data2

# convert a dataframe to an array

df1 <- data.frame(x = 1:5, y = 5:1)
df1

df2 <- data.frame(x = 11:15,y = 15:11)
df2

Array1 <- array(data = c(unlist(df1),  unlist(df2)),
                dim = c(5, 2, 2),
                dimnames = list(rownames(df1),
                                colnames(df1)))
Array1 

##################################################
###### III. R function objects and operations ####
##################################################

# 01- Built-in Functions
# https://www.datacamp.com/doc/r/functions

# Numeric Functions
abs(12)
log(12)
sqrt(121)
exp(15)
floor(8.9)
ceiling(8.9)
round(8.4)

# Character Functions

?substr
x <- "abcdef"
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
?mean
mean(x, trim=0,na.rm= FALSE )

x <- c(2, 4, 6, 100)
mean(x)

x <- c(2, 4, 6, 100)
mean(x, trim = 0.25)

sum(x)
range(x)

# Other things

sum(c(1, 2))
sum(1, 2, 3, NA, na.rm = TRUE)
paste0("hell", "o ", "world", "!") # note the "o "

# 02- installing and loading packages

# --from CRAN
# Install packages by IDE or using install.packages()

# install.packages('readr')
# install.packages(c('readr', 'ggplot2', 'tidyr'))

# --from GitHub

# install.packages('devtools')
# devtools::install_github('rstudio/shiny')

# --from special Repositories
# install.packages('furrr',
#                  repos='http://cran.us.r-project.org',
#                  dependencies=TRUE)

# --from Zip files
# Installing R Packages from Zip Files by IDE or
# install.packages('C:/Users/User/Downloads/abc_2.1.zip',
#                  repos=NULL, type='source')

# 
# library(ds4psy)  # load the package
# plot_fn() # check the parameters
# plot_fn(x = 1)
# plot_fn(x = 7)
# plot_fn(x = 5, y = 1)
# plot_fn(x = 5, y = 5, A = TRUE, B = FALSE, C = TRUE, 
#         D = FALSE, E = FALSE, F = FALSE,  g = "black")

library(tidyverse)

?filter()  # 2 different packages 

# Indicating the filter() function of a given package: 
?stats::filter
?dplyr::filter

# 03-using self-defined functions
# # https://rpubs.com/NateByers/functions
# 
# # Writing functions
# myMean <- function(x){
#   total_count_of_values <- length(x)
#   total_sum_of_values <- sum(x)
#   average_of_values <- total_sum_of_values/total_count_of_values
#   average_of_values
# }
# 

source("src/myMean.R") 

my_vector <- c(1, 3, 5, 2, 6, 9, 0)
vector_mean <- myMean(x = my_vector)
vector_mean

# add_three <- function(x){
#   y <- x + 3
#   return(y)
# }

source("src/add_three.R")
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

################################################
## IV. naming objects and operating on them ####
################################################

# 01-rules for naming objects

# using char, number and underscore to name objects

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

# Avoid using spaces inside variables 

# naming tea_pot instead of tea pot

# 02-input objects into R environment

##############################################
########### V. Best Practices for R ##########
##############################################
# https://swcarpentry.github.io/r-novice-inflammation/06-best-practices-R.html
# https://www.r-bloggers.com/2024/06/writing-r-code-the-good-way/

# 01- well-organized project 
# A well-organized directory structure helps navigating 
# the project efficiently. It separates data, scripts, and 
# results, making it easier to locate and manage files

# project/
#   ├── data/
#   ├── scripts/
#   └── results/


# 02- Customizing Snippets for tracking
# Starting with an annotated description of who write the
# code and what the code does for track when you have to 
# look at or change it in the future
# https://blog.devgenius.io/how-to-automatically-create-headers-in-r-scripts-be69152ac23f


# 03- Defining a relative path for import files into R and
# export them out R environment. For example:

input_file <- "data/data.csv" 
output_file <- "data/results.csv"

# 04- annotating and marking code using # or #- to set off 
#  code sections or separate the function definitions. 

input_data <- read.csv(input_file) # read input 
sample_number <- nrow(input_data) # get number of samples 
results <- some_other_function(input_file, 
                               sample_number) 

# 05-Proper indentation and spacing make code more readable 
#  and maintainable

vec <- c(1, 2, 3)

# 06- Pipes are used for streamline code by chaining 
#  operations in a readable manner

library(dplyr)
data %>%
  filter(x > 1) %>%
  summarise(mean_y = mean(y))

##############################################
####### VI. Link Rstudio with github ########
##############################################
# https://happygitwithr.com/rstudio-git-github
# Link your recol to a tutorial_2026
