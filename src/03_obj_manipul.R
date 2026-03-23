# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces some examples about basic 
#          manipulation on data and self-defined functions.
#          
# Author:  Fanglin Liu
# Email:   flliu315@163.com
# Date:    2026-03-21
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#################################################
## 01-data manipluation using tidyverse
#################################################
# 1) built-in datasets

data()  # Data sets in package ‘datasets’

data(package = .packages(all.available = TRUE)) # list the data sets in all *available* packages

all_datasets <- data(package = "datasets")$results[, "Item"] # Counting Number of Built-in Datasets
length(all_datasets)

data(package = "ade4") 
library(ade4)
data(doubs) # load the dataset
head(doubs) # Checking the Structure of the Dataset
str(doubs)
colnames(doubs$env) # Checking Column Names

# 2) manipulation with tidyverse 
library("tidyverse")  # load the tidyverse packages, incl. dplyr

# A) select(), filter(), mutate(), and pipe using dplyr

# use pipes for manipulating data
doubs$env %>% 
  select(dfs, alt, oxy) %>%   # select dfs, alt, pen
  filter(alt > 300)  # filter alt > 300 

# use mutate() to create a new column
doubs$env %>% 
  filter(!is.na(oxy)) %>% 
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>% 
  head()

# Split-apply-combine data analysis
doubs$env %>% 
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>% 
  group_by(oxygen_category) %>% 
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), 
            .groups = "drop") 

# summary the above steps as follows

doubs$env %>%  
  select(dfs, alt, oxy, pH) %>%  # select dfs, alt, oxy, pH
  filter(alt > 200) %>%   # keep the rows in which alt > 200
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) %>%   
  rename(distance = dfs, oxygen = oxy) %>%    # rename
  arrange(desc(alt)) %>%  # order by alt decrease
  group_by(oxygen_category) %>%  # groups by oxy
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), 
            .groups = "drop")  

# B) Reshaping doubs between long and wide format
# a. using with tidyr::gather and spread

long_env <- doubs$env |> # from wide format to long 
  gather(key = "variable", value = "value", 
         -dfs) # keep dfs column

head(long_env)

# reverse the gather() operation
wide_env <- long_env |> # Convert back to wide format
  spread(key = "variable", value = "value")

head(wide_env)

# b. using pivot_longer() and pivot_wider()

long_env_new <- doubs$env |> 
  pivot_longer(cols = -dfs, 
               names_to = "variable", 
               values_to = "value")

print(long_env_new, n =30)

wide_env_new <- long_env_new |> 
  pivot_wider(names_from = "variable", values_from = "value")


# C) Data visualization with ggplot2 

env <- doubs$env 

# Plotting scatter plot or a line
ggplot(data = env)
ggplot(data = env, 
       aes(x = alt, y = oxy)) # define aes
ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_point() # dot plots

ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_line() # draw a line 

# Assign plot to a variable and then add plot
basic_plot1 <- ggplot(data = env, 
                     aes(x = alt, y = oxy, 
                         color = dfs)) +
  geom_point()

basic_plot1

basic_plot2 <- ggplot(data = env, 
                      aes(x = alt, y = oxy, 
                          color = dfs)) +
  geom_point(color = "blue") #  add colors

basic_plot2


ggplot(data = env, 
       aes(x = alt, y = oxy, color = dfs)) +
  geom_line() # add colors


# Usually plots with white background
ggplot(data = env, aes(x = alt, y = oxy, color = dfs)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank())

# Customization with aes and title
my_plot <- 
  ggplot(doubs$env, aes(x = alt, y = oxy, 
                        color = dfs, size = dfs)) +
  geom_point(alpha = 0.8) +  
  scale_color_gradient(low = "blue", high = "red") +  # 颜色渐变
  labs(title = "alt vs oxygen",
       x = "Altitude",
       y = "Oxygen Level",
       color = "Distance from Source",
       size = "dfs") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5, # middle
                                  face = "bold"))

ggsave("results/name_of_file.png", 
       my_plot, width = 15, height = 10)

################################################
## 02- self-defined functions with for and apply
################################################

# 1) using for loop

y1 <- rnorm(10)
y2 <- rnorm(10) + 10
dat <- data.frame(y1, y2)
dat

result <- list() # define an empty list for saving result
for(i in 1:2){
  result[[i]] <- mean(dat[,i]) # calculating the average
}
result

# 2) using apply()

apply(dat, 2, mean)

################################################
## 03- inserting codes in text with rmarkdown
################################################

