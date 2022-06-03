# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("purrr")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

# import data
library(readr)
store <- read_csv("Desktop/Bootcamp/samsple_store.csv")
View(store)
dim(store) # 9,994 rows and 21 column

head(store)

####### table  example data


# for more information
str(store)

######### example 


names(store)

# I  will change name to easier and start with lowercase
names(store)[names(store) == "Row ID"] <- "row_id"
names(store)[names(store) == "Order ID"] <- "order_id"
names(store)[names(store) == "Order Date"] <- "order_date"
names(store)[names(store) == "Ship Date"] <- "ship_date"
names(store)[names(store) == "Ship Mode"] <- "ship_mode"
names(store)[names(store) == "Customer ID"] <- "customer_id"
names(store)[names(store) == "Customer Name"] <- "customer_nam



###########  Question ############################
# How many customers in this database ?
# Top 10 of customers which we had sold the most? 
# What is product which we sales the most? 
# Arrang top 3 of product that we have most profit?
# Trend of 3 products in each year
# The state that we have most sales and what is the proportion of total sales?
# what is the most ship mode that customer choose ?
# how many group in segment and what is the most segment ?
# Trends of transaction?





