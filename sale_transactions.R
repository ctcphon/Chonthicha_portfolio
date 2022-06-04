# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("purrr")
# install.packages("data.table")


library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# import data
library(readr)
store <- read_csv("Desktop/Bootcamp/samsple_store.csv")
View(store)
dim(store) # 9,994 rows and 21 column

# Let’s me show some information of the data. 



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



###########  Question #################################

# How many customers in this database ?
# Top 10 of customers which we had sold the most? 
# What is product which we sales the most? 
# Arrang top 3 of product that we have most profit?
# Trend of 3 products in each year
# The state that we have most sales and what is the proportion of total sales?
# what is the most ship mode that customer choose ?
# how many group in segment and what is the most segment ?
# Trends of transaction?

###########################################################


######## clean and choose data for analysis ########

# I use as.Date function to convert columns order_date and ship_date to datetime.
view(store$ship_date)
class(store$ship_date)
?strptime

ship_date <- as.Date(store$ship_date, format = "%m/%d/%Y")
order_date <- as.Date(store$order_date, format = "%m/%d/%Y")

store1 <- store %>% separate(order_date, into = c("mont", "day", "year"), remove = F, convert = T)
view(store1)
names(store1)
store1 <- store1[c(1,2,3,5,4,6,7:24)] # new arrange column

str(store1)
summary(store1)
########### Show ######### Data description
#### Data table overview

#######################################################
# How many customers in this database ?
# Ans 783 


# Top 10 of customers which we had sold the most? 

customer_s <- store1 %>% 
  group_by(customer_name) %>%
  summarize(
    sum_sale = sum(sales),
    sum_profit = sum(profit),
    sum_discount = sum(discount),
    n=n()
    ) %>%
  arrange(desc(sum_sale)) 

# From this question, I compared sales, profit, discount and n (n is count transaction)that we have from individual customer top 10.
# we see The most sale customer doesn't mean we have most profit. 

# customer_name      sum_sale sum_profit sum_discount       n
# <chr>                 <dbl>      <dbl>        <dbl>      <int>
# 1 Sean Miller          25043.     -1981.          3.7    15
# 2 Tamara Chand         19052.      8981.          1.4    12
# 3 Raymond Buch         15117.      6976.          1.7    18
# 4 Tom Ashbrook         14596.      4704.          0.8    10
# 5 Adrian Barton        14474.      5445.          4.8    20
# 6 Ken Lonsdale         14175.       807.          5.8    29
# 7 Sanjit Chand         14142.      5757.          1.4    22
# 8 Hunter Lopez         12873.      5622.          0.2    11
# 9 Sanjit Engle         12209.      2651.          2.1    19
# 10 Christopher Conant   12129.      2177.         3.1    11








