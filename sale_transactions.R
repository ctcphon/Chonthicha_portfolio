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


head(store) # 4 ID and 17 variables

# A tibble: 6 × 21
#  row_id order_id       order_date ship_date  ship_mode      customer_id
   <dbl> <chr>          <chr>      <chr>      <chr>          <chr>      
# 1      1 CA-2016-152156 11/8/2016  11/11/2016 Second Class   CG-12520   
# 2      2 CA-2016-152156 11/8/2016  11/11/2016 Second Class   CG-12520   
# 3      3 CA-2016-138688 6/12/2016  6/16/2016  Second Class   DV-13045   
# 4      4 US-2015-108966 10/11/2015 10/18/2015 Standard Class SO-20335   
# 5      5 US-2015-108966 10/11/2015 10/18/2015 Standard Class SO-20335   
# 6      6 CA-2014-115812 6/9/2014   6/14/2014  Standard Class BH-11710   
# … with 15 more variables: customer_name <chr>, segment <chr>,
#   country <chr>, city <chr>, state <chr>, postal_code <chr>,
#   region <chr>, product_id <chr>, category <chr>, sub_category <chr>,
#   product_name <chr>, sales <dbl>, quantity <dbl>, discount <dbl>,
#   profit <dbl>


str(store)  # for more information
summary(store)
mean(store$sales) # 229.858
mean(store$profit) # 28.6569

names(store)

# I  will change name to easier and start with lowercase
names(store)[names(store) == "Row ID"] <- "row_id"
names(store)[names(store) == "Order ID"] <- "order_id"
names(store)[names(store) == "Order Date"] <- "order_date"
names(store)[names(store) == "Ship Date"] <- "ship_date"
names(store)[names(store) == "Ship Mode"] <- "ship_mode"
names(store)[names(store) == "Customer ID"] <- "customer_id"
names(store)[names(store) == "Customer Name"] <- "customer_name"
names(store)[names(store) == "Segment"] <- "segment"
names(store)[names(store) == "Country"] <- "country"
names(store)[names(store) == "City"] <- "city"
names(store)[names(store) == "State"] <- "state"
names(store)[names(store) == "Postal Code"] <- "postal_code"
names(store)[names(store) == "Region"] <- "region"
names(store)[names(store) == "Product ID"] <- "product_id"
names(store)[names(store) == "Category"] <- "category"
names(store)[names(store) == "Sub-Category"] <- "sub_category"
names(store)[names(store) == "Product Name"] <- "product_name"
names(store)[names(store) == "Sales"] <- "sales"
names(store)[names(store) =="Quantity"] <- "quantity"
names(store)[names(store) == "Discount"] <- "discount"
names(store)[names(store) == "Profit"] <- "profit"


###########  Question ############################

# Q1: Which year has the most sales ?
# Q2: In 2017, show percentage of sale by month
# Q3: How many customers in this database ?
# Q4: Top 10 of customers which we had sold the most?
# Q5: Where is region and what is segment that we have most customer?
# Q6: What is product which we sales the most? 
# Q7: Arrang top 3 of product that we have most profit?
# Q8: The state in 2017 that we have most sales and what is the proportion of total sales?
# Q9: In California state, what product is the most profit for this store ?
# Q10: What is the most ship mode that customer choose ?

##################################################



######## prepare data for analysis ########

# I use as.Date function to convert columns order_date and ship_date to datetime.
view(store$ship_date)
class(store$ship_date)
?strptime

ship_date <- as.Date(store$ship_date, format = "%m/%d/%Y")
order_date <- as.Date(store$order_date, format = "%m/%d/%Y")

# I wanna know more about year and month(from order date)for count unique value as a new column.

store$month_year <- format(as.Date(
 store$order_date, format = "%m/%d/%Y"), "%Y-%m") 


# I counting unique customer in each Month-Year
group_by_month <- 
  store %>%                             
  group_by(month_year) %>%
  summarise(unique_customer = n_distinct(customer_id))
group_by_month

# A tibble: 48 × 2
#  month_year unique_customer
#   <chr>                <int>
# 1 2014-01                 32
# 2 2014-02                 27
# 3 2014-03                 69
# 4 2014-04                 64
# 5 2014-05                 67
# 6 2014-06                 63
# 7 2014-07                 65
# 8 2014-08                 70
# 9 2014-09                118
# 10 2014-10                 75
# … with 38 more rows

# if we see that we can know about seasonal or which month has a lot of customers.
# more than this we can calculate multiple varible by group. 

library(data.table)

group_by_month_subcat <-
  data.table(store)[,  list(unique_customer = n_distinct(customer_id),
                          number_of_entries = .N,unique_product = n_distinct(product_name)),
                  by=month_year]
group_by_month_subcat %>%  arrange(desc(unique_product))
head(group_by_month_subcat, 8)
# Example results 
#   month_year unique_customer number_of_entries unique_product
# 1:    2016-11             165               370            337
# 2:    2016-06              90               199            186
# 3:    2015-10              79               166            154
# 4:    2014-06              63               135            130
# 5:    2017-04             109               203            192
# 6:    2016-12             157               352            314
# 7:    2015-11             146               324            296
# 8:    2014-11             139               318            290



########################## Answer ########################################

# Q1: Which year has the most sales 
## Ans 2017 (sum = $733,215)
store1 <- store %>% separate(order_date, into = c("month", "day", "year"), remove = F, convert = T)

y_sale <- store1 %>%
  group_by(year) %>%
  summarise(
    sum_sale = sum(sales)
  ) %>%
  arrange(desc(sum_sale))
  
# y_sale 
# A tibble: 4 × 2
#   year sum_sale    
#  <int>    <dbl> 
# 1  2017  733215. 
# 2  2016  609206. 
# 3  2014  484247. 
# 4  2015  470533.  

# Q2: In 2017, show percentage of sale by month (November has the most sale in 2017)
my_sale <- store1 %>%
  filter(year == "2017") %>%
  group_by(month_year) %>%
  summarise(
    sum_sale = sum(sales),
    percen_sale = (sum_sale/733215)*100
  )
# A tibble: 12 × 3
#   month_year sum_sale percen_sale
#   <chr>         <dbl>       <dbl>
# 1 2017-01      43971.        6.00
# 2 2017-02      20301.        2.77
# 3 2017-03      58872.        8.03
# 4 2017-04      36522.        4.98
# 5 2017-05      44261.        6.04
# 6 2017-06      52982.        7.23
# 7 2017-07      45264.        6.17
# 8 2017-08      63121.        8.61
# 9 2017-09      87867.       12.0 
# 10 2017-10      77777.       10.6 
# 11 2017-11     118448.       16.2 
# 12 2017-12      83829.       11.4 
  

# Q3: How many customers in this database ?
## Ans 783 
store1 %>% 
  count(customer_name)


# Q4: Top 10 of customers which we had sold the most? 
cn_sale <- store1 %>% 
  group_by(customer_name) %>%
  summarize(
    sum_sale = sum(sales),
    sum_profit = sum(profit),
    sum_discount = sum(discount),
    n=n(),
  ) %>%
  arrange(desc(sum_sale)) 

# From this question, I compared sales, profit, discount and n (n is count transaction)that we have from individual customer top 10.
# we see The most sale customer doesn't mean we have most profit. 

## Ans

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


# Q5: where is region and what is segment that we have most customer?
## Ans West and cunsumer segment

# display customer segment by region
table(store1$region, store1$segment) 
#          Consumer Corporate Home Office
#  Central     1212       673         438
#  East        1469       877         502
#  South        838       510         272
#  West        1672       960         571


# Q6: What is product which we sales the most? 
## Ans The product is Canon imageCLASS 2200 Advanced Copier in Technology

product_sale <- store1 %>% 
  group_by(product_id) %>%
  summarize(
    sum_sale = sum(sales),
    sum_profit = sum(profit),
    n=n()
  )
product_sale %>% arrange(desc(sum_sale))

# product_sale %>% arrange(desc(sum_sale)) 
# A tibble: 1,862 × 4
#   product_id      sum_sale sum_profit     n
#   <chr>              <dbl>      <dbl> <int>
# 1 TEC-CO-10004722   61600.   2.52e+ 4     5

most_product <- store1 %>% filter(product_id == "TEC-CO-10004722")
most_product$product_name


# Q7: Arrang top 3 of product that we have most profit?
## Ans:[1] "Hewlett Packard LaserJet 3310 Copier"                                       
#      [2] "Fellowes PB500 Electric Punch Plastic Comb Binding Machine with Manual Bind"
#      [3] "Canon imageCLASS 2200 Advanced Copier" 

product_profit <- store1 %>% 
  group_by(product_id) %>%
  summarize(
    sum_profit = sum(profit),
    n=n()
  )
product_profit %>% arrange(desc(sum_profit)) 

# A tibble: 1,862 × 3
#   product_id      sum_profit     n
#   <chr>                <dbl> <int>
# 1 TEC-CO-10004722     25200.     5
# 2 OFF-BI-10003527      7753.    10
# 3 TEC-CO-10001449      6984.     8

product_profit %>% arrange(desc(sum_profit)) 
mostp_profit <- store1 %>% filter(product_id == "TEC-CO-10004722"|
                                    product_id == "OFF-BI-10003527"|
                                    product_id == "TEC-CO-10001449")
mostp_profit1 <- unique(mostp_profit$product_name)


# Q8: The state in 2017 that we have most sales and what is the proportion of total sales?
## Ans California and the proportion is 0.2

state <- as.factor(store1$state)
table(state)
unique(store1$state) # 49 state

store1 %>% filter(year==2017) %>%
  summarise(sum_sale = sum(sales)) # 733215

state_sale <- store1 %>%
  filter(year == 2017) %>%
  group_by(state) %>%
  summarise(
    sum_state_sale = sum(sales),
    percent_sale = (sum_state_sale/733215)*100
  ) %>% 
  arrange(desc(proportion_sale))
state_sale

# A tibble: 47 × 3
#   state          sum_state_sale     percent_sale
#   <chr>               <dbl>           <dbl>
# 1 California        146388.           20.0 
# 2 New York           93923.           12.8 
# 3 Washington         65540.            8.94
# 4 Texas              43422.            5.92
# 5 Pennsylvania       42688.            5.82
# 6 Florida            26445.            3.61
# 7 Michigan           25834.            3.52
# 8 Illinois           24352.            3.32
# 9 North Carolina     23457.            3.20
#10 Ohio               23265.            3.17
# … with 37 more rows

# Q9: In California state, what product is the most profit for this store ?
## Ans: Fellowes PB500 Electric Punch Plastic Comb Binding Machine with Manual Bind($2288) in Office Supplies category

state <- as.factor(store1$state)
  store1 %>% 
    filter(state == "California") %>%
    group_by(product_id)%>%
    summarise(profit = sum(profit),
              n=n()) %>%
    arrange(desc(profit))
  
mostp_cari <- store %>% 
  filter(product_id == "OFF-BI-10003527") %>%
  select(product_name, category, sub_category)
unique(mostp_cari) 
view(mostp_cari)

# Q10: What is the most ship mode that customer choose?
## Ans: Standard class

shipmode <- store1$ship_mode  
table(shipmode)    

#   First Class       Same Day   Second Class  Standard Class 
#          1538            543           1945           5968 




