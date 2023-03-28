# Adidas Sales Dataset from Kaggle
# https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset

library(ggplot2)
library(forecast)
library(readxl)
library(zoo)
library(tidyverse)
library(dplyr)
library(TSstudio)

Adidas_US_Sales_Datasets <- read_excel("Adidas US Sales Datasets.xlsx")

adidas_df <- Adidas_US_Sales_Datasets
#check the data
head(adidas_df)
# see some rows are not helpful
adidas_df <- adidas_df[-2,]
adidas_df <- adidas_df[-1,]
head(adidas_df)
# This ends the first row as the column names so we will replace the colnames
# with the REAL colnames
colnamesREAL <- c("Retailer", "Retailer_ID", "Invoice_Date", "Region", "State", "City", "Product", "Price_Per_Unit", "Units_Sold",
                  "Total_Sales", "Operating_Profit", "Operating_Margin", "Sales_Method")
colnamesREAL
adidas_df <- adidas_df[-1,]
colnames(adidas_df) <- colnamesREAL
colnames(adidas_df)
head(adidas_df)
#The types for each row are all chr but some should be num
# Price per, units sold, total sales, operating profit, operating margin
adidas_df$Price_Per_Unit <- as.numeric(adidas_df$Price_Per_Unit)
adidas_df$Units_Sold <- as.numeric(adidas_df$Units_Sold)
adidas_df$Total_Sales <- as.numeric(adidas_df$Total_Sales)
adidas_df$Operating_Profit <- as.numeric(adidas_df$Operating_Profit)
adidas_df$Operating_Margin <- as.numeric(adidas_df$Operating_Margin)

adidas_df$Invoice_Date <- as.numeric(adidas_df$Invoice_Date)
adidas_df$Invoice_Date <- as.Date(adidas_df$Invoice_Date, origin = "1899-12-30")

head(adidas_df)
tail(adidas_df)
#See if there are any N/A values in dataframe. it returns 0 so there are
# no N/A s
sum(is.na.data.frame(adidas_df))

adidas_df <- adidas_df[order(adidas_df$Invoice_Date),]
head(adidas_df)

##############################################################################################################
#Here we look at the distribution of certain Variables. To see where the
# Most product is moved to and sold.
table(adidas_df$Retailer)
table(adidas_df$Region)
table(adidas_df$State)
table(adidas_df$City)
table(adidas_df$Product)
table(adidas_df$Sales_Method)
table(adidas_df$Invoice_Date)
#Visuals for basic distributions
# Retailers
ggplot(adidas_df, aes(x = Retailer, color = Retailer, fill = Retailer)) + 
  geom_bar() +
  ggtitle("Retailers") + 
  ylab("Number of Sales") +
  theme(legend.position = "none")  
# Regional Sales
ggplot(adidas_df, aes(x = Region, color = Region, fill = Region)) +
  geom_bar() +
  ggtitle("Regional Sales") + 
  ylab("Number of Sales") +
  theme(legend.position = "none")
ggplot(adidas_df, aes(x = Product, color = Product, fill = Product)) + 
  geom_bar() + 
  ggtitle("Products Sold") +
  theme(axis.text.x = element_text(size = 6.7, angle = 90)) +
  ylab("Number of Sales")
## Products sold is not a mistake graph the number of sales 
## by product are all fairly even in count. look at the table.
ggplot(adidas_df, aes(x = Sales_Method, color = Sales_Method, fill = Sales_Method)) + 
  geom_bar() +
  ggtitle("Sales Method Distribution") +
  xlab("Sales Method")+
  ylab("Number of Sales")+
  theme(legend.position = "none")
######################################################################
#In sales quarters are important because sales are fluid in which the volumes of sales can differ from 
# each quarter. For example sales can be much higher in Q4 because of popular holidays and Q1 could be slow as a result.
# In our data set and splitting the data between each quarter for 2020 and 2021. 
# We also see that 2020 had a lot less sales than 2021. I wonder what happened?


Q1_2020 <- adidas_df %>% 
  filter(between(Invoice_Date, as.Date("2020-01-01"), as.Date("2020-03-31")))
Q2_2020 <- adidas_df %>% 
  filter(between(Invoice_Date, as.Date("2020-04-01"), as.Date("2020-06-30")))
Q3_2020 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2020-07-01"), as.Date("2020-09-30")))
Q4_2020 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2020-10-01"), as.Date("2020-12-31")))
Total_2020 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2020-01-01"), as.Date("2020-12-31")))

Q1_2021 <- adidas_df %>% 
  filter(between(Invoice_Date, as.Date("2021-01-01"), as.Date("2021-03-31")))
Q2_2021 <- adidas_df %>% 
  filter(between(Invoice_Date, as.Date("2021-04-01"), as.Date("2021-06-30")))
Q3_2021 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2021-07-01"), as.Date("2021-09-30")))
Q4_2021 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2021-10-01"), as.Date("2021-12-31")))
Total_2021 <- adidas_df %>%
  filter(between(Invoice_Date, as.Date("2021-01-01"), as.Date("2021-12-31")))
# Q1 in 2020 and 2021
ts_plot(Q1_2020)
ts_plot(Q1_2021)
# Q2 in both years
ts_plot(Q2_2020)
ts_plot(Q2_2021)
# Q3 
ts_plot(Q3_2020)
ts_plot(Q3_2021)
# Q4
ts_plot(Q4_2020)
ts_plot(Q4_2021)
