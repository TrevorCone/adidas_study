# Adidas Sales Dataset from Kaggle
# https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset
library(ggplot2)
library(forecast)
library(readxl)
library(tidyverse)
library(leaps)
#Read in excel data
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
#Remove Retailer_ID
adidas_df <- adidas_df[-2]
head(adidas_df)
#The types for each row are all chr but some should be num
# Price per, units sold, total sales, operating profit, operating margin
adidas_df$Price_Per_Unit <- as.numeric(adidas_df$Price_Per_Unit)
adidas_df$Units_Sold <- as.numeric(adidas_df$Units_Sold)
adidas_df$Total_Sales <- as.numeric(adidas_df$Total_Sales)
adidas_df$Operating_Profit <- as.numeric(adidas_df$Operating_Profit)
adidas_df$Operating_Margin <- as.numeric(adidas_df$Operating_Margin)
adidas_df$Invoice_Date <- as.numeric(adidas_df$Invoice_Date)
adidas_df$Invoice_Date <- as.Date(adidas_df$Invoice_Date, origin = "1899-12-31")

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
#Visuals for basic distributions of counts for products sold in each category.
# This graph shows the number of sales for each retailer. From the graph Foot Locker has the most sales of adidas products. 
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
#Sales by state
ggplot(adidas_df, aes(y = State, color = State, fill = State)) +
  geom_bar(width = 0.5, position = position_dodge(0.7)) +
  ggtitle("State Sales") + 
  xlab("Number of Sales") +
  theme(legend.position = "none")
#City sales
ggplot(adidas_df, aes(y = City, color = City, fill = City)) +
  geom_bar(width = 0.5, position = position_dodge(0.7))+
  ggtitle("Top Ten Cities by Sales")+
  xlab("Number of Sales")+
  theme(legend.position = "none")
#Products sold. 
## Products sold is not a mistake graph the number of sales 
ggplot(adidas_df, aes(x = Product, color = Product, fill = Product)) + 
  geom_bar() + 
  ggtitle("Products Sold") +
  theme(axis.text.x = element_text(size = 6.7, angle = 90)) +
  ylab("Number of Sales")
#Sales Method
ggplot(adidas_df, aes(x = Sales_Method, color = Sales_Method, fill = Sales_Method)) + 
  geom_bar() +
  ggtitle("Sales Method Distribution") +
  xlab("Sales Method")+
  ylab("Number of Sales")+
  theme(legend.position = "none")
#Total Sales
ggplot(adidas_df, aes(x = Invoice_Date, y = Total_Sales, color = Total_Sales)) +
  geom_jitter() + 
  theme(axis.text.x = element_text(size = 6.7, angle = 90)) +
  ylab("Total Sales")
ggplot(adidas_df, aes(x = Invoice_Date, y = Operating_Profit, color = Total_Sales)) +
  geom_jitter() + 
  theme(axis.text.x = element_text(size = 6.7, angle = 90)) +
  ylab("Operating Profit")
###########################################################################################################
## When we look at the two invoice date graphs there is significant
## jump of number of sales between 2020 and 2021. I believe there was a event outside of the control of many of us
## that affected these sales. (COVID)
## in addition, working in sales you know sales can be very different based on the time of the year.
## In retail clothing obviously sales become stronger towards the end of the year for the holidays
## To try and predict sales based on this data set I believe we need to subset
## based on quarters. 
Adidas2021 <- adidas_df %>% 
  filter(Invoice_Date >= as.Date("2021-01-01") & Invoice_Date <= as.Date("2021-12-31"))
#looking at the summary we can see that the bulk of the original data is in 2021 so I believe it will be ok to make predictions based on 
# 2021 only. Where we can have better data to work with.  
summary(Adidas2021)
#Finding linearity of variables using Total_sales our dependent VAriable (y) This can give us an Idea of what would be a good predictor.
ggplot(Adidas2021, aes(x = Price_Per_Unit, y = Total_Sales))+ geom_point() 
ggplot(Adidas2021, aes(x = Units_Sold, y = Total_Sales))+ geom_point()
ggplot(Adidas2021, aes(x = Operating_Profit, y = Total_Sales))+ geom_point()
ggplot(Adidas2021, aes(x = Operating_Margin, y = Total_Sales))+ geom_point()
#### Building Predictive models using lm
# Using Total sales as dependent, and Units sold, operating profit, operating margin, city and state as independent variables
AdidasModel1 <- lm(Total_Sales ~ Units_Sold + Price_Per_Unit, data = Adidas2021)
summary(AdidasModel1)
PredictedSales1 <- data.frame(Sales_Prediction = predict(AdidasModel1, Adidas2021), 
                             actualTotal_sales = Adidas2021$Total_Sales,
                             difference = PredictedSales$Sales_Prediction - PredictedSales$actualTotal_sales
)
head(PredictedSales1)
#plotting predicted sales v actual sales from 2021 to see our model is fairly close to what we had. 
AdidasModel1_plot <- ggplot(PredictedSales2, aes(x = Sales_Prediction, y = actualTotal_sales, color = difference)) + 
  geom_point() + 
  geom_abline() + 
  ggtitle("Actual v Predicted Sales")+
  xlab("Actual Sales 2021") +
  ylab("Predicted Sales")
AdidasModel1_plot

