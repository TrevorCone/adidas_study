# Adidas Sales Dataset from Kaggle
# https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset


library(readxl)
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
##############################################################################################################
