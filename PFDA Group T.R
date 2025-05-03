# PFDA ASSIGNMENT GROUP T

# GROUP MEMBER'S NAME AND TP
#1 Wee Yue Tim, TP070811
#2 Yong Yung Hao, TP069044
#3 Chong Wen Kang, TP071658

# Install and load required packages
#install.packages("DataExplorer")
#install.packages("VIM")
library(DataExplorer)
library(VIM)
library(dplyr)
library(ggplot2)

# 2.0 DATA PREPARATION

# 2.1 DATA IMPORT
# Import dataset (edit the file path according to file location)
fileUrl = 'D:\\User\\Documents\\APU\\Degree\\Y2S1\\PFDA\\Assignment\\retail_data 1.csv'

# Read the csv file
df = read.csv(fileUrl)

# View the dataset
#View(df)

# 2.2 DATA CLEANING
# 2.2.1 Convert Data Type
# Check structure of data set
str(df)

# Lowercase all column names
names(df) <- tolower(names(df))
str(df)

# 2.2.1 Convert Data Type
# Phone - from numeric to character
df$phone <- as.character(df$phone)

# Date
df$date <- as.Date(df$date, format = "%m/%d/%Y")

# Convert month names to numbers
df$month <- match(df$month, month.name)

# Round numeric columns to 2 decimal
df$total_amount     <- round(df$total_amount, 2)
df$amount           <- round(df$amount, 2)

str(df)

# 2.2.2 Handle Missing Values

# Make all empty spaces = NA
# List of columns to clean
cols_to_clean <- c("ratings", "product_category", "age", "gender", "shipping_method",
                   "income", "customer_segment", "total_purchases", "total_amount",
                   "payment_method", "order_status", "transaction_id", "customer_id",
                   "name", "email", "phone", "address", "city", "state", "zipcode",
                   "country", "date", "year", "month", "time", "amount", "product_brand",
                   "product_type", "feedback", "products")

# Replace empty spaces " " to NA for all columns
df[cols_to_clean] <- lapply(df[cols_to_clean], function(x) replace(x, x == "", NA))

# Observe data NA values
plot_missing(df)

# For shipping method column
# Remove the hyphen from "Same-Day" and make it "Same Day"
df$shipping_method <- gsub("-", " ", df$shipping_method)

# Show the number of NA values per column
colSums(is.na(df))

# CLEAN Transaction_ID or Customer_ID
# Remove rows where Transaction_ID or Customer_ID is NA
df <- df[!is.na(df$transaction_id) & !is.na(df$customer_id), ]

# NA for transaction_id and customer_id column should be 0
colSums(is.na(df))


# CLEAN product_category using product_brand mapping

# Create a look-up table from non-missing values
#1 Filter column without missing values
#2 Get the most common category for each brand
brand_category_map <- df %>%
  filter(!is.na(product_brand) & !is.na(product_category)) %>%
  group_by(product_brand) %>%
  summarise(common_category = names(sort(table(product_category), decreasing = TRUE))[1])

# Save original category for comparison
df$product_category_before <- df$product_category

# Merge this info into the original data
df <- left_join(df, brand_category_map, by = "product_brand")

# Fill in missing product_category using the most common one
df$product_category[is.na(df$product_category)] <- df$common_category[is.na(df$product_category)]

# Check the category filled by 
df[c("product_brand", "product_category_before", "product_category")]

# Remove the helper column
df$common_category <- NULL
df$product_category_before <- NULL

# NA for product_category column should be 0
colSums(is.na(df))


# CLEAN date, year, and month columns
# Fill missing 'date' using available 'year' and 'month'
df$date[is.na(df$date) & !is.na(df$year) & !is.na(df$month)] <- as.Date(
  paste0(df$year[is.na(df$date) & !is.na(df$year) & !is.na(df$month)],
         "-",
         sprintf("%02d", df$month[is.na(df$date) & !is.na(df$year) & !is.na(df$month)]),
         "-01")
)

# Fill missing 'year' from 'date'
df$year[is.na(df$year) & !is.na(df$date)] <- as.integer(format(df$date[is.na(df$year) & !is.na(df$date)], "%Y"))

# Fill missing 'month' (integer form) from 'date'
df$month[is.na(df$month) & !is.na(df$date)] <- as.integer(format(df$date[is.na(df$month) & !is.na(df$date)], "%m"))

# Drop helper column if it exists (for consistency)
df$month_num <- NULL

# CLEAN Other Columns
# Median imputation for Numerical Variables
df$age[is.na(df$age)]                         <- median(df$age, na.rm = TRUE)
df$total_purchases[is.na(df$total_purchases)] <- median(df$total_purchases, na.rm = TRUE)
df$total_amount[is.na(df$total_amount)]       <- median(df$total_amount, na.rm = TRUE)
df$amount[is.na(df$amount)]                   <- median(df$amount, na.rm = TRUE)

# Direct imputations
# Flag as Unknown or -1
df$name[is.na(df$name)]                 <- "Unknown"
df$email[is.na(df$email)]               <- "Unknown"
df$address[is.na(df$address)]           <- "Unknown"
df$zipcode[is.na(df$zipcode)]           <- -1
df$phone[is.na(df$phone)]               <- "Unknown"
df$time[is.na(df$time)]                 <- "Unknown"
df$products[is.na(df$products)]         <- "Unknown"
df$order_status[is.na(df$order_status)] <- "Unknown"
df$feedback[is.na(df$feedback)]         <- "Unknown"

# Hot deck imputation
# Gender, income, customer segment (by age)
df <- hotdeck(df, variable = c("gender", "income", "customer_segment"), domain_var = "age", imp_var = FALSE)

# Location fields (by zipcode)
df <- hotdeck(df, variable = c("city", "state", "country"), domain_var = "zipcode", imp_var = FALSE)

# Shipping & payment method (by country)
df <- hotdeck(df, variable = c("shipping_method", "payment_method"), domain_var = "country", imp_var = FALSE)

# Product brand and type (by product category)
df <- hotdeck(df, variable = c("product_brand", "product_type"), domain_var = "product_category", imp_var = FALSE)

# Ratings (by feedback)
df <- hotdeck(df, variable = "ratings", domain_var = "feedback", imp_var = FALSE)

# Remove columns with NA remaining after imputations due to unknown refer column
df <- df[complete.cases(df[, c("city", "state", "country", "ratings", "month")]), ]

# All columns NA count should be 0
colSums(is.na(df))

# Observe cleaned data NA values
plot_missing(df)

#2.2.3 Handling Outliers
# Step 1: Identify numeric columns
str(df)

# Step 2: Select all numeric columns where outliers may appeared
num_col = c("age","total_purchases","amount","total_amount")

# Step 3: Visualize the data using boxplot to spot outliers
for (col in num_col) {
  print(ggplot(df, aes_string(x = col)) + 
          geom_boxplot(fill = "lightblue",outlier.color="red"))
}

# Only spot outlier after Q3 at total_amount
# Step 4: Replace outlier with upper limit
q1 = quantile(df$total_amount,0.25)
q3 = quantile(df$total_amount,0.75)
IQR = q3-q1
up = q3 + 1.5 * IQR
df$total_amount = replace(df$total_amount,df$total_amount>up,up)
up
# Step 5: Check boxplot again to ensure outlier is replaced
ggplot(df,aes(x=total_amount)) + 
  geom_boxplot(fill = "lightblue",outlier.color = "red")

#2.3 Data Export
# Export the cleaned data into csv file format
write.csv(df,"D:\\User\\Documents\\APU\\Degree\\Y2S1\\PFDA\\Assignment\\cleaned_data.csv")
