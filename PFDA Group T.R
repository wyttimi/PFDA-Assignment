# PFDA ASSIGNMENT GROUP T

# GROUP MEMBER'S NAME AND TP
#1 Wee Yue Tim, TP070811
#2 Yong Yung Hao, TP069044
#3 Chong Wen Kang, TP071658



# 2.0 DATA PREPARATION

# 2.1 DATA IMPORT
# Import dataset (edit the file path according to file location)
fileUrl = '/Users/ecloudvalley/Library/CloudStorage/OneDrive-AsiaPacificUniversityofTechnologyAndInnovation(APU)/APU/Degree/Sem1/Programming with data analytic/R Programming/PFDA-Assignment/Group Assignment/retail_data 1.csv'

# Read the csv file
df = read.csv(fileUrl)

# View the dataset
View(df)



# 2.2 DATA CLEANING
# Check structure of data set
str(df)

# Lowercase all column names
names(df) <- tolower(names(df))
str(df)

# Make all empty spaces = NA
# List of columns to clean
columns_to_clean <- c("ratings", "product_category", "age", "gender", "shipping_method",
                      "income", "customer_segment", "total_purchases", "total_amount",
                      "payment_method", "order_status", "transaction_id", "customer_id",
                      "name", "email", "phone", "address", "city", "state", "zipcode",
                      "country", "date", "year", "month", "time", "amount", "product_brand",
                      "product_type", "feedback", "products")

# Replace empty spaces " " to NA for all columns
df[cols_to_clean] <- lapply(df[cols_to_clean], function(x) replace(x, x == "", NA))

# Observe data NA values
#install.packages("DataExplorer")
library(DataExplorer)

plot_missing(df)

# For shipping method column
# Remove the hyphen from "Same-Day" and make it "Same Day"
df$shipping_method <- gsub("-", " ", df$shipping_method)


# 2.2.2 Handle Missing Values
# Show the number of NA values per column
colSums(is.na(df))

# Remove rows where Transaction_ID or Customer_ID is NA
df <- df[!is.na(df$transaction_id) & !is.na(df$customer_id), ]

# NA for transaction_id and customer_id column should be 0
colSums(is.na(df))

# Clean product_category using product_brand mapping
# Install and load required packages
#install.packages("VIM")
library(VIM)
library(dplyr)

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


# Median imputation for Numerical Variables
df$age[is.na(df$age)]                         <- median(as.numeric(df$age), na.rm = TRUE)
df$total_purchases[is.na(df$total_purchases)] <- median(as.numeric(df$total_purchases), na.rm = TRUE)
df$total_amount[is.na(df$total_amount)]       <- median(as.numeric(df$total_amount), na.rm = TRUE)
df$amount[is.na(df$amount)]                   <- median(as.numeric(df$amount), na.rm = TRUE)


# Direct imputations
# Flag as Unknown or -1
df$name[is.na(df$name)]         <- "Unknown"
df$email[is.na(df$email)]       <- "Unknown"
df$address[is.na(df$address)]   <- "Unknown"
df$zipcode[is.na(df$zipcode)]   <- -1
df$phone[is.na(df$phone)]       <- -1
df$time[is.na(df$time)]         <- "Unknown"
df$products[is.na(df$products)] <- "Unknown"
df$order_status[is.na(df$order_status)] <- "Unknown"
df$feedback[is.na(df$feedback)] <- "Unknown"


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



# Handle date, year, and month columns
# Convert 'date' to Date format
df$date <- as.Date(df$date, format = "%m/%d/%Y")

# Convert month names to numbers for imputation (ex:"January" to 1)
month_map <- setNames(1:12, month.name)
df$month_num <- month_map[df$month]

# Fill missing 'date' using available 'year' and 'month_num'
df$date[is.na(df$date) & !is.na(df$year) & !is.na(df$month_num)] <- as.Date(
  paste0(df$year[is.na(df$date) & !is.na(df$year) & !is.na(df$month_num)],
         "-",
         sprintf("%02d", df$month_num[is.na(df$date) & !is.na(df$year) & !is.na(df$month_num)]),
         "-01")
)

# Fill missing 'year' from 'date'
df$year[is.na(df$year) & !is.na(df$date)] <- as.integer(format(df$date[is.na(df$year) & !is.na(df$date)], "%Y"))

# Fill missing 'month' (character form) from 'date'
df$month[is.na(df$month) & !is.na(df$date)] <- month.name[as.integer(format(df$date[is.na(df$month) & !is.na(df$date)], "%m"))]

# Drop helper column
df$month_num <- NULL

colSums(is.na(df))


# Remove columns with NA remaining after imputations due to unknown refer column
df <- df[complete.cases(df[, c("city", "state", "country", "ratings", "month")]), ]

# All columns NA count should be 0
colSums(is.na(df))

# Observe cleaned data NA values
plot_missing(df)





# 3.0 DATA ANALYSIS
# 3.1 OBJECTIVE 1: 
# To study the impact of the shipping method towards the ratings given by customers. – Wee Yue Tim TP070811

# ANALYSIS 3.1.1:
# What is the distribution of satisfaction ratings across different shipping methods? (Descriptive Analysis)

# Load necessary libraries
library(ggplot2)
library(dplyr)

#1 Frequency Table: Count ratings by shipping method
freq_table <- df %>%
  count(shipping_method, ratings) %>% #Counts how many times each combination of shipping_method and ratings
  arrange(shipping_method) # Sorts the output by shipping method alphabetically

print(freq_table)


#2 Bar Plot: Distribution of ratings by shipping method
ggplot(df, aes(x = shipping_method, fill = ratings)) +
  geom_bar(position = "dodge") + # Places bars side by side for comparison
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(0.8), vjust = -0.5, size = 2.5, color = "#34495E") +
  labs(title = "Distribution of Ratings by Shipping Method",
       x = "Shipping Method", y = "Count") +
  scale_fill_manual(values = c("High" = "#1E90FF", "Low" = "#FF4500")) +  # Custom colors
  theme_minimal() + # Plot styles
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black", hjust = 0.5),  # Center title
    axis.title.x = element_text(size = 10, face = "bold", color = "black"),
    axis.title.y = element_text(size = 10, face = "bold", color = "black"),
    axis.text.x = element_text(size = 8, color = "#7F8C8D"),
    axis.text.y = element_text(size = 8, color = "#7F8C8D"),
    legend.title = element_text(size = 10, face = "bold", color = "black"),
    legend.text = element_text(size = 8, color = "#7F8C8D")
  )


#3 Summary Statistics: Count and percentage of ratings per shipping method
summary_stats <- df %>%
  group_by(shipping_method, ratings) %>% # Groups the dataset by shipping_method and ratings
  summarise(count = n()) %>% # Counts how many of each rating
  mutate(percentage = (count / sum(count)) * 100) # Calculates percentage of each rating within that shipping method

print(summary_stats)



# ANALYSIS 3.1.2
# ANALYSIS 3.1.3
# ANALYSIS 3.1.4

