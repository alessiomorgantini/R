#Author: Alessio Morgantini
# Assignment A1
# Jan 25th 2024


### Question 1###

library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

# Read the CSV file
data <- read.csv("/Users/alessiomorgantini/hult_class/personal folder/A1_assignment_data.csv", check.names=FALSE)

# Convert the Date column to a Date type
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Extract Year, Month, and Day of Week
data$Year <- year(data$Date)
data$Month <- month(data$Date)

# Extract Day of Week as numeric vector (1 = Sunday, 2 = Monday, ..., 7 = Saturday)
data$DayOfWeek <- wday(data$Date)

# Convert Day of Week to named factors
data$DayOfWeekLabel <- factor(data$DayOfWeek, levels = 1:7, labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Aggregate Sales Data with the correct column name
yearly_sales <- aggregate(`Sale (Dollars)` ~ Year, data, sum)
monthly_sales <- aggregate(`Sale (Dollars)` ~ Month, data, sum)
weekday_sales <- aggregate(`Sale (Dollars)` ~ DayOfWeek, data, sum)

# Plot Yearly Sales
ggplot(yearly_sales, aes(x=Year, y=`Sale (Dollars)`)) +
  geom_bar(stat="identity", fill="steelblue") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels with commas
  labs(title="Yearly Sales", x="Year", y="Total Sales")


# Plot Monthly Sales
ggplot(monthly_sales, aes(x=Month, y=`Sale (Dollars)`)) +
  geom_bar(stat="identity", fill="steelblue") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels with commas
  labs(title="Monthly Sales", x="Month", y="Total Sales")



# Print the yearly sales data
print(yearly_sales)

# Print the monthly sales data
print(monthly_sales)

### Question 2###

library(lubridate)
library(dplyr)
library(ggplot2)

# Read the CSV file into a data frame with a new name to avoid conflict
sales_data <- read.csv("/Users/alessiomorgantini/hult_class/personal folder/A1_assignment_data.csv", check.names=FALSE)

# Convert the Date column to a Date type
sales_data$Date <- as.Date(sales_data$Date, format="%Y-%m-%d")

# Make sure sales_data is a data frame
sales_data <- as.data.frame(sales_data)

# Now use dplyr functions on sales_data
store_sales <- sales_data %>%
  dplyr::group_by(`Store Number`) %>%
  dplyr::summarise(Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE), 
                   Transactions = n_distinct(`Invoice/Item Number`))


# Calculate the average sale per transaction for each store
store_sales <- store_sales %>%
  mutate(Average_Sale_Per_Transaction = Total_Sales / Transactions)

# Sort the stores by Total Sales to see the top-performing stores
top_stores_by_sales <- store_sales %>%
  arrange(desc(Total_Sales))

# View the top stores based on total sales
top_stores_by_sales

# Visualize the top N stores by total sales using a bar chart
top_n_stores <- 10  # You can change this to see more or fewer stores
ggplot(top_stores_by_sales[1:top_n_stores, ], aes(x=reorder(`Store Number`, Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis labels with commas
  labs(title=paste("Top", top_n_stores, "Stores by Total Sales"), x="Store Number", y="Total Sales") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))  # Rotate the x labels for readability

# Optionally, you can visualize the top N stores by total sales using a bar chart
top_n_stores <- 10  # You can change this to see more or fewer stores
ggplot(top_stores_by_sales[1:top_n_stores, ], aes(x=reorder(`Store Number`, Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title=paste("Top", top_n_stores, "Stores by Total Sales"), x="Store Number", y="Total Sales") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))  # Rotate the x labels for readability

# Visualize the average sale per transaction for the top N stores
ggplot(top_stores_by_sales[1:top_n_stores, ], aes(x=reorder(`Store Number`, Average_Sale_Per_Transaction), y=Average_Sale_Per_Transaction)) +
  geom_bar(stat="identity", fill="coral") +
  labs(title=paste("Top", top_n_stores, "Stores by Average Sale Per Transaction"), x="Store Number", y="Average Sale Per Transaction") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))  # Rotate the x labels for readability

# Define the number of top stores to display
top_n_stores <- 10  # Adjust this number to display more or fewer top stores

# Display the top N stores by total sales
top_stores_by_total_sales <- head(top_stores_by_sales, top_n_stores)
print(top_stores_by_total_sales)

# Display the top N stores by average sale per transaction
top_stores_by_avg_sale <- top_stores_by_sales %>%
  arrange(desc(Average_Sale_Per_Transaction)) %>%
  head(top_n_stores)
print(top_stores_by_avg_sale)

##### question 3#####

# Load the necessary libraries
library(dplyr)
library(ggplot2)

sales_data <- read.csv("/Users/alessiomorgantini/hult_class/personal folder/A1_assignment_data.csv", check.names=FALSE)

# Convert the 'Date' column to a date format if necessary
sales_data$Date <- as.Date(sales_data$Date, format="%Y-%m-%d")

# Ensure that sales_data is a data frame
sales_data <- as.data.frame(sales_data)

# Aggregate the total sales for each liquor category
category_sales <- sales_data %>%
  group_by(`Category Name`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
            Count = n())

# Calculate the average sales per product in each category
category_sales <- category_sales %>%
  mutate(Average_Sales = Total_Sales / Count)

# Sort the categories based on total sales to identify the most popular ones
category_sales <- category_sales %>%
  arrange(desc(Total_Sales))

# Visualize the data
ggplot(category_sales, aes(x=reorder(`Category Name`, Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(title="Total Sales by Liquor Category", x="Category", y="Total Sales")

# Display the category sales data
print(category_sales)

### question 4###

# Analyze Sales by Bottle Volume
sales_by_volume <- sales_data %>%
  group_by(`Bottle Volume (ml)`) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
            Count = n()) %>%
  arrange(desc(Total_Sales))

# Analyze Price Segment Analysis
# Define price segments based on the retail price of the bottles
# Here is an example of defining three price segments: Low, Medium, and High
sales_data$Price_Segment <- cut(sales_data$`State Bottle Retail`,
                                breaks = c(-Inf, 10, 20, Inf),
                                labels = c("Low", "Medium", "High"))

# Now, aggregate sales by price segment
sales_by_price_segment <- sales_data %>%
  group_by(Price_Segment) %>%
  summarise(Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
            Count = n()) %>%
  arrange(desc(Total_Sales))

# Calculate the total sales and number of transactions by bottle volume
sales_volume_summary <- sales_data %>%
  group_by(`Bottle Volume (ml)`) %>%
  summarise(
    Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
    Number_of_Transactions = n(),
    Average_Sale_Per_Transaction = Total_Sales / Number_of_Transactions
  ) %>%
  arrange(desc(Total_Sales))

# Print out the summary
print(sales_volume_summary)


# Adjust the x-axis to zoom in on a specific range of bottle volumes

ggplot(sales_by_volume, aes(x=`Bottle Volume (ml)`, y=Total_Sales)) +
  geom_bar(stat="identity", fill="steelblue", width = 30.0) +
  labs(title="Sales by Bottle Volume", x="Bottle Volume (ml)", y="Total Sales") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  xlim(0, 2000) # Set the limits of the x-axis to zoom in

print(sales_by_volume)

# Add a label for the total sales rounded to no decimal places
sales_by_price_segment <- sales_by_price_segment %>%
  mutate(Label = round(Total_Sales, 0))

# Create the plot with labels only for the top three segments
ggplot(sales_by_price_segment, aes(x=Price_Segment, y=Total_Sales)) +
  geom_bar(stat="identity", fill="coral") +
  geom_text(aes(label=ifelse(rank(-Total_Sales) <= 3, as.character(Label), "")), 
            vjust=-0.3, color="black", size=3.5) +
  labs(title="Sales by Price Segment", x="Price Segment", y="Total Sales") +
  theme(axis.text.x=element_text(angle=90, hjust=1))  # Rotate the x-axis labels for readability

### Question 5 ####


# Aggregate the total sales and volume sold by vendor
vendor_performance <- sales_data %>%
  group_by(`Vendor Name`) %>%
  summarise(
    Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
    Total_Bottles_Sold = sum(`Bottles Sold`, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Sales))

# Calculate the market share for each vendor (assuming you have total market sales and volume)
total_market_sales <- sum(sales_data$`Sale..Dollars.`, na.rm = TRUE)
total_market_volume <- sum(sales_data$`Volume.Sold..Liters.`, na.rm = TRUE)

vendor_performance <- vendor_performance %>%
  mutate(
    Sales_Market_Share = Total_Sales / total_market_sales,
    Volume_Market_Share = Total_Bottles_Sold / total_market_volume
  )

# Rank vendors by total sales and total volume sold
vendor_performance <- vendor_performance %>%
  mutate(
    Rank_by_Sales = rank(-Total_Sales),
    Rank_by_Volume = rank(-Total_Bottles_Sold)
  )

# View the vendor performance summary
print(vendor_performance)

### Question 6 ####

# Aggregate sales data by City
sales_by_city <- sales_data %>%
  group_by(City) %>%
  summarise(
    Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
    Transactions = n(),
    Average_Sale = Total_Sales / Transactions
  ) %>%
  arrange(desc(Total_Sales))

# Aggregate sales data by County
sales_by_county <- sales_data %>%
  group_by(County) %>%
  summarise(
    Total_Sales = sum(`Sale (Dollars)`, na.rm = TRUE),
    Transactions = n(),
    Average_Sale = Total_Sales / Transactions
  ) %>%
  arrange(desc(Total_Sales))

# Visualize the top 10 cities by sales
ggplot(head(sales_by_city, 10), aes(x=reorder(City, Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity", fill="skyblue") +
  theme(axis.text.x=element_text(angle=65, hjust=1)) +
  labs(title="Top 10 Cities by Total Sales", x="City", y="Total Sales (Dollars)")

# Visualize the top 10 counties by sales
ggplot(head(sales_by_county, 10), aes(x=reorder(County, Total_Sales), y=Total_Sales)) +
  geom_bar(stat="identity", fill="lightgreen") +
  theme(axis.text.x=element_text(angle=65, hjust=1)) +
  labs(title="Top 10 Counties by Total Sales", x="County", y="Total Sales (Dollars)")

# Print the top 10 cities by total sales
print(head(sales_by_city, 10))

# Print the top 10 counties by total sales
print(head(sales_by_county, 10))

#END