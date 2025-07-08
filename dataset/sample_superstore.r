
# 1. Import the data
superstore <- read.csv("dataset/Sample - Superstore.csv", stringsAsFactors = FALSE)
# Verify it loaded correctly:
head(superstore)
summary(superstore)
str(superstore)

sales_vec <- superstore$Sales

subset_15 <- superstore[1:15, c("Order.ID", "Customer.Name", "Sales")]
# Inspect
subset_15

num_rows <- nrow(superstore)
num_cols <- ncol(superstore)

cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n")

# 1. Rows where Profit > 100
profit_over_100 <- superstore[superstore$Profit > 100, ]

# 2. Rows where Category == "Furniture" AND Sales > 500
furniture_big_sales <- superstore[
  superstore$Category == "Furniture" & superstore$Sales > 500,
]
head(furniture_big_sales)

# 3. Rows where Region == "West" AND Quantity > 5
west_high_qty <- superstore[
  superstore$Region == "West" & superstore$Quantity > 5,
]
head(west_high_qty)

# 1. Add Profit Margin = (Profit / Sales) * 100
superstore$Profit.Margin <- (superstore$Profit / superstore$Sales) * 100

# 2. Round Sales to 2 decimal places
superstore$Sales <- round(superstore$Sales, 2)

# 3. Remove the Postal Code column
#    read.csv() will have turned "Postal Code" into Postal.Code
superstore <- subset(superstore, select = -Postal.Code)

# Verify the changes
head(superstore)
str(superstore)


# 1. Check for missing values
#    Count missing per column
missing_per_col <- colSums(is.na(superstore))
print(missing_per_col)

any_na <- anyNA(superstore)
cat("Any missing values in the data frame? ", any_na, "\n\n")


# 3. Replace missing values in Sales with the column mean
#    First, compute the mean of Sales (excluding NAs)
mean_sales <- mean(superstore$Sales, na.rm = TRUE)

#    Method A: Base R replacement
superstore$Sales[is.na(superstore$Sales)] <- mean_sales

# 1. Group by Region and sum Sales & Profit
region_summary <- aggregate(
  cbind(Sales, Profit) ~ Region,
  data = superstore,
  FUN  = sum
)
print(region_summary)

# 2. Create Discount Level column
superstore$Discount.Level <- with(
  superstore,
  ifelse(Discount <= 0.2, "Low",
    ifelse(Discount <= 0.5, "Medium", "High")
  )
)
# Quick check
table(superstore$Discount.Level)
# 3. Sort the entire data frame by Sales descending
superstore_sorted <- superstore[order(-superstore$Sales), ]

# Inspect the top rows
head(superstore_sorted)