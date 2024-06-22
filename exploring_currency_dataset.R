# Title: Exploring the Ethiopian Currency Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 19th, 2024


# Reading the dataset
x <- read.csv("USD_ETB_Historical_Data_By_Month.csv")
dim(x)  # 350 observations, 7 columns
str(x)
head(x)

# Date column
sum(is.na(x$Date)) # No NAs
class(x$Date) # Character
x$Date <- as.Date(x$Date, format = "%m/%d/%Y")
class(x$Date) # Date
sum(is.na(x$Date)) # Still no NAs
range(x$Date) # 1995-01-01 to 2024-01-03

# Price column
sum(is.na(x$Price)) # No NAs
class(x$Price) # Numeric - correct
range(x$Price) # 5.42 to 56.7485, a large change over time I suspect

str(x)
head(x)

# Converting the Date to a year-month format so that it is consistent with the
# other datasets
# Also creating a year column
x$year_month <- format(x$Date, "%Y-%m")
x$year <- format(x$Date, "%Y")
head(x)

# Selecting the relevant columns for further analysis in combination with the
# other datasets
# Vol. and Change.. (would be redundant information) are not included
x <- x[, c("year", 
           "year_month",
           "Price",
           "Open",
           "High",
           "Low")]

str(x) # a 350 x 6 table
head(x)

# Exporting it into a CSV file so that it can be loaded and combined with other
# datasets
write.csv(x, "cr_monthly.csv", row.names = FALSE)
