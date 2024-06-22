# Title: Exploring the Ethiopian Rainfall Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 19th, 2024


# Reading the dataset
x <- read.csv("eth_rainfall.csv")
dim(x)  # 38,376 entries, 7 columns
str(x)
head(x)

# X column, seems like IDs
sum(is.na(x$X)) # No NAs
length(unique(x$X)) # 38,376 unique values (1 per row), therefore all are IDs

# admin1 column
sum(is.na(x$admin1)) # No NAs
table(x$admin1) # 13 different values, nan - one of the values, "Special EA" too
range(table(x$admin1)) # Range of Frequencies: 468 (nan), 9380 (Oromiya)

# admin2 column
sum(is.na(x$admin2)) # No NAs
table(x$admin2)
length(table(x$admin2)) # 81 different values
table(x$admin1, x$admin2) # Zones 01, 02, 03, 04, 05 are all in Afar

# year column
sum(is.na(x$year)) # No NAs
range(x$year) # 1981 to 2019
table(x$year) # 984 observations for each year
table(x$year, x$admin1) # Multiples of 12, has full year data for every region

# month column
sum(is.na(x$month)) # No NAs
range(x$month) # 1 to 12
table(x$month) # 3198 observations for each month of the year (as in month #)

# mean.rainfall column
sum(is.na(x$mean.rainfall)) # No NAs
class(x$mean.rainfall) # Numeric - correct
range(x$mean.rainfall) # 0.1887958 to 527.9119873

# rainfall_lag column
sum(is.na(x$rainfall_lag)) # 3198 NAs
length(x$rainfall_lag) # 38,376 entries in total
class(x$rainfall_lag) # Numeric - correct
range(x$rainfall_lag) # NA to NA

# Combining the year and month columns to a single year_month column
x$year_month <- paste(x$year, sprintf("%02d", x$month), sep="-")
class(x$year_month) # Character class
str(x)
head(x)

# Selecting the relevant columns for further analysis
x <- x[, c("year", 
           "year_month",
           "admin1",
           "admin2",
           "mean.rainfall",
           "rainfall_lag")]

str(x) # 38376 x 5
head(x)

# Save to a CSV file to combine with other datasets
write.csv(x, "rf_monthly.csv", row.names = FALSE)

