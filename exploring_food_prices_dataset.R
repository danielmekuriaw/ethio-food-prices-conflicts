# Title: Exploring the Ethiopian Food Prices Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 8th, 2024

# This dataset includes prices both in Ethiopian Birr and US Dollars. 
# It also includes general latitude and longitude values of the markets, 
# which could be useful for creating an interactive map interface.

# Reading data and exploring basic attributes
x <- read.csv("wfp_food_prices_eth.csv")
dim(x)  # 40,482 entries, 14 columns
str(x)
head(x)

# Reference row (the 1st row), with descriptions of each column
r <- x[1, ]
head(r)

# Removing the first column from the dataframe
xx <- x[-1, ]
head(xx)

# Date range
min(xx$date) # First data date: 2000-01-15
max(xx$date) # Last data date: 2023-12-15

# Exploring the dataframe column by column
# Starting with the date column
sum(is.na(xx$date)) # No NAs
class(xx$date) # character
xx$date <- as.Date(xx$date, format = "%Y-%m-%d") # Converting it to a date
class(xx$date) # Date
sum(is.na(xx$date)) # No NAs after conversion either


# admin1 column
sum(is.na(xx$admin1)) # No NAs
table(xx$admin1) # A very wide range of frequencies
range(table(xx$admin1)) # Range of frequencies: 5, 11941
                        # Most frequent: Oromia
                        # Least frequent: Empty string followed by B. Gumuz (473)
xx[xx$admin1 == "", ] # 5 entries in 2020 of the Bichena market
xx[xx$market == "Bichena", ] # These 5 are the only entries of Bichena
                             # FILL IN ADMIN1
length(unique(xx$admin1))

# admin2 column
sum(is.na(xx$admin2)) # No NAs
table(xx$admin2) # 5 Empty entries, very wide range of frequencies, 

# 612 - administrative unit not available
# Also has entries like: ZONE 1, ZONE 2, ZONE 3 and so on
# Standardizing the ZONE entries ("ZONE1" and "ZONE 1" are separate values)
xx$admin2 <- gsub("ZONE ", "ZONE", xx$admin2) 
table(xx$admin2) # would still need to figure out what the ZONES represent! (*)
range(table(xx$admin2)) # Range: 5, 3124
xx[xx$admin2 == "", ] # Also Bichena

temp <- table(xx$admin1, xx$admin2)
apply(temp > 0, 1, sum)


# market column
sum(is.na(xx$market)) # No NAs
table(xx$market)
range(table(xx$market)) # Range of frequencies: 5, 1948
                        # Most frequent: Addis Ababa
                        # Least frequent: Bichena


# latitude column
sum(is.na(xx$latitude)) # No NAs
class(xx$latitude) # character
xx$latitude <- as.numeric(xx$latitude)
class(xx$latitude) # Now, it's numeric, which makes sense for coordinates


# longitude column
sum(is.na(xx$longitude)) # No NAs
class(xx$longitude) # character
xx$longitude <- as.numeric(xx$longitude)
class(xx$longitude) # Now, it's numeric, which is reasonable for coordinates

plot(xx$latitude ~ xx$longitude)

# category column
sum(is.na(xx$category)) # No NAs
table(xx$category) # 8 different categories, wide range of frequencies
range(table(xx$category)) # Range of frequencies: 794, 23677

# commodity column
sum(is.na(xx$commodity)) # No NAs
table(xx$commodity)
range(table(xx$commodity)) # Range of frequencies: 1, 7181
                           # Most frequent: Maize (white)
                           # Least frequent: Wage (non-qualified labour), Kocho

# unit column
sum(is.na(xx$unit)) # No NAs
table(xx$unit) # Includes units of different types for different entities
range(table(xx$unit)) # Range of frequencies: 69, 23853
                      # Values: 100 KG, Day,Head, KG, L, Unit, USD/LCU

# priceflag column
sum(is.na(xx$priceflag)) # No NAs
table(xx$priceflag) # Possible values: actual, aggregate
range(table(xx$priceflag)) # Frequencies: 1508 and 38973

# pricetype column
sum(is.na(xx$pricetype)) # No NAs
table(xx$pricetype) # Possible values: Retail, Wholesale
range(table(xx$pricetype)) # Frequencies: 9189 and 31292

# currency column
sum(is.na(xx$currency)) # No NAs
table(xx$currency) # All values are "ETB"

# price column
sum(is.na(xx$price)) # No NAs
class(xx$price) # character
xx$price <- as.numeric(xx$price)
class(xx$price) # Now, it's numeric, which is reasonable for prices

# usdprice column
sum(is.na(xx$usdprice)) # No NAs
class(xx$usdprice) # character
xx$usdprice <- as.numeric(xx$usdprice)
class(xx$usdprice) # Now, it's numeric, which is reasonable for prices

head(xx)
str(xx)

# Preparing data for merging with the other datasets
# Since the date is just 15 for all, we can just remove it and just have a
# year-month format
# Also creating a year column
xx$month_year <- format(xx$date, "%Y-%m")
xx$year <- format(xx$date, "%Y")
head(xx)
tail(xx)

# Aggregate data would not be needed if we already have the actual price data
# Besides, the aggregate data are very few in numbers compared to the actual
table(xx$priceflag)
# Only keeping the actual prices
xx <- xx[xx$priceflag == "actual",]
unique(xx$priceflag)
str(xx)

# Now aggregating using the actual prices for those that match in every grouping
# factor. Mean of usdprice calculated if there are multiple
xxa <- aggregate(
  usdprice ~ year +
    month_year + 
    admin1 + 
    admin2 +
    latitude + 
    longitude + 
    category + 
    unit + 
    pricetype,
  data = xx,
  FUN = mean,
  na.rm = TRUE)

str(xxa)
head(xxa, 200)

# Selecting and keeping only the relevant columns for further analysis in
# combination with the other datasets
# priceflag not needed since all of them are actual prices
xxa <- xxa[, c("year",
             "month_year",
             "admin1",
             "admin2",
             "latitude",
             "longitude",
             "category",
             "unit",
             "pricetype",
             "usdprice")]

str(xxa) # 19,610 x 9 table
head(xxa)

# Writing to a CSV file to later be combined with other datasets
write.csv(xxa, "fp_monthly.csv", row.names = FALSE)
