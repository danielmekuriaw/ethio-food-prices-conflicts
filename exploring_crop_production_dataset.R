# Title: Exploring the Ethiopian Crop Production Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 19th, 2024

# Reading the dataset
x <- read.csv("ethiopia-crop-production-statistics-meher-and-belg-admin-2-level-2004-2017.csv")
dim(x)  # 22,087 entries, 11 columns
str(x)
head(x)

# Season column
sum(is.na(x$Season)) # No NAs
table(x$Season) # All seasons are "Meher", 22,087

# Year range
min(x$Year) # 2004/2005
max(x$Year) # 2017/2018

# Country column
sum(is.na(x$Season)) # No NAs
table(x$Country) # All entries are "Ethiopia", 22,087

# Region column
sum(is.na(x$Region)) # No NAs
table(x$Region)
length(table(x$Region)) # 12 categories, one of them is empty string
range(table(x$Region)) # Range of frequencies: 14, 7444
nrow(x[x$Region == "", ]) # 52 empty Regions

# Zone column
sum(is.na(x$Zone)) # No NAs
table(x$Zone)
length(table(x$Zone)) # 79 categories, one of them is empty string
head(x[x$Zone == "", ]) # Empty Region and empty Zone coincide on some
tail(x[x$Zone == "", ]) 
nrow(x[x$Zone == "", ]) # 336 empty Zone


# Crop
sum(is.na(x$Crop)) # No NAs
table(x$Crop)
length(table(x$Crop)) # 71 categories, Maize appears in 3 different formats
                      # Faba beans appear in two different formats
                      # Field peas appear in two different formats
                      # Grass peas, Hops, Lentils, appear in different formats
                      # Linseed, Mangoes, Onion, Papayas, Potatoes, Safflower
                      # Sesame, Sorghum, Garlic
# Fixing the duplicate names
x$Crop <- gsub("Maize\\s+", "Maize", x$Crop) 
x$Crop <- gsub("Faba beans ", "Faba beans", x$Crop)
x$Crop <- gsub("Field peas ", "Field peas", x$Crop)
x$Crop <- gsub("Grass peas ", "Grass peas", x$Crop)
x$Crop <- gsub("Grass Peas", "Grass peas", x$Crop)
x$Crop <- gsub("Hops ", "Hops", x$Crop)
x$Crop <- gsub("Lentils ", "Lentils", x$Crop)
x$Crop <- gsub("Linseed ", "Linseed", x$Crop)
x$Crop <- gsub("Mangoes ", "Mangoes", x$Crop)
x$Crop <- gsub("Onion ", "Onion", x$Crop)
x$Crop <- gsub("Papayas ", "Papayas", x$Crop)
x$Crop <- gsub("Potatoes ", "Potatoes", x$Crop)
x$Crop <- gsub("Safflower ", "Safflower", x$Crop)
x$Crop <- gsub("Sesame ", "Sesame", x$Crop)
x$Crop <- gsub("Sorghum ", "Sorghum", x$Crop)
x$Crop <- gsub("Garlic ", "Garlic", x$Crop)
table(x$Crop) # Varying frequencies
length(table(x$Crop)) # 55 Types of Crops
range(table(x$Crop)) # Range of frequencies: 5 (Sunflower), 712 (Maize)
# trimws - base R - Look into it

# Category
sum(is.na(x$Category)) # No NAs
table(x$Category) # There's a duplicate category name
# Fixing duplicate
x$Category <- gsub("Hops ", "Hops", x$Category)
table(x$Category)
length(table(x$Category)) # 11 Categories
range(table(x$Category)) # Range of Frequencies: 90 (Enset), 4041 (Cereals)

# Number.of.holders (Need to understand what it represents!!)
sum(is.na(x$Number.of.holders)) # No NAs
length(x$Number.of.holders) # 22,087
length(unique(x$Number.of.holders)) # 16,146 unique number of holders
nrow(x[x$Number.of.holders == "", ]) # 2417 empty strings
class(x$Number.of.holders) # Character type
x$Number.of.holders <- as.numeric(x$Number.of.holders) # Introduces NAs
class(x$Number.of.holders) # Numeric type
sum(is.na(x$Number.of.holders)) # 3519 NAs

# Area.in.hectare
sum(is.na(x$Area.in.hectare)) # No NAs
class(x$Area.in.hectare) # Character type
nrow(x[x$Area.in.hectare == "", ]) # 6491 empty strings
x$Area.in.hectare <- as.numeric(x$Area.in.hectare) # NAs are expected
class(x$Area.in.hectare) # Numeric type
sum(is.na(x$Area.in.hectare)) # 6962 NAs

# Production.in.quintal
sum(is.na(x$Production.in.quintal)) # No NAs
class(x$Production.in.quintal) # Character
nrow(x[x$Production.in.quintal == "", ]) # 8364 empty strings
x$Production.in.quintal <- as.numeric(x$Production.in.quintal)
class(x$Production.in.quintal) # Numeric type
sum(is.na(x$Production.in.quintal)) # 9181 NAs

# Yield..qt..ha.
sum(is.na(x$Yield..qt...ha.)) # No NAs
class(x$Yield..qt...ha.) # Character
nrow(x[x$Yield..qt...ha. == "", ]) # 8560 empty strings
x$Yield..qt...ha. <- as.numeric(x$Yield..qt...ha.)
class(x$Yield..qt...ha.) # Numeric type
sum(is.na(x$Yield..qt...ha.)) # 8616 NAs
# Renaming Column for convenience
x$Yqth <- x$Yield..qt...ha.

str(x)

# Selecting the relevant columns for further analysis
# All seasons are the same - so no need to include that column
x <- x[, c("Year",
           "Region",
           "Zone",
           "Category",
           "Number.of.holders",
           "Area.in.hectare",
           "Production.in.quintal",
           "Yqth")]

str(x)
head(x)

# Meher is May - September so we can just keep the year that comes on second in
# the following formats: 2014/2015 (this would just become 2015)
x$Year <- sub(".*/(\\d{4})", "\\1", x$Year)

str(x) # 22,087 x 9 table
head(x)

# Aggregating for each category
# 1st replacing NAs with 0s
x$Area.in.hectare[is.na(x$Area.in.hectare)] <- 0
x$Production.in.quintal[is.na(x$Production.in.quintal)] <- 0
x$Yqth[is.na(x$Yqth)] <- 0

# Formula for aggregation
agg.formula <- cbind(Number.of.holders, 
                     Area.in.hectare, 
                     Production.in.quintal, 
                     Yqth) ~ Year + Region + Zone + Category

# Perform the aggregation
xx <- aggregate(agg.formula, data = x, FUN = sum)

str(xx) # 5750 x 8 table
head(xx, 200)

# Save to a CSV file to  combine with other datasets
write.csv(xx, "cp_yearly.csv", row.names = FALSE)
