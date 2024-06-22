# Title: Exploring the Ethiopian Population Projections Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 19th, 2024

library(tidyverse)

# Flag variable to control whether debugging outputs woll be displayed or not
debug.print <- FALSE

# Reading the dataset
x <- read.csv("ethiopia-population-projection-at-admin-2-level-2000-2020.csv")
dim(x)  # 91 Observations, 1146 columns
str(x)
head(x)

# AREA_NAME column
sum(is.na(x$AREA_NAME)) # No NAs
table(x$AREA_NAME)
length(unique(x$AREA_NAME)) # 91, one per observation

# GEO_MATCH column
sum(is.na(x$GEO_MATCH)) # No NAs
table(x$GEO_MATCH)
length(unique(x$GEO_MATCH)) # 91, 1 per observation

# GEO_LABEL column
sum(is.na(x$GEO_LABEL)) # No NAs
table(x$GEO_LABEL)
length(unique(x$GEO_LABEL)) # 91, 1 per observation

# COUNTRY column
sum(is.na(x$COUNTRY)) # No NAs
table(x$COUNTRY) # Only "ETHIOPIA"

# ADM1_NAME
sum(is.na(x$ADM1_NAME)) # No NAs
table(x$ADM1_NAME)
length(table(x$ADM1_NAME)) # 12 different values
range(table(x$ADM1_NAME)) # Range of Frequencies: 1 (HARERI HIZB), 20 (OROMIYA)

# ADM2_NAME
sum(is.na(x$ADM2_NAME)) # No NAs
table(x$ADM2_NAME)
length(table(x$ADM2_NAME)) # 90 different values, one of the rows doesn't have it
range(table(x$ADM2_NAME)) # Range of Frequencies: 1, 2

# ADM3_NAME
sum(is.na(x$ADM3_NAME)) # 91 NAs - all the observations for this column = NA
ADM3_NAME <- NULL # Removing column

# ADM_LEVEL
sum(is.na(x$ADM_LEVEL)) # No NAs
table(x$ADM_LEVEL) # All of the observations have a value of 2

# COMMENT
sum(is.na(x$COMMENT)) # No NAs
table(x$COMMENT)
length(unique(x$COMMENT)) # 49 unique comments
range(table(x$COMMENT)) # 1, 40 ("")

# Converting Character columns to Numeric
convert_to_numeric <- function(col) {
  as.numeric(gsub(",", "", col)) # First taking out comma
}
# Applying this function to all columns that have population data
numeric_columns <- grep("_20", names(x), value = TRUE)
x[, numeric_columns] <- sapply(x[, numeric_columns], 
                                        convert_to_numeric)
str(x) # They have all been converted to numeric now

# Removing all the "Both"/B columns - they are just sum of corresponding M & F
x <- x[, !grepl("^B[0-9]", names(x))]
str(x) # All Bs are removed

# Removing total columns
# Filter out columns for "Both" genders and total columns
x <- x[, !grepl("^B[0-9]|BTOTL|MTOTL|FTOTL", names(x))]
str(x) # All TOTL columns are removed, they are aggregates

# Creating new age groups
# 0-19, 20-59, 60+ - for each gender - M & F
# Age group dictionary
age_groups <- list("0004|0509|1014|1519" = "0-19", 
                   "2024|2529|3034|3539|4044|4549|5054|5559" = "20-59", 
                   "6064|6569|7074|7579|80PL" = "60+")

###############################################################################
###############################################################################

# Function to aggregate columns based on the new age groups
sum_by_age_group <- function(df, cols, age_group, year) {
  
  # Regex pattern to match the age codes in the group followed by the year
  age_pattern <- paste0("(", age_group, ")_", year)
  matching_cols <- grep(age_pattern, cols, value = TRUE) # Finding the columns
  
  # Printing for debugging purposes
  if (debug.print){
    cat("Age Pattern: ", age_pattern)
    cat("Matching Columns: ", matching_cols) 
  }
  
  # Checking if any columns matched (if so, returning the matching columns)
  if (length(matching_cols) > 0) {
    return(rowSums(df[, matching_cols, drop = FALSE], na.rm = TRUE))
  } else {
    return(rep(0, nrow(df)))  # Return zero vector if no columns match
  }
}

# Looping over each year and gender
# using the age group keys directly from the dictionary
for (year in 2000:2020) {
  
  for (gender in c("M", "F")) {
    
    # Printing for debugging
    if (debug.print){
      cat("Processing year:", year, "\n")
      cat("Processing gender:", gender, "\n")
    }
    
    # Finding the columns with the relevant gender characters
    gender_year_col <- grep(paste0("^", gender), names(x), value = TRUE)
    
    # Going through each of the columns in the relevant age group to aggregate
    for (age_key in names(age_groups)) {
      
      # Printing for debugging
      if (debug.print){
        cat("Using age group pattern: ", age_key, "\n")
      }
      
      # Creating the new columns for the new age groups
      result_key <- paste(gender, age_groups[[age_key]], year, sep = "_")
      x[[result_key]] <- sum_by_age_group(x, gender_year_col, age_key, year)
    }
  }
}


# Print structure of x to see the new columns
# The old columns have not been removed, so we only see them as they are a lot
str(x)

# Selecting the relevant and aggregated columns for further combinations and
# analysis.
# NOT included columns: COUNTRY, ADM_LEVEL3, COMMENT, GENC, FIPS, NSO_CODE
# All initial gender/age/year groups are also removed for the new groupings
x <- x[, c("ADM1_NAME",
           "ADM2_NAME",
           grep("_0-19_|_20-59_|_60+", names(x), value = TRUE))]
str(x) # 91 x 128

# Pivoting - making the population columns into rows
xl <- x %>%
  pivot_longer(
    cols = matches("^[MF]_\\d+(-\\d+|\\+)_\\d+$"), # regex to get population col
    names_to = c("Gender", "Age_Group", "Year"),
    names_sep = "_",
    values_to = "Population"
  ) %>%
  mutate(
    Year = as.integer(Year),  # year as an integer
    Gender = ifelse(Gender == "M", "Male", "Female"), # just 'M'/'F'
    Age_Group = gsub("^[MF]_(\\d+-\\d+)_\\d+$", "\\1", Age_Group) # Extract age group
  )

head(xl)
str(xl)

# Aggregating the populations to 6 different columns instead of having gender
# and age group columns
# Creating a column with the future column names
xl$column_name <- with(xl, paste(Gender, Age_Group, "Population", sep="_"))

# Gender and Age_Group columns are redundant now with the addition of the
# column_name column
xl <- xl[, c("ADM1_NAME", 
             "ADM2_NAME",
             "Year",
             "Population",
             "column_name")]

str(xl)
head(xl)

# Pivotting to the desired format
xw <- xl %>%
  group_by(ADM1_NAME, ADM2_NAME, Year, column_name) %>%
  summarise(Population = sum(Population), .groups = "drop") %>%
  pivot_wider(names_from = column_name,
              values_from = Population,
              values_fill = 0,
              values_fn = list(Population = sum))

xw <- as.data.frame(xw)

str(xw) # 1890 x 8
head(xw)

# Making sure we didn't lose any locations from the xl data
sort(unique(xl$ADM1_NAME)) == sort(unique(xw$ADM1_NAME)) # All TRUEs
sort(unique(xl$ADM2_NAME)) == sort(unique(xw$ADM2_NAME)) # All TRUEs

# Exporting the dataframe into a CSV file so that it can be combined with other
# datasets
write.csv(xw, "pp_yearly.csv", row.names = FALSE)
