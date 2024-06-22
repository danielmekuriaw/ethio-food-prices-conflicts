# Title: Exploring the Ethiopian Population Dataset
# Author: Daniel Mekuriaw
# Last Updated: May 02, 2024

# Reading file
x <- read.csv("ethiopia_census_norm.csv")
str(x) # 1911 obs. of 4 variables
head(x)

# ADMIN1NAME column
sum(is.na(x$ADMIN1NAME)) # 0 NAs
table(x$ADMIN1NAME)
length(unique(x$ADMIN1NAME)) # 12 admin1 regions, one of them REGION 17
# Removing entries with REGION 17 as this project only looks at 11 regions
x <- x[x$ADMIN1NAME != "REGION 17", ]
table(x$ADMIN1NAME)
length(unique(x$ADMIN1NAME)) # 11 admin1 regions

# ADMIN2NAME column
sum(is.na(x$ADMIN2NAME)) # 0 NAs
table(x$ADMIN2NAME)
length(unique(x$ADMIN2NAME)) # 86 admin2 regions

# POP column
sum(is.na(x$POP)) # 0 NAs
range(x$POP) # 14253 to 4927367

# YEAR column
sum(is.na(x$YEAR))
range(x$YEAR) # 2000 - 2020
table(x$YEAR) # Each year has 87 observations

# Exporting CSV to combine it with the other datasets
write.csv(x, "p_yearly.csv", row.names = FALSE)
