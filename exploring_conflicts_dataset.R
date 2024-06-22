# Title: Exploring the Ethiopian Conflicts Dataset
# Author: Daniel Mekuriaw
# Last Updated: April 8th, 2024

# This dataset also includes latitude and longitude values of the places where 
# the conflicts took place, which would also be useful for creating map 
# interfaces.

# Loading the library to read .xlsx files
library(readxl)

# Loading Tidyverse
library(tidyverse)

# Reading file and exploring basic attributes
cf <- read_excel("ethiopia_conflict_data.xlsx")
dim(cf)  # 10,660 entries, 31 columns
str(cf)
head(cf)

# EVENT_ID_CNTY column
sum(is.na(cf$EVENT_ID_CNTY)) # No NAs
table(cf$EVENT_ID_CNTY) # All are 1s, so unique ID for each conflict event

# EVENT_DATE column
sum(is.na(cf$EVENT_DATE)) # No NAs
# Date range
min(cf$EVENT_DATE) # First data date: 1997-01-15 UTC
max(cf$EVENT_DATE) # Last data date: 2023-10-20 UTC
# Explicitly setting timezone to UTC so that the "UTC" is removed from the dates
attr(cf$EVENT_DATE, "tzone") <- "UTC"
head(cf$EVENT_DATE) # DID NOT WORK
class(cf$EVENT_DATE) # POSIXct, POSIXt
# Converting to character first to remove "UTC"
cf$EVENT_DATE <- as.character(cf$EVENT_DATE)
sum(is.na(cf$EVENT_DATE)) # Still no NAs
# Removing "UTC" from the date strings
cf$EVENT_DATE <- gsub(" UTC", "", cf$EVENT_DATE)
# Verifying the changes
head(cf$EVENT_DATE) # "2023-10-20" "2023-10-20" "2023-10-20" "2023-10-19"...
# Converting back to date
cf$EVENT_DATE <- as.Date(cf$EVENT_DATE)
class(cf$EVENT_DATE)# Date
sum(is.na(cf$EVENT_DATE)) # Still no NAs


# YEAR column
sum(is.na(cf$YEAR)) # No NAs
table(cf$YEAR) # 1997 - 2023, recent years have lots more records than previous
range(table(cf$YEAR)) # Range of frequencies: 23, 1875

# TIME_PRECISION column
sum(is.na(cf$TIME_PRECISION)) # No NAs
table(cf$TIME_PRECISION) # Possible values: 1, 2, 3
                         # Frequencies: 8489, 1413, 758 respectively

# DISORDER_TYPE column
sum(is.na(cf$DISORDER_TYPE)) # No NAs
table(cf$DISORDER_TYPE) # Possible values: "Demonstrations", "Political violence
                        # ", "Political violence; Demonstrations", and
                        # "Strategic developments"
range(table(cf$DISORDER_TYPE)) # Range of frequencies: 493, 7194

# EVENT_TYPE column
sum(is.na(cf$EVENT_TYPE)) # No NAs
table(cf$EVENT_TYPE) # Possible values: "Battles", "Explosions/Remote violence",
                     # "Protests", "Riots", "Strategic developments", 
                     # "Violence against civilians"
range(table(cf$EVENT_TYPE)) # Range of frequencies: 385, 4428

# SUB_EVENT_TYPE column
sum(is.na(cf$SUB_EVENT_TYPE)) # No NAs
table(cf$SUB_EVENT_TYPE) # Different types, only one suicide bombing
range(table(cf$SUB_EVENT_TYPE)) # Range of frequencies: 1, 4166
                                # Most frequent: Armed clash

# ACTOR1 column
sum(is.na(cf$ACTOR1)) # No NAs
table(cf$ACTOR1)
length(unique(cf$ACTOR1)) # 160 different actors, each with different frequencies
range(table(cf$ACTOR1)) # Range of frequencies: 1, 2374
                        # Most frequent: Protesters (Ethiopia)

# ASSOC_ACTOR_1 column
sum(is.na(cf$ASSOC_ACTOR_1)) # 7458 NAs
table(cf$ASSOC_ACTOR_1)
length(unique(cf$ASSOC_ACTOR_1)) # 232 different associated actors
range(table(cf$ASSOC_ACTOR_1)) # Range of frequencies: 1, 916
                               # Most frequent: Oromo Ethnic Group (Ethiopia)

# INTER1 column
sum(is.na(cf$INTER1)) # 0 NAs
table(cf$INTER1)
length(unique(cf$INTER1)) # 8
range(table(cf$INTER1)) # Range of frequencies: 11 (7), 4163 (1)

# ACTOR2 column
sum(is.na(cf$ACTOR2)) # 2029 NAs
table(cf$ACTOR2)
length(unique(cf$ACTOR2)) # 182
range(table(cf$ACTOR2)) # Range of frequencies: 1 (multiple), 
                        # 2832 (Civilians (Ethiopia))

# ASSOC_ACTOR_2 column
sum(is.na(cf$ASSOC_ACTOR_2)) # 7563 NAs
table(cf$ASSOC_ACTOR_2)
length(unique(cf$ASSOC_ACTOR_2)) # 387
range(table(cf$ASSOC_ACTOR_2)) # Range of frequencies: 1 (multiple), 
                               # 543 (Tigray Ethnic Militia (Ethiopia))


# INTER2 column
sum(is.na(cf$INTER2)) # 0 NAs
table(cf$INTER2)
length(unique(cf$INTER2)) # 8 (0-8, skips 6)
range(table(cf$INTER2)) # Range of frequencies: 44 (5), 2897 (7)

# INTERACTION column
sum(is.na(cf$INTERACTION)) # 0 NAs
table(cf$INTERACTION)
length(unique(cf$INTERACTION)) # 38 (10-88, skips some numbers)
range(table(cf$INTERACTION)) # Range of frequencies: 1 (5), 2258 (12)

# CIVILIAN_TARGETING column
sum(is.na(cf$CIVILIAN_TARGETING)) # 7660 NAs
table(cf$CIVILIAN_TARGETING) # "Civilian targeting" is the only value, 3000
length(unique(cf$CIVILIAN_TARGETING)) # 2 ???
range(table(cf$CIVILIAN_TARGETING)) # 3000

# ISO column
sum(is.na(cf$ISO)) # 0 NAs
table(cf$ISO) # 231 is the only value, 10660
length(unique(cf$ISO)) # 1
range(table(cf$ISO)) # 10660

# REGION column
sum(is.na(cf$REGION)) # 0 NAs
table(cf$REGION) # "Eastern Africa" is the only value, 10660
length(unique(cf$REGION)) # 1
range(table(cf$REGION)) # 10660

# COUNTRY column
sum(is.na(cf$COUNTRY)) # 0 NAs
table(cf$COUNTRY) # "Ethiopia" is the only value, 10660
length(unique(cf$COUNTRY)) # 1
range(table(cf$COUNTRY)) # 10660

# ADMIN1 column
sum(is.na(cf$ADMIN1)) # 0 NAs
table(cf$ADMIN1)
length(unique(cf$ADMIN1)) # 13 different values
range(table(cf$ADMIN1)) # Range of Frequencies: 51 (Harari), 4594 (Oromia)

# ADMIN2 column
sum(is.na(cf$ADMIN2)) # 0 NAs
table(cf$ADMIN2)
length(unique(cf$ADMIN2)) # 90 different values
range(table(cf$ADMIN2)) # Range of Frequencies: 1 (multiple), 581 (West Shewa)

# ADMIN3 column
sum(is.na(cf$ADMIN3)) # 0 NAs
table(cf$ADMIN3)
length(unique(cf$ADMIN3)) # 694 different values
range(table(cf$ADMIN3)) # Range of Frequencies: 1 (multiple), 382 ()

# LOCATION column
sum(is.na(cf$LOCATION)) # 0 NAs
table(cf$LOCATION) # lots of different values
length(unique(cf$LOCATION)) # 1361 different values
range(table(cf$LOCATION)) # Range of Frequencies: 1 (multiple), 376 ()

# LATITUDE column
sum(is.na(cf$LATITUDE)) # 0 NAs
range(cf$LATITUDE) # 3.450 - 14.695

# LONGITUDE column
sum(is.na(cf$LONGITUDE)) # 0 NAs
range(cf$LONGITUDE) # 33.086 - 45.529

# Plotting, does look like Ethiopia roughly
plot(cf$LATITUDE ~ cf$LONGITUDE) # size - fatalities

# GEO_PRECISION column
sum(is.na(cf$GEO_PRECISION)) # 0 NAs
table(cf$GEO_PRECISION) # 3 different values (1, 2, 3)
length(unique(cf$GEO_PRECISION)) # 3 different values
range(table(cf$GEO_PRECISION)) # Range of Frequencies: 274 (3), 5880 (1)

# FATALITIES column
sum(is.na(cf$FATALITIES)) # 0 NAs
range(cf$FATALITIES) # 0 - 1172
hist(log(1+cf$FATALITIES))

# TAGS column
sum(is.na(cf$TAGS)) # 9279 NAs
table(cf$TAGS) # 3 different values (1, 2, 3)
length(unique(cf$TAGS)) # 67 different values
range(table(cf$TAGS)) # Range of Frequencies: 1 (multiple), 1119 ()

# NOT LOOKING AT
# TIMESTAMP, NOTES, SOURCE, SOURCE_SCALE

# Aggregating the conflict data per month, per year
# First selecting the relevant columns
cf.m <- cf[, c("YEAR", 
               "EVENT_DATE",
               "EVENT_TYPE",
               "ADMIN1",
               "ADMIN2", # The common column in other datasets too
               "FATALITIES")]

# Converting EVENT_DATE to a month-year format
# This would be helpful to aggregate per month
cf.m$MONTH_YEAR <- format(cf.m$EVENT_DATE, "%Y-%m")
head(cf.m)
tail(cf.m)

# Grouping by month-year, ADMIN1 and ADMIN2, summing fatalities
cf.monthly <- cf.m %>%
  group_by(YEAR, MONTH_YEAR, ADMIN1, ADMIN2) %>%
  summarise(TOTAL_FATALITIES = sum(FATALITIES), .groups = 'drop')

# Counting event types for each month-year, ADMIN1 and ADMIN2 combinations
event.counts <- cf.m %>%
  group_by(YEAR, MONTH_YEAR, ADMIN1, ADMIN2, EVENT_TYPE) %>%
  summarise(EVENT_COUNT = n(), .groups = 'drop')

# Joining the two datasets
cf.monthly <- merge(cf.monthly, 
                    event.counts, 
                    by = c("YEAR","MONTH_YEAR", "ADMIN1","ADMIN2"))
head(cf.monthly)

# Spreading event types into separate columns
cf.monthly <- cf.monthly %>%
  pivot_wider(names_from = EVENT_TYPE,
              values_from = EVENT_COUNT,
              values_fill = list(EVENT_COUNT = 0))

cf.monthly <- as.data.frame(cf.monthly)

str(cf.monthly) # 3,424 x 10 table
head(cf.monthly)

# Writing to a CSV file to later be combined with another dataset
write.csv(cf.monthly, "cf_monthly.csv", row.names = FALSE)