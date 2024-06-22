# Title: Combining the different datasets together, monthly aggregations
# Author: Daniel Mekuriaw
# Last Updated: April 30th, 2024

# Loading/reading all the datasets to be combined

# Food Prices Dataset
fp.monthly <- read.csv("fp_monthly.csv")
head(fp.monthly)

# Conflicts Dataset
cf.monthly <- read.csv("cf_monthly.csv")
head(cf.monthly)

# Crop Production Dataset
cp.yearly <- read.csv("cp_yearly.csv")
head(cp.yearly)

# Rainfall Dataset
rf.monthly <- read.csv("rf_monthly.csv")
head(rf.monthly)

# Population Dataset
p.yearly <- read.csv("p_yearly.csv")
head(p.yearly)

# Population Projections Dataset
pp.yearly <- read.csv("pp_yearly.csv")
head(pp.yearly)

# Currency Dataset
cr.monthly <- read.csv("cr_monthly.csv")
head(cr.monthly)


# YEARLY --> MONTHLY CONVERSIONS
# Creating duplicate entries for each yearly dataset for each month of the given
# year
str(cp.yearly)
str(p.yearly)
str(pp.yearly)

# Dealing with the yearly Crop Production dataset first
# Replicating each row in the dataframe 12x
cp.yearly.expanded <- cp.yearly[rep(seq_len(nrow(cp.yearly)), each = 12), ]
head(cp.yearly.expanded, 20)

# Repeating 1:12 for each 12 rows of each year
cp.yearly.expanded$month <- rep(1:12, times = nrow(cp.yearly))
head(cp.yearly.expanded, 20)

# Creating a year-month column using the year values and the month
cp.yearly.expanded$year_month <- with(cp.yearly.expanded, 
                                      paste(Year, 
                                            sprintf("%02d", month), 
                                            sep = "-"))
head(cp.yearly.expanded)

# Dropping the month column as it is no longer needed
cp.yearly.expanded$month <- NULL

str(cp.yearly.expanded)

# Selecting the final set of columns
cp.yearly.expanded <- cp.yearly.expanded[, c("Year",
                                             "year_month",
                                             "Region",
                                             "Zone",
                                             "Category",
                                             "Number.of.holders",
                                             "Area.in.hectare",
                                             "Production.in.quintal",
                                             "Yqth")]

head(cp.yearly.expanded)

# Expanding the yearly population dataset next
p.yearly.expanded <- p.yearly[rep(seq_len(nrow(p.yearly)), each = 12), ]
head(p.yearly.expanded, 20)

# Repeating 1:12 for each 12 rows of each year
p.yearly.expanded$month <- rep(1:12, times = nrow(p.yearly))
head(p.yearly.expanded, 20)

# Creating a year-month column using the year values and the month
p.yearly.expanded$year_month <- with(p.yearly.expanded, 
                                      paste(YEAR, 
                                            sprintf("%02d", month), 
                                            sep = "-"))
head(p.yearly.expanded)

# Dropping the month column as it is no longer needed
p.yearly.expanded$month <- NULL
str(p.yearly.expanded)

# Selecting the final set of columns
p.yearly.expanded <- p.yearly.expanded[, c("YEAR", 
                                           "year_month",
                                           "ADMIN1NAME",
                                           "ADMIN2NAME",
                                           "POP")]
head(p.yearly.expanded, 20)

# Dealing with the yearly Population projections dataset next
# Replicating each row in the dataframe 12x
pp.yearly.expanded <- pp.yearly[rep(seq_len(nrow(pp.yearly)), each = 12), ]
head(pp.yearly.expanded, 20)

# Repeating 1:12 for each 12 rows of each year
pp.yearly.expanded$month <- rep(1:12, times = nrow(pp.yearly))
head(pp.yearly.expanded, 20)

# Creating a year-month column using the year values and the month
pp.yearly.expanded$year_month <- with(pp.yearly.expanded, 
                                      paste(Year, 
                                            sprintf("%02d", month), 
                                            sep = "-"))
head(pp.yearly.expanded)

# Dropping the month column as it is no longer needed
pp.yearly.expanded$month <- NULL

str(pp.yearly.expanded)

# Selecting the final set of columns
pp.yearly.expanded <- pp.yearly.expanded[, c("Year",
                                             "year_month",
                                             "ADM1_NAME",
                                             "ADM2_NAME",
                                             "Female_0.19_Population",
                                             "Female_20.59_Population",
                                             "Female_60._Population",
                                             "Male_0.19_Population",
                                             "Male_20.59_Population",
                                             "Male_60._Population")]

head(pp.yearly.expanded)

# COMBINING DATASETS
# Looking at the columns to be used to combine from each of the datasets

# Conflict dataset
range(cf.monthly$MONTH_YEAR)
unique(cf.monthly$ADMIN1)
unique(cf.monthly$ADMIN2)

# Food prices dataset
range(fp.monthly$month_year)
unique(fp.monthly$admin1)
unique(fp.monthly$admin2)

# Crop Production dataset
range(cp.yearly.expanded$year_month)
unique(cp.yearly.expanded$Region)
unique(cp.yearly.expanded$Zone)

# Rainfall dataset
range(rf.monthly$year_month)
unique(rf.monthly$admin1)
unique(rf.monthly$admin2)

# Population Projections dataset
range(pp.yearly.expanded$year_month)
unique(pp.yearly.expanded$ADM1_NAME)
unique(pp.yearly.expanded$ADM2_NAME)

# Population dataset
range(p.yearly.expanded$year_month)
unique(p.yearly.expanded$ADMIN1NAME)
unique(p.yearly.expanded$ADMIN2NAME)

# Currency dataset
range(cr.monthly$year_month)

###############################################################################

# STANDARDIZING ADMIN 1 Column entries
# Assigning standardized labels to the 9 National Regional States as well as
# the two administrative councils

# Conflicts data
unique(cf.monthly$ADMIN1)
length(unique(cf.monthly$ADMIN1)) # 13, new regions: Sidama (2020), South West
# Replace 'Sidama', 'SNNP' and 'South West' with 'SNNPR'
cf.monthly$ADMIN1 <- ifelse(cf.monthly$ADMIN1 %in%
                              c("Sidama", "SNNP", "South West"), 
                            "SNNPR", 
                            cf.monthly$ADMIN1)
unique(cf.monthly$ADMIN1)
length(unique(cf.monthly$ADMIN1)) # 11 - these 11 admin 1 values will be used


# Food prices data
unique(fp.monthly$admin1)
length(unique(fp.monthly$admin1)) # 12, including an empty string
# Looking into the ones with empty strings
sum(fp.monthly$admin1 == "") # 5 rows/data points
fp.monthly[fp.monthly$admin1 == "", ] # These are the Bichena data points
                                      # Bichena is in the Amhara region
# Setting their admin1 value to "Amhara"
fp.monthly$admin1[fp.monthly$admin1 == ""] <- "Amhara"
unique(fp.monthly$admin1)
length(unique(fp.monthly$admin1)) # 11, no empty string anymore
# Setting "B. Gumuz" to "Benshangul/Gumuz" to standardize with the other source
fp.monthly$admin1 <- ifelse(fp.monthly$admin1 %in% c("B. Gumuz"), 
                            "Benshangul/Gumuz", 
                            fp.monthly$admin1)
unique(fp.monthly$admin1) # Now we have Benshangul/Gumuz and not B. Gumuz
# Making sure we have the same set of admin 1 values from the 2 datasets so far:
sort(unique(cf.monthly$ADMIN1)) == sort(unique(fp.monthly$admin1)) # All TRUEs


# Crop Production data
unique(cp.yearly.expanded$Region)
# The naming of some of the columns need to be changed to match with what has
# been normalized for the two datasets so far
cp.yearly.expanded$Region <- ifelse(
  cp.yearly.expanded$Region == "Benishangul Gumuz", 
  "Benshangul/Gumuz", 
  cp.yearly.expanded$Region)
cp.yearly.expanded$Region <- ifelse(
  cp.yearly.expanded$Region %in% c("Oromiya ", "Oromiya"), 
  "Oromia", 
  cp.yearly.expanded$Region)
cp.yearly.expanded$Region <- ifelse(
  cp.yearly.expanded$Region == "SNNP", 
  "SNNPR", 
  cp.yearly.expanded$Region)
cp.yearly.expanded$Region <- ifelse(
  cp.yearly.expanded$Region == "Gambella", 
  "Gambela", 
  cp.yearly.expanded$Region)
unique(cp.yearly.expanded$Region) # Have some with "" as their Region
length(unique(cp.yearly.expanded$Region)) # 11, including the empty strings
# Looking into which rows have empty entries for this column
sum(cp.yearly.expanded$Region == "") # 624 entries, they also don't have Zone
nrow(cp.yearly.expanded) # out of 265,044 total entries
# Removing these rows with empty strings for region and Zone, shouldn't be
# significant considering how few they are
# Remove rows where either Region or Zone are empty strings
cp.yearly.expanded <- cp.yearly.expanded[cp.yearly.expanded$Region != "", ]
unique(cp.yearly.expanded$Region) # No Region with an empty string
length(unique(cp.yearly.expanded$Region)) # 10 different values
# Trying to figure out which of the 11 regions from the earlier two datasets is
# missing from this dataset
sort(unique(cp.yearly.expanded$Region)) # Missing Addis Ababa
sort(unique(fp.monthly$admin1))


# Rainfall dataset
unique(rf.monthly$admin1)
# The naming of some of the columns need to be changed to match with what has
# been normalized for the datasets so far
rf.monthly$admin1 <- ifelse(
  rf.monthly$admin1 == "Gambella", 
  "Gambela", 
  rf.monthly$admin1)
rf.monthly$admin1 <- ifelse(
  rf.monthly$admin1 == "Benishangul Gumuz", 
  "Benshangul/Gumuz", 
  rf.monthly$admin1)
rf.monthly$admin1 <- ifelse(
  rf.monthly$admin1 == "Oromiya", 
  "Oromia", 
  rf.monthly$admin1)
rf.monthly$admin1 <- ifelse(
  rf.monthly$admin1 == "SNNP", 
  "SNNPR", 
  rf.monthly$admin1)
unique(rf.monthly$admin1)
length(unique(rf.monthly$admin1)) # 13, including Special EA and "nan"
# Exploring the rows with "Special EA" and "nan" further
sum(rf.monthly$admin1 == "Special EA") # 1872 entries
sum(rf.monthly$admin1 == "nan") # 468 entries
nrow(rf.monthly) # out of 38,376 entries
head(rf.monthly[rf.monthly$admin1 == "Special EA", ], 20)
head(rf.monthly[rf.monthly$admin1 == "nan", ], 20)
# Removing all rows with admin1 entries of "Special EA" and "nan"
# It's very difficult to interpret them otherwise since their locations are not
# specified
rf.monthly <- rf.monthly[rf.monthly$admin1 != "Special EA", ]
rf.monthly <- rf.monthly[rf.monthly$admin1 != "nan", ]
unique(rf.monthly$admin1)
length(unique(rf.monthly$admin1)) # 11 regions
# Making sure we have the same set of admin 1 values from all datasets so far:
sort(unique(cf.monthly$ADMIN1)) == sort(unique(rf.monthly$admin1)) # All TRUEs


# Population dataset
unique(pp.yearly.expanded$ADM1_NAME)
# The naming of the columns needs to be changed to match with what has
# been normalized for the datasets so far
# Removing any extra spaces if any
pp.yearly.expanded$ADM1_NAME <- trimws(pp.yearly.expanded$ADM1_NAME)
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "ADIS ABEBA"] <- "Addis Ababa"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "AFAR"] <- "Afar"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "AMARA"] <- "Amhara"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "BINSHANGUL GUMUZ"] <- "Benshangul/Gumuz"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "DIR? DAWA"] <- "Dire Dawa"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "GAMB?LA HIZBOCH"] <- "Gambela"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "HARERI HIZB"] <- "Harari"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "OROMIYA"] <- "Oromia"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "SUMAL?"] <- "Somali"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "TIGRAY"] <- "Tigray"
pp.yearly.expanded$ADM1_NAME[
  pp.yearly.expanded$ADM1_NAME == "YEDEBUB BIH?ROCH BIH?RESEBOCH NA HIZBOCH"] <- 
  "SNNPR"
unique(pp.yearly.expanded$ADM1_NAME) # Includes a region called "REGION 17"
# Exploring the rows with the "REGION 17" value further
sum(pp.yearly.expanded$ADM1_NAME == "REGION 17") # 1008 entries
nrow(pp.yearly.expanded) # out of 22,932 total entries
head(pp.yearly.expanded[pp.yearly.expanded$ADM1_NAME == "REGION 17", ], 20)
# Seems like it is a special EA region, which is only present in the rainfall
# dataset. Therefore, it will be excluded from the remaining dataset
# It would be very difficult to try and interpret it in the context of the other
# datasets.
pp.yearly.expanded <- pp.yearly.expanded[
  pp.yearly.expanded$ADM1_NAME != "REGION 17", ]
unique(pp.yearly.expanded$ADM1_NAME)
length(unique(pp.yearly.expanded$ADM1_NAME)) # 11 regions
# Making sure we have the same set of admin 1 values from all datasets so far:
sort(unique(cf.monthly$ADMIN1)) == sort(unique(pp.yearly.expanded$ADM1_NAME))
# All TRUEs

# Now doing the same thing for p.yearly - population dataset
unique(p.yearly.expanded$ADMIN1NAME)
# The naming of the columns needs to be changed to match with what has
# been normalized for the datasets so far
# Removing any extra spaces if any
p.yearly.expanded$ADMIN1NAME <- trimws(p.yearly.expanded$ADMIN1NAME)
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "ADIS ABEBA"] <- "Addis Ababa"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "AFAR"] <- "Afar"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "AMARA"] <- "Amhara"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "BINSHANGUL GUMUZ"] <- "Benshangul/Gumuz"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "DIR? DAWA"] <- "Dire Dawa"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "GAMB?LA HIZBOCH"] <- "Gambela"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "HARERI HIZB"] <- "Harari"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "OROMIYA"] <- "Oromia"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "SUMAL?"] <- "Somali"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "TIGRAY"] <- "Tigray"
p.yearly.expanded$ADMIN1NAME[
  p.yearly.expanded$ADMIN1NAME == "YEDEBUB BIH?ROCH BIH?RESEBOCH NA HIZBOCH"] <- 
  "SNNPR"

unique(p.yearly.expanded$ADMIN1NAME)
sort(unique(cf.monthly$ADMIN1)) == sort(unique(p.yearly.expanded$ADMIN1NAME))
# ALL TRUEs

################################################################################

# STANDARDIZING ADMIN 2 Column entries

# Starting with the Conflicts dataset
unique(cf.monthly$ADMIN2)
sort(unique(cf.monthly$ADMIN2))
length(unique(cf.monthly$ADMIN2)) # 90 ADMIN2s

# Food prices dataset
unique(fp.monthly$admin2) # includes empty "" strings as values
length(unique(fp.monthly$admin2)) # 57 admin2s
sum(fp.monthly$admin2 == "") # 5 rows/data points
fp.monthly[fp.monthly$admin2 == "", ] # This is Bichena again
# Based on online research, it would belong to "E. GOJAM"
fp.monthly$admin2[fp.monthly$admin2 == ""] <- "E. GOJAM"
unique(fp.monthly$admin2)
sort(unique(fp.monthly$admin2))
length(unique(fp.monthly$admin2)) # 56 different values

# A renaming vector to standardize the admin 2 values between the two datasets
rename.vector <- c(
  "Afder" = "AFDER",
  "Agnewak" = "Agnewak",
  "Alle" = "Alle",
  "Amaro" = "AMARO SW",
  "Arsi" = "ARSI",
  "Asosa" = "ASOSA",
  "Awi" = "Awi",
  "Awsi-Zone 1" = "Awsi-Zone 1",
  "Bale" = "BALE",
  "Bench Sheko" = "Bench Sheko",
  "Borena" = "BORENA",
  "Buno Bedele" = "Buno Bedele",
  "Burji" = "Burji",
  "Central Gondar" = "N. GONDER",
  "Central Tigray" = "C. TIGRAY",
  "Daawa" = "Daawa",
  "Dawuro" = "Dawuro",
  "Derashe" = "Derashe",
  "Dire Dawa rural" = "DIRE DAWA",
  "Dire Dawa urban" = "DIRE DAWA",
  "Doolo" = "Doolo",
  "East Bale" = "East Bale",
  "East Gojam" = "E. GOJAM",
  "East Hararge" = "E. HARERGE",
  "East Shewa" = "E. SHEWA",
  "East Wellega" = "E. WELLEGA",
  "Eastern Tigray" = "E. TIGRAY",
  "Erer" = "Erer",
  "Fafan" = "Fafan",
  "Fanti-Zone 4" = "Fanti-Zone 4",
  "Finfine Special" = "Finfine Special",
  "Gabi-Zone 3" = "Gabi-Zone 3",
  "Gamo" = "GAMO GOFA",
  "Gedeo" = "GEDEO",
  "Gofa" = "Gofa",
  "Guji" = "GUJI",
  "Guraghe" = "GURAGE",
  "Hadiya" = "HADIYA",
  "Halaba" = "Halaba",
  "Harari" = "Harari",
  "Hari-Zone 5" = "Hari-Zone 5",
  "Horo Gudru Wellega" = "Horo Gudru Wellega",
  "Ilu Aba Bora" = "Ilu Aba Bora",
  "Itang Special woreda" = "Itang Special Woreda",
  "Jarar" = "Jarar",
  "Jimma" = "JIMMA",
  "Kefa" = "Kefa",
  "Kellem Wollega" = "Kellem Wollega",
  "Kemashi" = "Kemashi",
  "Kembata Tibaro" = "Kembata Tibaro",
  "Kilbati-Zone 2" = "Kilbati-Zone 2",
  "Konso" = "KONSO SW",
  "Konta Special" = "Konta Special",
  "Korahe" = "KORAHE",
  "Liban" = "LIBEN",
  "Majang" = "Majang",
  "Mao Komo Special" = "Mao Komo Special",
  "Mekelle Tigray" = "MEKELE",
  "Metekel" = "METEKEL",
  "Mirab Omo" = "Mirab Omo",
  "Nogob" = "Nogob",
  "North Gondar" = "N. GONDER",
  "North Shewa" = "N. SHEWA (R3)",
  "North Wello" = "N. WELLO",
  "North Western Tigray" = "NW. TIGRAY",
  "Nuwer" = "Nuwer",
  "Oromia" = "OROMIYA",
  "Region 14" = "Region 14",
  "Shabelle" = "Shabelle",
  "Sheka" = "Sheka",
  "Sidama" = "SIDAMA",
  "Siltie" = "Siltie",
  "Siti" = "Siti",
  "South Eastern Tigray" = "S. TIGRAY",
  "South Gondar" = "S. GONDER",
  "South Omo" = "SOUTH OMO",
  "South Wello" = "S. WELLO",
  "South West Shewa" = "S.W. SHEWA",
  "Southern Tigray" = "S. TIGRAY",
  "Wag Hamra" = "W. HAMRA",
  "Welayta" = "WELAYITA",
  "West Arsi" = "West Arsi",
  "West Gojam" = "W. GOJAM",
  "West Gondar" = "W. GONDER",
  "West Guji" = "West Guji",
  "West Hararge" = "W. HARERGE",
  "West Shewa" = "WEST SHEWA",
  "West Wellega" = "West Wellega",
  "Western Tigray" = "W. TIGRAY"
)

# Crop production dataset
unique(cp.yearly.expanded$Zone) # includes empty "" strings as values
length(unique(cp.yearly.expanded$Zone)) # 79 values including the empty ones
sum(cp.yearly.expanded$Zone == "") # 3408 empty values
nrow(cp.yearly.expanded) # out of 264,420 values
head(cp.yearly.expanded[cp.yearly.expanded$Zone == "", ], 20)
unique(cp.yearly.expanded[cp.yearly.expanded$Zone == "", ]$Region) # D/t regions
# Removing these rows withe empty zones, very few compared to the total number
# of observations
cp.yearly.expanded <- cp.yearly.expanded[cp.yearly.expanded$Zone != "", ]
unique(cp.yearly.expanded$Zone) # includes empty "" strings as values
length(unique(cp.yearly.expanded$Zone)) # 78, no empty string
cp.yearly.expanded$Zone[cp.yearly.expanded$Zone == " Zone 01"] <- "Zone 01"
unique(cp.yearly.expanded$Zone)
length(unique(cp.yearly.expanded$Zone)) # 77 different values

# Existing rename.vector with new additions from the new dataset
rename.vector <- c(
  rename.vector,  # Existing mappings
  "Zone  03" = "Zone 3",
  "Zone 01" = "Zone 1",
  "Argoba " = "Argoba",  # Trimmed space
  "East Gojjam" = "E. GOJAM",
  "North Gondar" = "N. GONDER",
  "North Shewa" = "N. SHEWA (R3)",
  "North Wollo" = "N. WELLO",
  "South Gonder" = "S. GONDER",
  "South Wolo" = "S. WELLO",
  "Wag Himra" = "W. HAMRA",
  "West Gojjam" = "W. GOJAM",
  "Assosa" = "ASOSA",
  "Kamashi" = "Kamashi",
  "Maokomo " = "Mao Komo Special",  # Trimmed space
  "Metekel" = "METEKEL",
  "Agnuwak" = "Agnewak",
  "Etang Spe." = "Itang Special Woreda",
  "Mejenger" = "Mezhenger",
  "Nuwer" = "Nuwer",
  "Harer" = "Harar/Hundene",
  "Arsi" = "ARSI",
  "Bale" = "BALE",
  "Borena" = "BORENA",
  "East Hararge" = "E. HARERGE",
  "East Shewa" = "E. SHEWA",
  "East Wellega" = "E. WELLEGA",
  "Guji" = "GUJI",
  "Horo Gudru Wellega" = "Horo Gudru Wellega",
  "Ilu Aba Bora" = "Ilu Aba Bora",
  "Jimma" = "JIMMA",
  "Qeleme Wellega" = "Qeleme Wellega",
  "South West Shewa" = "S.W. SHEWA",
  "West Arsi" = "West Arsi",
  "West Hararge" = "W. HARERGE",
  "West Shewa" = "WEST SHEWA",
  "West Wellega" = "West Wellega",
  "Alaba" = "Alaba",
  "Amaro " = "Amaro",  # Trimmed space
  "Basketo" = "Basketo",
  "Bench Maji" = "Bench Maji",
  "Dawuro" = "Dawuro",
  "Derashe " = "Derashe",  # Trimmed space
  "Gamo Gofa" = "GAMO GOFA",
  "Gedeo" = "GEDEO",
  "Gurage" = "GURAGE",
  "Hadiya" = "HADIYA",
  "Kefa" = "Kefa",
  "Kembata Timbaro" = "Kembata Tibaro",  # Corrected spelling
  "Konso " = "KONSO SW",  # Trimmed space
  "Konta" = "Konta Special",
  "Segen" = "Segen",
  "Sheka" = "Sheka",
  "Sidama" = "SIDAMA",
  "Siliti" = "Siltie",
  "South Omo" = "SOUTH OMO",
  "Wolayita" = "WELAYITA",
  "YEM" = "YEM",
  "Jijiga" = "JIJIGA",
  "Liben" = "LIBEN",
  "Shinile" = "Shinile",
  "Central Tigray" = "C. TIGRAY",
  "Eastern Tigray" = "E. TIGRAY",
  "North Western Tigray" = "NW. TIGRAY",
  "South Tigray" = "S. TIGRAY",
  "Western Tigray" = "W. TIGRAY"
)

# Rainfall dataset
unique(rf.monthly$admin2)
length(unique(rf.monthly$admin2)) # 76 different values

# Updating the rename_vector with new additions from rf.monthly$admin2
rename.vector <- c(
  rename.vector,  # Existing mappings
  "Addis Ababa" = "Addis Ababa",
  "Zone  02" = "Zone 2",  # Correcting spacing issues
  "Zone  03" = "Zone 3",
  "Zone 01" = "Zone 1",
  "Zone 04" = "Zone 4",
  "Zone 05" = "Zone 5",
  "Argoba" = "Argoba",
  "Bahir Dar" = "Bahir Dar",
  "Adama" = "Adama",
  "Burayu" = "Burayu",
  "Awassa" = "Awassa",
  "Jimma Sp." = "Jimma",
  "Mekele Especial Zone" = "MEKELE",
  "Degehabur" = "Degehabur",
  "Fik" = "Fik",
  "Gode" = "Gode",
  "Warder" = "Warder"
)

# Population Projections dataset
unique(pp.yearly.expanded$ADM2_NAME)
length(unique(pp.yearly.expanded$ADM2_NAME)) # 86 different values

# Updating the existing rename_vector with additional mappings
rename.vector <- c(
  rename.vector,  # Existing mappings
  "YEM SPECIAL WEREDA" = "YEM",
  "YEM SPECIAL ZONE" = "YEM",
  "DIR? DAWA" = "Dire Dawa",
  "DIR? DAWA TOWN" = "Dire Dawa",
  "HARERI HIZB" = "Harari",
  "ADAMA SPECIAL ZONE" = "Adama",
  "BURAYU SPECIAL ZONE" = "Burayu",
  "JIMA SPECIAL ZONE" = "Jimma",
  "MEKELE SPECIAL" = "MEKELE",
  "AWI-ZONE" = "Awi",
  "BAHIR DAR SPECIAL ZONE" = "Bahir Dar",
  "KELEM WELEGA" = "Kellem Wollega",
  "HAWASSA CITY ADMINISTRATION-ZONE" = "Awassa",
  "DEGEHABUR" = "Degehabur",
  "FIK" = "Fik",
  "GODE" = "Gode",
  "JIJIGA" = "Jijiga",
  "KORAHE" = "Korahe",
  "LIBEN" = "Liben",
  "SHINILE" = "Shinile",
  "WARDER" = "Warder",
  "CENTRAL TIGRAY" = "C. TIGRAY",
  "EASTERN TIGRAY" = "E. TIGRAY",
  "NORTH WESTERN TIGRAY" = "NW. TIGRAY",
  "SOUTHERN TIGRAY" = "S. TIGRAY",
  "WESTERN TIGRAY" = "W. TIGRAY",
  "ARGOBA SPECIAL ZONE" = "Argoba"
)

# Population dataset
unique(p.yearly.expanded$ADMIN2NAME)
length(unique(p.yearly.expanded$ADMIN2NAME)) # 86 different values

sort(unique(p.yearly.expanded$ADMIN2NAME)) == 
  sort(unique(pp.yearly.expanded$ADM2_NAME)) # All TRUEs, same values for both
# So no need to add any values to the rename.vector

apply.rename <- function(name) {
  if (!is.na(name) && name %in% names(rename.vector)) {
    return(rename.vector[name])
  } else {
    return(name)
  }
}

# Applying the rename function to each dataset
cf.monthly$ADMIN2 <- sapply(cf.monthly$ADMIN2, apply.rename)
fp.monthly$admin2 <- sapply(fp.monthly$admin2, apply.rename)
cp.yearly.expanded$Zone <- sapply(cp.yearly.expanded$Zone, apply.rename)
rf.monthly$admin2 <- sapply(rf.monthly$admin2, apply.rename)
pp.yearly.expanded$ADM2_NAME <- sapply(pp.yearly.expanded$ADM2_NAME, 
                                       apply.rename)
p.yearly.expanded$ADMIN2NAME <- sapply(p.yearly.expanded$ADMIN2NAME, 
                                       apply.rename)

# Checking the outcomes and verifying the updates
sort(unique(cf.monthly$ADMIN2))
length(unique(cf.monthly$ADMIN2)) # 86 (used to be 90, Dire Dawa agg)

sort(unique(fp.monthly$admin2))
length(unique(fp.monthly$admin2)) # 56 (as expected)

sort(unique(cp.yearly.expanded$Zone))
length(unique(cp.yearly.expanded$Zone)) # 74 (used to be 77)

sort(unique(rf.monthly$admin2))
length(unique(rf.monthly$admin2))# 76 (as expected)

sort(unique(pp.yearly.expanded$ADM2_NAME))
length(unique(pp.yearly.expanded$ADM2_NAME)) # 84 (used to be 86, 2 YEM agg)

sort(unique(p.yearly.expanded$ADMIN2NAME))
length(unique(p.yearly.expanded$ADMIN2NAME)) # 84 (used to be 86, 2 YEM agg)

# Now that the relevant columns have been standardized, can proceed to the
# combining stage
str(cf.monthly)
str(fp.monthly)

# Also important to look at the crop category similarities between the food
# prices and crop production dataset - to have the corresponding category entry
# together

unique(fp.monthly$category)
unique(cp.yearly.expanded$Category)

# Standardizing the crop/food category names for the two datasets

# Category mapping
category.map <- list(
  "Cereals and Tubers" = c("cereals and tubers", "Cereals", "Root Crops"),
  "Vegetables and Fruits" = c("vegetables and fruits", "Fruit Crops", "Vegetables"),
  "Pulses and Nuts" = c("pulses and nuts", "Pulses"),
  "Oil and Fats" = c("oil and fats", "Oilseeds"),
  "Miscellaneous Food" = "miscellaneous food",
  "Milk and Dairy" = "milk and dairy",
  "Meat, Fish and Eggs" = "meat, fish and eggs",
  "Non-Food" = "non-food",
  "Coffee" = "Coffee",
  "Chat" = "Chat",
  "Special Crops" = c("Hops", "Sugar Cane", "Enset")
)

# Function to apply the category mapping
apply.category.mapping <- function(category, mapping) {
  for (cat in names(mapping)) {
    if (category %in% mapping[[cat]]) {
      return(cat)
    }
  }
  return(NA)  # NA if no match is found
}

# Applying to fp.monthly
fp.monthly$CropCategory <- sapply(fp.monthly$category, 
                              apply.category.mapping, 
                              mapping = category.map)

# Applying to cp.yearly.expanded
cp.yearly.expanded$CropCategory <- sapply(cp.yearly.expanded$Category, 
                                      apply.category.mapping, 
                                      mapping = category.map)
# These columns are no longer needed
fp.monthly$category <- NULL
cp.yearly.expanded$Category <- NULL

# Checking results
# We have a common set of CropCategories
unique(fp.monthly$CropCategory)
unique(cp.yearly.expanded$CropCategory)

# Exploring an issue with entries with similar grouping attributes but on
# separate rows
cp.yearly.expanded[cp.yearly.expanded$year_month == "2005-08" &
                     cp.yearly.expanded$Region == "Oromia" &
                     cp.yearly.expanded$Zone == "ARSI", ]

# Aggregating the crop production data based on the new standardized categories
cp.yearly.expanded <- aggregate(
  cbind(Number.of.holders, 
        Area.in.hectare, 
        Production.in.quintal, 
        Yqth) ~ Year + year_month + Region + Zone + CropCategory,
  data = cp.yearly.expanded,
  FUN = sum, 
  na.action = na.omit)

str(cp.yearly.expanded) # 49260 x 9
head(cp.yearly.expanded)

# Rechecking the issue identified earlier
cp.yearly.expanded[cp.yearly.expanded$year_month == "2005-08" &
                     cp.yearly.expanded$Region == "Oromia" &
                     cp.yearly.expanded$Zone == "ARSI", ]
# No duplicate entries for identical grouping factors

################################################################################

# MERGING
# Merging the conflicts dataset with the food prices data
# Renaming columns to match
names(fp.monthly)[names(fp.monthly) == "month_year"] <- "year_month"
names(cf.monthly)[names(cf.monthly) == "MONTH_YEAR"] <- "year_month"
names(cf.monthly)[names(cf.monthly) == "YEAR"] <- "year"
names(cf.monthly)[names(cf.monthly) == "ADMIN1"] <- "admin1"
names(cf.monthly)[names(cf.monthly) == "ADMIN2"] <- "admin2"
cffp.combined.monthly <- merge(cf.monthly,
                               fp.monthly,
                               by = c("year", "year_month", "admin1", "admin2"),
                               all = TRUE)
str(cffp.combined.monthly)
head(cffp.combined.monthly)
# Number of complete rows
sum(complete.cases(cffp.combined.monthly)) # 3994 complete cases

# Now merging the crop production data to this
# Each merge matches the year_month, admin1, admin2 and CropCategory
str(cp.yearly.expanded)
# Renaming columns to match for merging
names(cp.yearly.expanded)[names(cp.yearly.expanded) == "Year"] <- "year"
names(cp.yearly.expanded)[names(cp.yearly.expanded) == "Region"] <- "admin1"
names(cp.yearly.expanded)[names(cp.yearly.expanded) == "Zone"] <- "admin2"
# Merging cffp.combined.monthly with cp.yearly.expanded
cp.combined.monthly <- merge(cffp.combined.monthly, cp.yearly.expanded, 
                        by = c("year",
                               "year_month", 
                               "admin1", 
                               "admin2", 
                               "CropCategory"), 
                        all.x = TRUE)
str(cp.combined.monthly)
head(cp.combined.monthly)
# Number of complete rows
sum(complete.cases(cp.combined.monthly)) # 327 complete cases
head(cp.combined.monthly[complete.cases(cp.combined.monthly),], 100)

# Now merging the rainfall data to this
str(rf.monthly)
# Merging cp.combined.monthly with rf.monthly
rf.combined.monthly <- merge(cp.combined.monthly, rf.monthly, 
                             by = c("year", "year_month", "admin1", "admin2"), 
                             all.x = TRUE)
str(rf.combined.monthly)
head(rf.combined.monthly)
# Number of complete rows
sum(complete.cases(rf.combined.monthly)) # 302 complete cases

# Now merging the population projections data to this
str(pp.yearly.expanded)
# Aligning the column names
names(pp.yearly.expanded)[names(pp.yearly.expanded) == "Year"] <- "year"
names(pp.yearly.expanded)[names(pp.yearly.expanded) == "ADM1_NAME"] <- "admin1"
names(pp.yearly.expanded)[names(pp.yearly.expanded) == "ADM2_NAME"] <- "admin2"
# Merging rf.combined.monthly with pp.yearly.expanded
pp.combined.monthly <- merge(rf.combined.monthly, pp.yearly.expanded, 
                             by = c("year", "year_month", "admin1", "admin2"), 
                             all.x = TRUE)
str(pp.combined.monthly)
head(pp.combined.monthly)
# Number of complete rows
sum(complete.cases(pp.combined.monthly)) # 97 complete cases

# Now merging the population data to this
str(p.yearly.expanded)
# Aligning the column names
names(p.yearly.expanded)[names(p.yearly.expanded) == "YEAR"] <- "year"
names(p.yearly.expanded)[names(p.yearly.expanded) == "ADMIN1NAME"] <- "admin1"
names(p.yearly.expanded)[names(p.yearly.expanded) == "ADMIN2NAME"] <- "admin2"
# Merging pp.combined.monthly with p.yearly.expanded
p.combined.monthly <- merge(pp.combined.monthly, p.yearly.expanded, 
                             by = c("year", "year_month", "admin1", "admin2"), 
                             all.x = TRUE)
str(p.combined.monthly)
head(p.combined.monthly)
# Number of complete rows
sum(complete.cases(p.combined.monthly)) # 97 complete cases


# Now merging the currency data
str(cr.monthly)
# Merging with pp.combined.monthly
cr.combined.monthly <- merge(p.combined.monthly, cr.monthly,
                             by = c("year", "year_month"),
                             all.x = TRUE)
str(cr.combined.monthly)
head(cr.combined.monthly)

# Number of complete rows
sum(complete.cases(cr.combined.monthly)) # 97 complete cases

complete.combined.monthly <- cr.combined.monthly[
  complete.cases(cr.combined.monthly),]
head(complete.combined.monthly, 100)
nrow(cr.combined.monthly) # 22,193

# Exporting the final combined dataset for modeling purposes
write.csv(cr.combined.monthly, 
          "combined_monthly_dataset.csv", 
          row.names = FALSE)
