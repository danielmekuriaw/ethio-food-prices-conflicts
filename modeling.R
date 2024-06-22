# Title: Exploring different models
# Author: Daniel Mekuriaw
# Last Updated: May 01, 2024

# Reading dataset to perform the modeling on
monthly.data <- read.csv("combined_monthly_dataset.csv")
str(monthly.data)
head(monthly.data)

### WORKING WITH THE COMPLETE CASE
# Subsetting the complete cases
monthly.data.cc <- monthly.data[complete.cases(monthly.data),]
str(monthly.data.cc)

# Building the model
# Starting with predicting usdprice using every other variable in the dataset
lm1 <- lm(usdprice ~ ., data = monthly.data.cc)
summary(lm1)

# pricetype column seems to have the most effect
# this is reasonable since Retail vs Wholesale has huge impact on the prices
# So we can proceed to split the two into two and model them separately
monthly.data.cc.wholesale <- monthly.data.cc[
  monthly.data.cc$pricetype == "Wholesale",]
unique(monthly.data.cc.wholesale$pricetype)

monthly.data.cc.retail <- monthly.data.cc[
  monthly.data.cc$pricetype == "Retail",]
unique(monthly.data.cc.retail$pricetype)

# Wholesale column is no longer useful for the two datasets as they are split
# based on this column
monthly.data.cc.wholesale$pricetype <- NULL
monthly.data.cc.retail$pricetype <- NULL

# Checking the state of the unit column
unique(monthly.data.cc.wholesale$unit)
table(monthly.data.cc.wholesale$unit) # only 100 KG
monthly.data.cc.wholesale$unit <- NULL # Removing col b/se it can only be 100KG
unique(monthly.data.cc.retail$unit) # Can 100 KG or KG
table(monthly.data.cc.retail$unit)

# Looking at the basic characteristics of the two splits of the complete case
str(monthly.data.cc.wholesale)
head(monthly.data.cc.wholesale)
str(monthly.data.cc.retail)
head(monthly.data.cc.retail)

# Modeling for each of the two datasets separately
# Error encountered when trying to model with the new wholesale data
# Happens to be because admin1 only has 1 value - "Oromia"
# CropCategory also only has 1 value - "Cereal and Tubers"
# Therefore, the two columns can be removed from the dataset before modeling
unique(monthly.data.cc.wholesale$admin1)
monthly.data.cc.wholesale$admin1 <- NULL
unique(monthly.data.cc.wholesale$CropCategory) # only Cereal and Tubers
monthly.data.cc.wholesale$CropCategory <- NULL

# Modelling wholesale
lm_wholesale_1 <- lm(usdprice ~ ., data = monthly.data.cc.wholesale)
summary(lm_wholesale_1) # Singularity issues, overfitting, lack of df

# Simplifying the model to address the issues
lm_wholesale_2 <- lm(usdprice ~ year + 
                       admin2 + 
                       TOTAL_FATALITIES +
                       Yqth +
                       mean.rainfall +
                       POP +
                       Price, 
                     data = monthly.data.cc.wholesale)
summary(lm_wholesale_2) # Least sig: TOTAL_FATALITIES and mean.rainfall

# Trying to improve the model even further
lm_wholesale_3 <- lm(usdprice ~ year + 
                       admin2 + 
                       Yqth +
                       POP +
                       Price, 
                     data = monthly.data.cc.wholesale)
summary(lm_wholesale_3) # Yqth is still the least sig

# Coming back to the retail data
# Modelling retail
lm_retail_1 <- lm(usdprice ~ ., data = monthly.data.cc.retail)
summary(lm_retail_1) # unitKG is the most significant variable

# Doing it without the unit column
# Highly expected that the unit would matter for the price
lm_retail_2 <- lm(usdprice ~ . - unit, data = monthly.data.cc.retail)
summary(lm_retail_2) # year_month2009-04 is the most significant but still have 
                     # lots of NA coefs
                     # Also see similar issues, such as use of too many predictors

# Simplifying model
lm_retail_3 <- lm(usdprice ~ year + 
                       admin2 + 
                       TOTAL_FATALITIES +
                       Yqth +
                       mean.rainfall +
                       POP +
                       Price, 
                     data = monthly.data.cc.retail)
summary(lm_retail_3) # TOTAL_FATALITIES, mean.rainfall, POP appear to be largely
                     # insignificant

# Simplifying model even further
lm_retail_4 <- lm(usdprice ~ year + 
                    admin2 + 
                    Yqth +
                    Price, 
                  data = monthly.data.cc.retail)
summary(lm_retail_4) # Has improved, but still no significant predictors

# All of these suggest that with only the complete cases following merging, it 
# would be very difficult to develop a robust model.
# So it would be helpful to explore imputations to improve the number of
# available complete cases in the datasets

str(monthly.data.cc.retail) # 50 obs. of 31 variables
str(monthly.data.cc.wholesale) # 47 obs. of 30 variables

################################################################################

### IMPUTATIONS to expand the amount of complete cases
str(monthly.data) # 22,193 obs. of 32 variables
head(monthly.data)

# Looking at the characteristics of the dataset
summary(monthly.data)
sapply(monthly.data, function(x) sum(is.na(x))) # Counting NAs for each column
# year, year_month, admin1, admin2 - No NAs
# CropCategory - 2553 NAs - DONE
# TOTAL_FATALITIES and the different conflict types - 15646 NAs
# Latitude, longitude, unit, pricetype, usdprice - 2553 NAs - DONE
# Different data from crop production dataset - 17752 NAs
# mean.rainfall - 15266 NAs # Can do some imputations
# rainfall_lag - 15868 NAs # Can do some imputations
# Population projection and POP - 16779 NAs
# Price, Open, High, Low - 10 NAs

# Given these observations and the preliminary modeling results using the complete
# cases, the first step would be to trim down some of the predictors as well as
# observations since it wouldn't be expected for all of them to be equally 
# significant
# After that, imputations can be done to expand the number of available complete
# cases.

# Removing all rows with NAs for CropCategory, unit, pricetype or usdprice
# The model would not work for any entries with NAs for these columns and they
# can also not be calculated from other datasets or be imputed
monthly.data <- monthly.data[!is.na(monthly.data$CropCategory),]
sapply(monthly.data, function(x) sum(is.na(x))) # Now all of those cols not NAs

# Imputation for mean.rainfall
yearly.mean.rainfall <- ave(monthly.data$mean.rainfall,
                            monthly.data$year,
                            FUN = function(x) mean(x, na.rm = TRUE))
# Replacing the NA values in mean.rainfall with the yearly averages
monthly.data$mean.rainfall[is.na(monthly.data$mean.rainfall)] <- 
  yearly.mean.rainfall[is.na(monthly.data$mean.rainfall)]
sapply(monthly.data, function(x) sum(is.na(x))) # Still 11,533 NAs

# Replacing any remaining NAs with the overall average of mean.rainfall
overall.mean.rainfall <- mean(monthly.data$mean.rainfall, na.rm = TRUE)
monthly.data$mean.rainfall[is.na(monthly.data$mean.rainfall)] <- 
  overall.mean.rainfall
sapply(monthly.data, function(x) sum(is.na(x))) # 0 NAs for mean.rainfall

# rainfall_lag repeats values from mean.rainfall and has more NAs
# From the preliminary investigations, it didn't seem like it added much
# Therefore, it would be reasonable to remove this variable
monthly.data$rainfall_lag <- NULL

# Also won't need the Open, High and Low columns from the currency data, Price
# is sufficient for this analysis
monthly.data$Open <- NULL
monthly.data$High <- NULL
monthly.data$Low <- NULL

# Imputations for the remaining columns
# Defining the columns to impute
columns.to.impute <- c("Female_0.19_Population", "Female_20.59_Population", 
                       "Female_60._Population", "Male_0.19_Population", 
                       "Male_20.59_Population", "Male_60._Population", "POP",
                       "Number.of.holders", "Area.in.hectare", 
                       "Production.in.quintal", "Yqth", "TOTAL_FATALITIES",
                       "Strategic.developments", "Explosions.Remote.violence",
                       "Protests", "Violence.against.civilians", "Battles", 
                       "Riots")

# Applying imputation for each of the remaining columns column
# Tries to find the yearly average for each column and impute the NA values with
for (col in columns.to.impute) {
  # Calculating the yearly average for the current column
  yearly.avg <- ave(monthly.data[[col]], 
                    monthly.data$year, 
                    FUN = function(x) mean(x, na.rm = TRUE))
  
  # Imputing NA values with the yearly average
  monthly.data[[col]][is.na(monthly.data[[col]])] <- 
    yearly.avg[is.na(monthly.data[[col]])]
}
sapply(monthly.data, function(x) sum(is.na(x))) # Still 8758 NAs for those

# Applying more imputations
# This time it calculates the overall average and replaces it with any remaining
for (col in columns.to.impute) {
  # Calculating the overall mean for the current column
  mean.value <- mean(monthly.data[[col]], na.rm = TRUE)
  
  # Replacing remaining NA values with the overall mean
  monthly.data[[col]][is.na(monthly.data[[col]])] <- mean.value
}
sapply(monthly.data, function(x) sum(is.na(x))) # Only price has NAs

# Price still 4 NAs
monthly.data[is.na(monthly.data$Price),] # Original data didn't have it either
# Manually updating it after doing research: https://exchangerates.org/usd/etb/in-2003
monthly.data[monthly.data$year_month == "2003-02",] <- 8.43
sapply(monthly.data, function(x) sum(is.na(x))) # Now, no column has NAs

# Checking the results
head(monthly.data)
str(monthly.data)

table(monthly.data$pricetype) # Retail: 16,226; # Wholesale: 3,410

# Exporting Imputed

# Improving the naming of some of the variables
names(monthly.data)[names(monthly.data) == "CorpCategory"] <- "crop_category"
names(monthly.data)[names(monthly.data) == "TOTAL_FATALITIES"] <- 
  "total_fatalities"
names(monthly.data)[names(monthly.data) == "Yqth"] <- "yield_qt_hectare"
names(monthly.data)[names(monthly.data) == "POP"] <- "Population"
names(monthly.data)[names(monthly.data) == "Price"] <- "USD_to_ETB_rate"
names(monthly.data)[names(monthly.data) == "usdprice"] <- "food_price_USD"
# Confirming changes
str(monthly.data)

################################################################################

# Back to modeling now that the imputations are done
lm_impute_1 <- lm(food_price_USD ~ ., 
                  data = monthly.data)
summary(lm_impute_1)
# Observations: all year_month values do not seem to be signficant
# There are some NA coefficients
# There are some levels within admin1 and admin2
# Some crop categories are more significant than others
# Strategic.developments and Protests are not significant, neither are Riots
# The projected populations as well as the actual population are also insignificant

# Improving model selection through removing the least significant predictors
# using the results of the previous model
lm_impute_2 <- lm(food_price_USD ~. - year_month
                  - Strategic.developments
                  - Protests
                  - Riots
                  - Female_0.19_Population
                  - Female_20.59_Population
                  - Female_60._Population
                  - Male_0.19_Population
                  - Male_20.59_Population
                  - Male_60._Population, 
                  data = monthly.data)
summary(lm_impute_2)
# Observations
# Admin1 regions are now not as significant. Their p-values are consistently
# higher than 0.44
# There are still some NA coefficients
# Some admin2 values are highly significant predictors
# Some crop categories are still more signficant than others
# total_fatalities are not very signficant, although also not the least significant
# Explosions.Remote.violence is not significant

# Improving the model further
lm_impute_3 <- lm(food_price_USD ~. - year_month
                  - Strategic.developments
                  - Protests
                  - Riots
                  - Female_0.19_Population
                  - Female_20.59_Population
                  - Female_60._Population
                  - Male_0.19_Population
                  - Male_20.59_Population
                  - Male_60._Population
                  - admin1
                  - total_fatalities
                  - Explosions.Remote.violence, 
                  data = monthly.data)

summary(lm_impute_3)
# Observations
# Admin2 values are not as significant anymore
# Their p-value is consistently greater than 0.5
# There are now only 3 variables with NA coefficients
# The year column is also one of the least signficant ones (pval = 0.74)

# Further improving the model
lm_impute_4 <- lm(food_price_USD ~. - year_month
                  - Strategic.developments
                  - Protests
                  - Riots
                  - Female_0.19_Population
                  - Female_20.59_Population
                  - Female_60._Population
                  - Male_0.19_Population
                  - Male_20.59_Population
                  - Male_60._Population
                  - admin1
                  - total_fatalities
                  - Explosions.Remote.violence
                  - admin2
                  - year,
                  data = monthly.data)

summary(lm_impute_4)
# Observations
# In this case, we are left with mostly significant variables
# The only relatively insignificant and stands out is Population with a p-val of
# 0.14
# The multiple R-sq has gone down while the p-value is still very small
# It still has two columns with NA coefficients

lm_impute_5 <- lm(food_price_USD ~. - year_month
                  - Strategic.developments
                  - Protests
                  - Riots
                  - Female_0.19_Population
                  - Female_20.59_Population
                  - Female_60._Population
                  - Male_0.19_Population
                  - Male_20.59_Population
                  - Male_60._Population
                  - admin1
                  - total_fatalities
                  - Explosions.Remote.violence
                  - admin2
                  - year
                  - Population,
                  data = monthly.data)
summary(lm_impute_5)
# Observations
# Now, have a model where each variable/column significantly contributes
# to the prediction.
# However, still have two variables with NA coefficients and we have a degrees
# of freedom of 19614, which is large.
# The multiple R-sq is 0.59 while the p-value is well less than 0.05.

par(mfrow=c(2,2))
plot(lm_impute_5)

# Attempting to address the missing coefficients issue
# Splitting the data based on the pricetype values (wholesale vs retail)
monthly.data.retail <- monthly.data[monthly.data$pricetype == "Retail",]
monthly.data.retail$pricetype <- NULL # Column no longer needed
str(monthly.data.retail) # 16226 observations
table(monthly.data.retail$unit) # Has different types of units

monthly.data.wholesale <- monthly.data[monthly.data$pricetype == "Wholesale",]
monthly.data.wholesale$pricetype <- NULL # Column no longer needed
str(monthly.data.wholesale)  # 3410 observations
table(monthly.data.wholesale$unit) # Only 100 KG, so may as well remove the col
monthly.data.wholesale$unit <- NULL

# Modeling for retail
lm_impute_retail_1 <- lm(food_price_USD ~ ., 
                         data = monthly.data.retail)
summary(lm_impute_retail_1)
# Observations
# The following columns are the least signficant ones: Strategic.developments,
# Protests, Riots, Female_0.19_Population, Female_20.59_Population, 
# Male_0.19_Population, Male_20.59_Population
# There are also several variables that have NA coefficients
# Some year_month values seem to be very significant, while others not so much
# R-sq = 0.6443, p-val < 0.05

# Improving the model further by excluding the least significant variables
lm_impute_retail_2 <- lm(food_price_USD ~ . - Strategic.developments
                         - Protests
                         - Riots
                         - Female_0.19_Population
                         - Female_20.59_Population
                         - Male_0.19_Population
                         - Male_20.59_Population, 
                         data = monthly.data.retail)
summary(lm_impute_retail_2)
# Observations
# There is no one variable that stands out as the least significant among all
# However, there are a different levels of several categorical variables that
# have very high p-values. Those same categorical variables include other 
# highly signifcant levels with very low p-values.
# R-sq = 0.6443, p-value < 0.05
# It still includes several variables whose coefficients are NAs.
# It also has a very large degrees of freedom with 15949.
# Looking at the outputs, the significant year_month levels seem to be consecutive
# There aren't any dispersed or just randomly significant year_month levels in
# the order that they are presented. This suggests to me that year may be a more
# significant factor and not year_month. Therefore, it would be reasonable to
# try and fit the model without the year_month variable.

# Trying to improve the model another time
lm_impute_retail_3 <- lm(food_price_USD ~ . - Strategic.developments
                         - Protests
                         - Riots
                         - Female_0.19_Population
                         - Female_20.59_Population
                         - Male_0.19_Population
                         - Male_20.59_Population
                         - year_month, 
                         data = monthly.data.retail)
summary(lm_impute_retail_3)
# Observations
# R-sq = 0.6336, p-val < 0.05
# Year is not as significant anymore, one of the least significant ones
# Some admin1, admin2 and cropcategory levels are very significant while some of
# their other ones are not very much so.
# Other not significant columns: Explosions.Remote.violence, 
# Violence.against.civilians, Production.in.quintal

# Improving model further with these observations
lm_impute_retail_4 <- lm(food_price_USD ~ . - Strategic.developments
                         - Protests
                         - Riots
                         - Female_0.19_Population
                         - Female_20.59_Population
                         - Male_0.19_Population
                         - Male_20.59_Population
                         - year_month
                         - year
                         - Explosions.Remote.violence
                         - Violence.against.civilians
                         - Production.in.quintal, 
                         data = monthly.data.retail)
summary(lm_impute_retail_4)
# Observations
# R-sq = 0.6335, p-value < 0.05
# 16150 df
# Different levels of categorical variables that are significant and some that
# are not

par(mfrow=c(2,2))
plot(lm_impute_retail_4)

# Modeling for wholesale
lm_impute_wholesale_1 <- lm(food_price_USD ~ ., 
                         data = monthly.data.wholesale)
summary(lm_impute_wholesale_1)
# Observations
# R-sq = 0.928, p-val < 0.05
# Some not-so-significant values: total_fatalities, Strategic.developments, 
# Protests, Violence.against.civilians, Riots, Number.of.holders, 
# yield_qt_hectare, mean.rainfall, Female_0.19_Population, Male_0.19_Population
# It has several variables with missing/NA coefficients

# Improving the model with these observations
lm_impute_wholesale_2 <- lm(food_price_USD ~ . - total_fatalities
                            - Strategic.developments
                            - Protests
                            - Violence.against.civilians
                            - Riots
                            - Number.of.holders
                            - yield_qt_hectare
                            - mean.rainfall
                            - Female_0.19_Population
                            - Male_0.19_Population, 
                            data = monthly.data.wholesale)
summary(lm_impute_wholesale_2)
# Observations
# R-sq = 0.9278, 3114 degrees of freedom, p-value < 0.05
# No single variable that is not-so-significant
# There are levels of some categorical variables that are insignificant, but
# the same variables include levels that are highly significant

par(mfrow=c(2,2))
plot(lm_impute_wholesale_2)
