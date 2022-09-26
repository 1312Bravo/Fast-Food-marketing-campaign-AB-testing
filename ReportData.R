source("Script.R")

# 1. About ----

# ...................................................

# Scenario 

# Plan: Fast food chain plans to add new item to its menu.
# Promoting: 3 possible marketing campaigns
# Question: Which promotion has the greatest effect on sales?
# Note: --> The new product is introduced at locations in several randomly selected markets.
#       --> Different promotion at each location.
#       --> Weekly sales of the new item for first 4 weeks are recorded.
# Goal: Evaluate AB testing results and decide which marketing strategy works the best.

# ...................................................

# Data -> 548 observations, 7 variables, 0 missing values

# MarketID: unique identifier for market ---> 10 different, not equally represented
# MarketSize: size of market area by sales ---> Large-168, Medium-320, Small-60
# LocationID: unique identifier for store location ---> 137 store locations, each of them once i.e. 4 weeks :)
# AgeOfStore: age of store in years ---> -->  1-28 but 25 unique
# Promotion: one of three promotions that were tested ---> 1-172, 2-188, 3-188 
# week: one of four weeks when the promotions were run
# SalesInThousands: sales amount for a specific LocationID, Promotion, and week 

# ..................................................

# Interest: 
# ---> SalesInThousands - a variable that determines the quality of the campaign.
# ---> Promotion - the only variable that should affect SalesInThousands that we analyze. 
# ---> Other variables are noises that we need to properly account for.


# 2. Basic ----

# Initial insight:
# The most basic effect of Promotion on cumulative Sales.
# --> Without considering other variables effect :)
Promotion.Basic
# Cumulative % of Sales -> Promotion [3] > Promotion [1] > Promotion [2]

# 3. weeks ----
# Before anything else, let's look at weeks to see if it needs to be considered within everything else.

# Effect of week variable on Sales -> We do not consider the Promotion group or anything else.
Weeks.Basic
# There are no differences between weeks in terms of Sales.

# Test: Are there differences in Sales depending only on weeks?
# Kruskal Wallis test 
Test.Weeks
# No relationship between week and Sales.

# Effect of week variable on Sales conditional on Promotion group.
Weeks.Promotion
# There are minimal differences in weeks within all Promotions groups.

# Test: Are there differences in Sales depending only on weeks
# Promotion 1: Kruskal Wallis test
Test.Weeks_1 # No relationship between week and Sales with Promotion 1
# Promotion 2: Kruskal Wallis test
Test.Weeks_2 # No relationship between week and Sales with Promotion 2
# Promotion 3: Kruskal Wallis test
Test.Weeks_3 # No relationship between week and Sales with Promotion 3

# 4. LocationID ----

# Regarding LocationID, do we have any outliers that could greatly increase Sales, even conditionally per week?
Location.outlier.Promotion 
# No outliers :)

# We Might to take number of Locations into accounts (LocationID)
Locations.PromotionGroup

# Effect of Promotion on cumulative Sales taking number of locations into account 
# --> Per Average location :)
Promotion.AvgLocation
# Average % of Sales per Location -> Promotion [1] > Promotion [3] > Promotion [2]

# Basic and number of Locations normalized:
Promotion.Basic.AvgLocation

# Test: Is there a difference in average Sales between Promotion groups
# -> Average - Taking into account number of locations per Promotion group :)
# -> Not taking into account weeks - cumulative all 4 weeks per Location :)

# Kruskal Wallis test 
Kruskal.Promotion.LocationNr
# There are significant differences in Sales between Promotion groups - But we don't know which pairs of groups are different.

# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
Pairwise.Wilcox.Promotion.LocationNr
# 1 = 3 & 1 != 2 & 2 != 3
Sales.Promotion.Location.Nr
# Promotion [1] = Promotion [3] > Promotion [2]


# 5. MarketSize ----
# Small-60, Medium-320, Large-168 

# Average number of Sales in 4 weeks conditional on MarketSize
MarketSize.Sales
# Large market > Small market > Medium market 

# Test: Is there a difference in average Sales between Market sizes
# -> Average - Taking into account number of locations per Market size :)
# -> Not taking into account weeks - cumulative all 4 weeks together and then average :)

# Kruskal Wallis test 
Kruskal.MarketSize
# There are significant differences in Sales between Market sizes - But we don't know which pairs of groups are different.

# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
Pairwise.Wilcox.MarketSize
# 1 != 2 != 3
Sales.MarketSize
# Large > Small > Medium

# How is MarketSize distributed among Promotion groups?
# That is, is there a hidden influence of MarketSize in Promotion? 
promotionXsize
# For each Promotion group, medium market size is most common and small market size the least common.
# However, the distribution is not the same for all Promotion groups.

# Test: Is there a difference in MarketSize distribution between Promotion groups?
data.Size.Promotion.Table
chisq.test(data.Size.Promotion.Table)
# MarketSize and Promotion group are not significantly correlated variables
# This means that the number of campaigns does not differ between Promotion group in terms of MarketSize.

# Addition:
# However, if we do not totally believe this test 
# Let's try to get rid of the influence of MarketSize
# We normalize Sales so that the average is 1 within each market Size

# We normalize so that Sales within Small will be of the same value as within Large MarketSize. 
# In this way, we can simply add or average the Sales within Promotion groups, 
# without fearing that the differences are the result of differences between the MakretSize distributions within Promotion groups, 
# which we know affect Sales.
# Now we can compare Sales by Promotion groups directly, without the possible influence of MarketSize.
MarketSize.Normalized


# 6. AgeOfStore ----

# Is there a relationship between AgeOfStore and Sales?
AgeOfStoreBasic
AgeOfStoreLinear
# No (linear) relationship

# Is there a relationship between AgeOfStore and Promotion group?
AgeOfStorePromotion
# Test - Kruskal Wallis test 
Kruskal.AgeOfStore.Promotion
# There are significant differences in Ages of stores between Promotion groups.


# 5. MarketID

# Regarding MarketID, do we have any outliers that could greatly increase Sales, even conditionally per MarketSize?
MarketID.outliers
# If we look at Sales by Promotion conditional on MarketID, we see that Sales are roughly equally distributed between Promotions at each MarketId 
# and also follow the findings based on MarketSize.
# There are therefore no outliers or MarketID influence that would affect Sales and not be detected in Promotion.












