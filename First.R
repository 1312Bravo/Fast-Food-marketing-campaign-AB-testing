library(tidyverse)
library(psych)
library(psychTools)

# MAIN --> https://www.kaggle.com/datasets/chebotinaa/fast-food-marketing-campaign-ab-test?resource=download
# HELP THIS --> https://www.kaggle.com/code/chebotinaa/a-b-testing-of-fast-food-marketing-campaign
# HELP THIS --> https://rpubs.com/ksdwivedy/finalRProject
# HELP OTHER --> https://www.kaggle.com/code/ekrembayar/a-b-testing-step-by-step-hypothesis-testing

# 1. About ----

## 1.1. Scenario ----

# Plan: Fast food chain plans to add new item to its menu.
# Promoting: 3 possible marketing campaigns
# Question: Which promotion has the greatest effect on sales?
# Note: --> The new product is introduced at locations in several randomly selected markets.
#       --> Different promotion at each location.
#       --> Weekly sales of the new item for first 4 weeks are recorded.
# Goal: Evaluate AB testing results and decide which marketing strategy works the best.

## 1.2 Data ----

# MarketID: unique identifier for market
# MarketSize: size of market area by sales
# LocationID: unique identifier for store location
# AgeOfStore: age of store in years
# Promotion: one of three promotions that were tested
# week: one of four weeks when the promotions were run
# SalesInThousands: sales amount for a specific LocationID, Promotion, and week


data.org <- read.csv("WA_Marketing-Campaign.csv")

# 2. EDA Raw ----
# https://cran.r-project.org/web/packages/psych/vignettes/intro.pdf

## 2.1 Data summary ----

describeFast(data.org) 
# 548 observations
# 7 variables
# 0 missing values

# How does the data look like?
headTail(data.org)

# Data types 
str(data.org)
data.org <- data.org %>% mutate(Promotion = factor(Promotion, levels = 1:3),  # Promotion is a factor :)
                                MarketID = factor(MarketID), # MarketID is a factor :)
                                MarketSize = factor(MarketSize, levels = c("Small", "Medium", "Large")), # MarketSize is a factor :)  
                                LocationID = factor(LocationID), # LocationID is a factor :)
                                week = factor(week) # week is a factor
                                )

# Summary
summary(data.org)
table(data.org$Promotion) # Promotion --> 1-172, 2-188, 3-188 
table(data.org$MarketID) # Number of markets --> 10 -> not equally represented
table(data.org$LocationID);  length(table(data.org$LocationID)) # LocationID --> 137 store locations, each of them once i.e. 4 weeks :) 
table(data.org$MarketSize) # MarketSize --> Large-168, Medium-320, Small-60

# AgeOfStore
str(data.org$AgeOfStore) # -> integer
unique(data.org$AgeOfStore) %>% length() 
unique(data.org$AgeOfStore) %>% sort() # 1-28 but 25 unique

# Data description
data.desc <- describe(data.org) 
data.desc
# Data description by group -> Group variable = Promotion :)
data.desc.group <- describeBy(data.org, data.org$Promotion)
data.desc.group

# Cleaned data --> Not needed here :) --> Change everything greater than 9 to NA
# cleaned <- scrub(data.org, max = 9)

## 2.2 Data inside ----

# Patterns? --> SPLOM (Scatter Plot Matrix)
pairs(data.org)
pairs.panels(data.org)
# Do not see anything :(

# Correlations
r <- lowerCor(data.org)
corPlot(r, diag = TRUE)
# MarketSize ~ SalesInThousands --> Of course :)
corr.test(r)

# 3. Variable relationships ----

## 3.1 Sales ----

# Linear model? -> Only Age is integer :) --> Linear or no relationship?
ggplot(data = data.org, aes(x = AgeOfStore, y = SalesInThousands)) + geom_point() + geom_smooth(method = "lm") # No relationship :)

# Linear model, lets try (withoud Location and Market ID)
sales.full.model <- lm(SalesInThousands ~ MarketSize + AgeOfStore + week , data = data.org)
par(mfrow=c(2,2)); plot(sales.full.model); par(mfrow=c(1,1)) # Well, I'd say its ok - "all" are factor :)
summary(sales.full.model)
# Only MarketSize is significant 
# R2 = 0.49 :)

# Sales ~ MarketSize 
lm(SalesInThousands ~ MarketSize, data = data.org) %>% summary()
# --> Large > Medium > Small --> Of course :)

ggplot(data = data.org, aes(x = LocationID, y = SalesInThousands)) + geom_point() + facet_wrap(.~MarketSize)
ggplot(data = data.org, aes(x = SalesInThousands, fill = MarketSize)) + geom_density(alpha = 0.5) + facet_wrap(.~Promotion)


## 3.2 Promotion ~ MarketSize ----

ggplot(data = data.org, aes(x = Promotion, fill = Promotion)) + geom_bar() + facet_wrap(.~MarketSize) + theme_bw()
table(data.org$MarketSize, data.org$Promotion)

# Small: 2 < 1 < 3
# Medium: 1 < 2 < 3
# Large: 3 < 1 < 2

# If we assume: Small = 1x, Medium = 2x, Large = 3x 
data.org <- data.org %>% mutate(MarketSizeX = ifelse(MarketSize == "Small", 1, ifelse(MarketSize == "Medium", 2, 3)))
# Average / median / sd of marketSize for each promotion
data.org %>% group_by(Promotion) %>% summarize(Mean = mean(MarketSizeX), Median = median(MarketSizeX), SD = sd(MarketSizeX))

# Promotion  Mean Median    SD
# 1          2.21     2  0.633
# 2          2.26     2  0.602
# 3          2.13     2  0.607

promotionXsize <- data.org %>% 
  group_by(Promotion, MarketSize) %>% summarize(n = n()) %>% 
  mutate(Promotion = paste0("Promotion_", Promotion)) %>%
  pivot_wider(names_from = Promotion, values_from = n) %>%
  mutate(Promotion_1 = Promotion_1 / sum(Promotion_1) * 100,
         Promotion_2 = Promotion_2 / sum(Promotion_2) * 100,
         Promotion_3 = Promotion_3 / sum(Promotion_3) * 100)

sizeXpromotion <- data.org %>% 
  group_by(Promotion, MarketSize) %>% summarize(n = n()) %>% 
  mutate(Promotion = paste0("Promotion_", Promotion)) %>%
  pivot_wider(names_from = MarketSize, values_from = n)

sizeXpromotion$Small <- sizeXpromotion$Small / sum(sizeXpromotion$Small) * 100
sizeXpromotion$Medium <- sizeXpromotion$Medium / sum(sizeXpromotion$Medium) * 100
sizeXpromotion$Large <- sizeXpromotion$Large / sum(sizeXpromotion$Large) * 100

sizeXpromotion1 <- sizeXpromotion %>% pivot_longer(!Promotion, names_to = "MarketSize", values_to = "SizePercent")

ggplot(data = sizeXpromotion1, aes(x = MarketSize, y = SizePercent, fill = MarketSize)) + geom_col() + facet_wrap(. ~ Promotion) + theme_bw()
sizeXpromotion

## 3.98 Promotion ~ Sales ----

error.bars(data.org %>% select(SalesInThousands, Promotion), group = "Promotion", ylab = "SalesInThousands", xlab = "Promotion")
data.org %>% group_by(Promotion) %>% summarize(Mean = mean(SalesInThousands), Median = median(SalesInThousands), SD = sd(SalesInThousands))
ggplot(data = data.org, aes(x = SalesInThousands, fill = Promotion)) + geom_density(alpha = .5)

# Promotion  Mean Median   SD
# 1          58.1   55.4  16.6
# 2          47.3   45.4  15.1
# 3          55.4   51.2  16.8

# Number of Sales depending on Promotion: 1 > 2 > 3 - Same distribution

## 3.99 AgeOfStore ----

# Promotion ~ AgeOfStore 
error.bars(data.org %>% select(AgeOfStore, Promotion), group = "Promotion", ylab = "AgeOfStore", xlab = "Promotion")
data.org %>% group_by(Promotion) %>% summarize(Mean = mean(AgeOfStore), Median = median(AgeOfStore), SD = sd(AgeOfStore))
ggplot(data = data.org, aes(x = AgeOfStore, fill = Promotion)) + geom_density(alpha = .5)

# Promotion  Mean Median    SD
# 1          8.28      6  6.64
# 2          7.98      7  6.60
# 3          9.23      8  6.65

# AgeOfStore depending on Promotion: 3 > 1 > 2
# Similar distribution - Promotion 3 has more in the older stores 

# AgeOfStore ~ Sales
# -> Nothing
ggplot(data = data.org, aes(x = AgeOfStore, y = SalesInThousands)) + geom_point()



error.bars(data.org %>% select(MarketSize, Promotion), group = "Promotion", ylab = "MarketSize", xlab = "Promotion")
error.bars(data.org %>% select(week, Promotion), group = "Promotion", ylab = "week", xlab = "Promotion")





























