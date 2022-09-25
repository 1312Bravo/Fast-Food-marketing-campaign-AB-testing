library(tidyverse)
library(psych)
library(psychTools)

# 1. About ----

# Scenario 

# Plan: Fast food chain plans to add new item to its menu.
# Promoting: 3 possible marketing campaigns
# Question: Which promotion has the greatest effect on sales?
# Note: --> The new product is introduced at locations in several randomly selected markets.
#       --> Different promotion at each location.
#       --> Weekly sales of the new item for first 4 weeks are recorded.
# Goal: Evaluate AB testing results and decide which marketing strategy works the best.

# Data 

# MarketID: unique identifier for market
# MarketSize: size of market area by sales
# LocationID: unique identifier for store location
# AgeOfStore: age of store in years
# Promotion: one of three promotions that were tested
# week: one of four weeks when the promotions were run
# SalesInThousands: sales amount for a specific LocationID, Promotion, and week

data.org <- read.csv("WA_Marketing-Campaign.csv")

# 2. Summary ----

# How does the data look like ?
describeFast(data.org) # 548 observations, 7 variables, 0 missing values
headTail(data.org) # How does the data look like?

str(data.org) # Data types 
data.org <- data.org %>% mutate(Promotion = factor(Promotion, levels = 1:3),  # Promotion is a factor :)
                                MarketID = factor(MarketID), # MarketID is a factor :)
                                MarketSize = factor(MarketSize, levels = c("Small", "Medium", "Large")), # MarketSize is a factor :)  
                                LocationID = factor(LocationID), # LocationID is a factor :)
                                week = factor(week)) # week is a factor
summary(data.org)
table(data.org$Promotion) # Promotion --> 1-172, 2-188, 3-188 
table(data.org$MarketID) # Number of markets --> 10 -> not equally represented
table(data.org$LocationID);  length(table(data.org$LocationID)) # LocationID --> 137 store locations, each of them once i.e. 4 weeks :)
unique(data.org$AgeOfStore) %>% sort(); unique(data.org$AgeOfStore) %>% length() # AgeOfStore -->  1-28 but 25 unique
table(data.org$MarketSize) # MarketSize --> Large-168, Medium-320, Small-60

# Correlations
r <- lowerCor(data.org)
corPlot(r, diag = TRUE) # MarketSize ~ SalesInThousands --> Of course :)
corr.test(r)

# Tries 
table(data.org$LocationID, data.org$Promotion)






# 99. Findings ----

describeFast(data.org) 
# 548 observations, 7 variables, 0 missing values

## 99.1 SalesInThousands ----
# --> A variable that determines the quality of the campaign.

# The most basic effect of Promotion on cumulative Sales
data.org %>% 
  group_by(Promotion) %>% summarize(CumSales = sum(SalesInThousands), NumberLocations = n() / 4) %>%
  mutate(CumPercentSales = CumSales / sum(CumSales) * 100) %>%
  ggplot(aes(x = Promotion, y = CumPercentSales, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0("Nr. of locations = ", NumberLocations)), vjust = -0.8) +
  geom_label(aes(label = paste0(round(CumPercentSales, 1), "%")), vjust = 1.5) + 
  labs(x = "Promotion", y = "Cumulative sales [%]") +
  theme_bw() + theme(legend.position = "none")

# Effect of Promotion on Sales, normalized for the number of locations
# --> Effect of Promotion on Sales per average location
data.org %>% 
  group_by(Promotion) %>% summarize(CumSales = sum(SalesInThousands), NumberLocations = n() / 4) %>%
  mutate(CumPercentSales = CumSales / sum(CumSales) * 100,
         CumSales.Normalized = CumSales / NumberLocations) %>%
  mutate(CumPercentSales.Normalized = CumSales.Normalized / sum(CumSales.Normalized) * 100) %>%
  ggplot(aes(x = Promotion, y = CumPercentSales.Normalized, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0("Nr. of locations = ", NumberLocations)), vjust = -0.8) +
  geom_label(aes(label = paste0("Normalized = ", round(CumPercentSales.Normalized, 1), "% --- ",
                                "Not normalized = ", round(CumPercentSales, 1), "%")), vjust = 1.5) +
  labs(x = "Promotion", y = "Cumulative sales per average location [%]") +
  theme_bw() + theme(legend.position = "none")

# Take average and not cumulative Sales conditionally on Promotion
data.org %>% 
  group_by(Promotion) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>%
  mutate(Avg.Sales.Percent = Avg.Sales / sum(Avg.Sales) * 100) %>%
  ggplot(aes(x = Promotion, y = Avg.Sales.Percent, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = paste0(round(Avg.Sales.Percent, 1), "%")), vjust = 1.5) +
  labs(x = "Promotion", y = "Average sales per location [%]") +
  theme_bw() + theme(legend.position = "none")

# Is there a significant difference in means between proportions, not taking into account the MarketSize and number of locations?

# Parametric or non parametric test ? --> Non parametric I think :) --> Kruskal Wallis test :)
ggplot(data = data.org, aes(x = SalesInThousands)) + geom_density(fill = "grey") + facet_wrap(. ~ Promotion) + theme_bw()
shapiro.test(data.org %>% filter(Promotion == "1") %>% pull(SalesInThousands)) # Promotion = 1 --> Non normality, other two are the same :)

kruskal.test(SalesInThousands ~ Promotion, data = data.org)
# There are significant differences between groups :)
# But we don't know which pairs of groups are different.
# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(data.org$SalesInThousands, data.org$Promotion, p.adjust.method = "BH")
# All three Promotion groups are different :) But which is the best and second best?
data.org %>% group_by(Promotion) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>% arrange(desc(Avg.Sales)) %>%
  mutate(Avg.Sales.Perc = Avg.Sales / sum(Avg.Sales) * 100)
# 1 (36.1%) > 3 (34.4%) > 2 (29.4%)

Sales.Promotion.Model <- lm(SalesInThousands ~ Promotion, data = data.org)
summary(Sales.Promotion.Model)  

## 99.2 Promotion ----

table(data.org$Promotion) 
# --> 1-172, 2-188, 3-188 
# --> Variable for which we want to decide on the use (AB test).

# Note:
# Due to the different number of Promotions between each campaign, 
# we should not consider the cumulative values of SalesInThousands but the average or median.

## 99.3 MarketSize ----

table(data.org$MarketSize)
# --> Small-60, Medium-320, Large-168 
# --> An additional variable that causes "noise" in the data and may have an indirect effect.

# Are Market Size and Sales Correlated Variables?
ggplot(data = data.org, aes(x = SalesInThousands)) + geom_density(fill = "grey") + facet_wrap(. ~ MarketSize) + theme_bw()
shapiro.test(data.org %>% filter(MarketSize == "Large") %>% pull(SalesInThousands)) # No normality (for Large :))
kruskal.test(SalesInThousands ~ MarketSize, data = data.org) # Sales differ statistically significantly according to Market Size
pairwise.wilcox.test(data.org$SalesInThousands, data.org$MarketSize, p.adjust.method = "BH") # All three groups are different
data.org %>% group_by(MarketSize) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>% 
  arrange(desc(Avg.Sales)) %>%
  mutate(Avg.Sales.Percentage = Avg.Sales / sum(Avg.Sales) * 100)
# Large (70.1 or 40.9%) > Small (57.4 or 33.5%) > Medium (44.0 or 25.6%)

# Question:
# Does Promotion via MarketSize have an indirect effect? 
# That is, is there a hidden influence of MarketSize in Promotion? 
# That is, is MarketSize equally distributed between Promotion groups?

# Percentage of MarketSize per each Promotion group
promotionXsize <- data.org %>% 
  group_by(Promotion, MarketSize) %>% summarize(n = n()) %>% 
  mutate(Promotion = paste0("Promotion_", Promotion)) %>%
  pivot_wider(names_from = Promotion, values_from = n) %>%
  mutate(Promotion_1 = Promotion_1 / sum(Promotion_1) * 100,
         Promotion_2 = Promotion_2 / sum(Promotion_2) * 100,
         Promotion_3 = Promotion_3 / sum(Promotion_3) * 100)

promotionXsize %>% pivot_longer(!MarketSize, names_to = "Promotion", values_to = "PromotionPercent") %>% arrange(Promotion) %>%
  ggplot(aes(x = Promotion, y = PromotionPercent, fill = Promotion)) + 
  geom_col() + facet_wrap(. ~ MarketSize) +
  theme_bw()

# For each Promotion group, medium market size is most common and small market size the least common

# Basic, number or percentage of Promotions conditionally on MarketSize
# -> Per each MarketSize show percentage of promotions :)
sizeXpromotion <- data.org %>% 
  group_by(Promotion, MarketSize) %>% summarize(n = n()) %>% 
  mutate(Promotion = paste0("Promotion_", Promotion)) %>%
  pivot_wider(names_from = MarketSize, values_from = n)
sizeXpromotion$Small <- sizeXpromotion$Small / sum(sizeXpromotion$Small) * 100
sizeXpromotion$Medium <- sizeXpromotion$Medium / sum(sizeXpromotion$Medium) * 100
sizeXpromotion$Large <- sizeXpromotion$Large / sum(sizeXpromotion$Large) * 100

sizeXpromotion.long <- sizeXpromotion %>% pivot_longer(!Promotion, names_to = "MarketSize", values_to = "SizePercent")

ggplot(data = sizeXpromotion.long, aes(x = Promotion, y = SizePercent, fill = Promotion)) + 
  geom_col() + facet_wrap(. ~ MarketSize) +
  theme_bw()

# Large market size --> 2 > 1 > 3
# Medium market size --> 3 > 2 > 1
# Small market size --> 3 > 1 > 2

# Is there a correlation between MarketSize and Promotion?
table(data.org$MarketSize, data.org$Promotion)
chisq.test(table(data.org$MarketSize, data.org$Promotion))
# --> MarketSize and Promotion group are not significantly correlated variables
# This means that the number of campaigns does not differ between Promotionoi in terms of MarketSize.
# So when averaging Sales inside promotions we can equate MarketSize, or MarketSize does not have an significant indirect effect.
# But I still wouldn't fully believe it based on the table below.
sizeXpromotion

# Try to calculate "average" market size for each Promotion group.

# If we do not consider Promotion, what is the average Sale conditional on MarketSize.
# For this, we assume that Promotion and MarketSize are not significantly correlated variables, 
# We "proved" this above.

MarketSize.AvgSales <- data.org %>% group_by(MarketSize) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>%
  mutate(Avg.Sales.Perc = Avg.Sales / sum(Avg.Sales) * 100)

# Let's try to get rid of the influence of Market Size
# -> We normalize Sales so that the average is 1 within each market Size
data.org <- data.org %>% left_join(MarketSize.AvgSales, by = "MarketSize") %>%
  mutate(SalesInThousands.Normalized = SalesInThousands / Avg.Sales) %>%
  mutate(SalesInThousands.Normalized.Avg = SalesInThousands.Normalized * mean(SalesInThousands))

# All ok? --> YES :)
mean(data.org$SalesInThousands.Normalized.Avg)
mean(data.org$SalesInThousands)
mean(data.org$SalesInThousands.Normalized)

# So, how now does the Price differ between Promotions when there is no longer MarketSize effect?
data.org %>% 
  group_by(Promotion) %>% summarize(Avg.Sales.Normalized = mean(SalesInThousands.Normalized.Avg)) %>%
  mutate(Avg.Sales.NormalizedAvg.Percent = Avg.Sales.Normalized / sum(Avg.Sales.Normalized) * 100) %>%
  ggplot(aes(x = Promotion, y = Avg.Sales.NormalizedAvg.Percent, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = paste0(round(Avg.Sales.NormalizedAvg.Percent, 1), "%")), vjust = 1.5) +
  labs(x = "Promotion", y = "Average normalized sales per location [%]") +
  theme_bw() + theme(legend.position = "none")

# Is there a significant difference in means between proportions when normalized for MarketSize?

# Parametric or non parametric test ? --> Non parametric I think :) --> Kruskal Wallis test :)
ggplot(data = data.org, aes(x = SalesInThousands.Normalized.Avg)) + geom_density(fill = "grey") + facet_wrap(. ~ Promotion) + theme_bw()
shapiro.test(data.org %>% filter(Promotion == "1") %>% pull(SalesInThousands.Normalized.Avg)) # Promotion = 1 --> Non normality
shapiro.test(data.org %>% filter(Promotion == "2") %>% pull(SalesInThousands.Normalized.Avg)) # Promotion = 2 --> Non normality
shapiro.test(data.org %>% filter(Promotion == "3") %>% pull(SalesInThousands.Normalized.Avg)) # Promotion = 3 --> Non normality


kruskal.test(SalesInThousands.Normalized.Avg ~ Promotion, data = data.org)
# There are significant differences between groups :)
# But we don't know which pairs of groups are different.
# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(data.org$SalesInThousands.Normalized.Avg, data.org$Promotion, p.adjust.method = "BH")
# Promotion 1 = Promotion 3 & Promotion 1 != Promotion 3 & Promotion 2 != Promotion 3
data.org %>% group_by(Promotion) %>% summarize(Avg.SalesNormalized = mean(SalesInThousands.Normalized.Avg)) %>% 
  arrange(desc(Avg.SalesNormalized)) %>%
  mutate(Avg.SalesNormalized.Perc = Avg.SalesNormalized / sum(Avg.SalesNormalized) * 100)
# 1 (35.8%) = 3 (35.0%) > 2 (29.2%)

## 99.4 Others ----

table(data.org$MarketID) # MarketID --> 10 different, not equally represented
length(table(data.org$LocationID)) # LocationID --> 137 store locations, each of them once i.e. 4 weeks :) 

### 99.4.1 AgeOfStore ----
unique(data.org$AgeOfStore) %>% sort() # AgeOfStore --> 1-28, mean = 8.5, median = 7

ggplot(data = data.org, aes(x = AgeOfStore, y = SalesInThousands)) + geom_point() + geom_smooth(method = "lm") + theme_bw() # No relationship
lm(SalesInThousands ~ AgeOfStore, data = data.org) %>% summary() # No relationship

# Does AgeOfStore possibly have an indirect effect on Sales through Promotion?
data.org %>% group_by(Promotion) %>% summarize(Mean = mean(AgeOfStore), Median = median(AgeOfStore), SD = sd(AgeOfStore)) %>% arrange(desc(Mean))
# AgeOfStore seems to be distributed a bit differently between the Promotion groups.
# 3 > 1 > 2 (oldest > youngest)

# Lets check a distribution with a plot
ggplot(data = data.org, aes(x = AgeOfStore, fill = Promotion)) + geom_density(alpha  = 0.5) + theme_bw()
# Promotion 3 may have slightly older stores than 1 and 2. Let's check with a test :) Again, non normality :)
pairwise.wilcox.test(data.org$AgeOfStore, data.org$Promotion, p.adjust.method = "BH")
# All p-values are greater than 0.05 -> There are no significant differences between Promotion groups regarding AgeOfStore.

### 99.4.2 week ----

# Effect of week variable on normalized Sales -> We do not consider the Promotion group
data.org %>% group_by(week) %>% summarize(Avg.Normalized.Sales = mean(SalesInThousands.Normalized.Avg)) %>%
  ggplot(aes(x = week, y = Avg.Normalized.Sales, fill = week)) + geom_col() +
  geom_label(aes(label = round(Avg.Normalized.Sales, 1)), vjust = 1.5) +
  theme_bw() + theme(legend.position = "none")
# There are no average differences between weeks in terms of normalized Sales

# What about not normalized Sales?
data.org %>% group_by(week) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>%
  ggplot(aes(x = week, y = Avg.Sales, fill = week)) + geom_col() +
  geom_label(aes(label = round(Avg.Sales, 1)), vjust = 1.5) +
  theme_bw() + theme(legend.position = "none")
# There are no differences either, of course the values are the same because we normalized it :)

# Effect of week variable on normalized Sales conditional on Promotion group
data.org %>% group_by(Promotion, week) %>% summarize(Avg.Normalized.Sales = mean(SalesInThousands.Normalized.Avg)) %>%
  ggplot(aes(x = week, y = Avg.Normalized.Sales, fill = week)) + geom_col() + facet_wrap(. ~ Promotion) + 
  geom_label(aes(label = round(Avg.Normalized.Sales, 1)), vjust = 1.5) +
  theme_bw() + theme(legend.position = "none")
# There are minimal differences in weeks between all Promotions groups

data.org %>% group_by(Promotion, week) %>% summarize(Avg.Normalized.Sales = mean(SalesInThousands.Normalized.Avg)) %>%
  ggplot(aes(x = Promotion, y = Avg.Normalized.Sales, fill = Promotion)) + geom_col() + facet_wrap(. ~ week) + 
  geom_label(aes(label = round(Avg.Normalized.Sales, 1)), vjust = 1.5) +
  theme_bw() + theme(legend.position = "none")

## 99.5 Hypothesis testing ----

# Based on the tests, we can conclude:

# Promotion: 
# Ignoring all other variables & Based on the percent of total Sales and Average Sales: 1 (36.1% or 58.1) > 3 (34.4% or 55.4) > 2 (29.4% or 47.3)
# Normalized for Market Size &  Based on the percent of total Sales and Average Sales: 1 (35.8% or 57.5) > 3 (35.0% or 56.2) > 2 (29.2% or 47.0)

# MarketSize:
# Ignoring all other variables & Based on the percent of total Sales and Average Sales: Large (70.1 or 40.9%) > Small (57.4 or 33.5%) > Medium (44.0 or 25.6%)
# MarketSize and Promotion group are not significantly correlated variables
# This means that the number of campaigns does not differ between Promotion in terms of MarketSize.
# So when averaging Sales inside promotions we can equate MarketSize, or MarketSize does not have an significant indirect effect.
# But I still wouldn't fully believe it based on the tables shown ...

# AgeOfStore:
# There is no relationship between AgeOfStore and Sales
# There are no significant differences between Sales regarding AgeOfStore.

# week:
# There are no differences between Sales regarding weeks.
# There are minimal differences in weeks between all Promotions groups