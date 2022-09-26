library(tidyverse)
library(psych)
library(psychTools)
data.org <- read.csv("WA_Marketing-Campaign.csv")

# 1. Data Summary ----

describeFast(data.org) # 548 observations, 7 variables, 0 missing values
headTail(data.org) # How does the data look like?
summary(data.org)

table(data.org$Promotion)
table(data.org$MarketID) 
table(data.org$LocationID);  length(table(data.org$LocationID)) 
unique(data.org$AgeOfStore) %>% sort(); unique(data.org$AgeOfStore) %>% length() 
table(data.org$MarketSize) 

## 1.1 Variable types ----

str(data.org)
data.org <- data.org %>% mutate(Promotion = factor(Promotion, levels = 1:3),  # Promotion is a factor :)
                                MarketID = factor(MarketID), # MarketID is a factor :)
                                MarketSize = factor(MarketSize, levels = c("Small", "Medium", "Large")), # MarketSize is a factor :)  
                                LocationID = factor(LocationID), # LocationID is a factor :)
                                week = factor(week)) # week is a factor

## 1.2 Correlations ----

r <- lowerCor(data.org)
r %>% round(2)
# corPlot(r, diag = TRUE)

# SalesInThousands ~ MarketID *
# SalesInThousands ~ MarketSize **
# SalesInThousands ~ LocationID *

# MarketSize ~ AgeOfStore * 
# MarketSize ~ MarketID *
# MarketSize ~ LocationID *

# LocationID ~ MarketID ***

## 1.3 Description ----

describe(data.org) 
describeBy(data.org, data.org$Promotion)

# 2. Promotion ----

# The most basic effect of Promotion on cumulative Sales.
# Without considering other variables effect.
# --> Initial insight.

Promotion.Basic <- data.org %>% 
  group_by(Promotion) %>% summarize(CumSales = sum(SalesInThousands), NumberLocations = n() / 4) %>%
  mutate(CumPercentSales = CumSales / sum(CumSales) * 100) %>%
  ggplot(aes(x = Promotion, y = CumPercentSales, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0("Nr. of locations = ", NumberLocations)), vjust = -0.8) +
  geom_label(aes(label = paste0("Cumulative % of sales = ", round(CumPercentSales, 1), "% (", round(CumSales), "k)")), vjust = 1.5) + 
  labs(x = "Promotion", y = "Cumulative sales [%]", title = "Cumulative Sales conditional on Promotion group") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# We Might to take number of Locations into accounts (LocationID)

## 2.1 weeks ----
# Before anything else, let's look at weeks to see if it needs to be considered within everything else.

# Effect of week variable on Sales -> We do not consider the Promotion group or anything else
Weeks.Basic <- data.org %>% group_by(week) %>% summarize(AvgSales = mean(SalesInThousands)) %>%
  ggplot(aes(x = week, y = AvgSales, fill = week)) + geom_col() +
  geom_label(aes(label = round(AvgSales, 1)), vjust = 1.5) +
  labs(x = "Week", y = "Average Sales per Week", title = "Week effect on Sales not considering anything else") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
# There are no differences between weeks in terms of Sales

# Test: Are there differences in Sales depending only on weeks
data.org %>% group_by(week) %>% summarize(avgSales = avg(SalesInThousands))
Test.Weeks <- kruskal.test(SalesInThousands ~ week, data = data.org)
# No relationship between week and Sales

# Effect of week variable on Sales conditional on Promotion group
Weeks.Promotion <- data.org %>% group_by(Promotion, week) %>% summarize(Avg.Sales = mean(SalesInThousands)) %>%
  ggplot(aes(x = week, y = Avg.Sales, fill = week)) + geom_col() + facet_wrap(. ~ Promotion) + 
  geom_label(aes(label = paste0(round(Avg.Sales, 1), "k")), vjust = 1.5) +
  labs(x = "Week", y = "Average Sales per Week", title = "Week effect on Sales conditional on Promotion groups") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
# There are minimal differences in weeks within all Promotions groups

# Test: Are there differences in Sales depending only on weeks

# Promotion 1:
data.org %>% filter(Promotion == "1") %>% group_by(week) %>% summarize(avgSales = mean(SalesInThousands))
Test.Weeks_1 <- kruskal.test(SalesInThousands ~ week, data = data.org %>% filter(Promotion == "1"))
# No relationship between week and Sales with Promotion 1

# Promotion 2:
data.org %>% filter(Promotion == "2") %>% group_by(week) %>% summarize(avgSales = mean(SalesInThousands))
Test.Weeks_2 <- kruskal.test(SalesInThousands ~ week, data = data.org %>% filter(Promotion == "2"))
# No relationship between week and Sales with Promotion 2

# Promotion 3:
data.org %>% filter(Promotion == "3") %>% group_by(week) %>% summarize(avgSales = mean(SalesInThousands))
Test.Weeks_3 <- kruskal.test(SalesInThousands ~ week, data = data.org %>% filter(Promotion == "3"))
# No relationship between week and Sales with Promotion 3

## 2.2 LocationID ----

Locations.PromotionGroup <- data.org %>% group_by(Promotion) %>% summarize(Nr.Locations = n() / 4)

# Regarding LocationID, do we have any outliers that could greatly increase Sales, even conditionally per week?

Location.outlier.Promotion <- ggplot(data = data.org %>% arrange(Promotion)) + 
  geom_point(aes(x = LocationID, y = SalesInThousands, color = MarketSize)) + facet_wrap(.~week) + theme_bw()

# No utliers :)

# Effect of Promotion on cumulative Sales taking number of locations into account 
# --> Per Average location :)

Promotion.AvgLocation <- data.org %>% 
  group_by(Promotion) %>% summarize(CumSales = sum(SalesInThousands), NumberLocations = n() / 4) %>%
  mutate(AvgSalesLoc = CumSales / NumberLocations) %>% 
  mutate(AvgSalesLocPercent = AvgSalesLoc / sum(AvgSalesLoc) * 100) %>%
  ggplot(aes(x = Promotion, y = AvgSalesLocPercent, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = paste0("Cumulative % of sales per Location = ", round(AvgSalesLocPercent, 1), "%  (", round(AvgSalesLoc), "k)")), vjust = 1.5) + 
  labs(x = "Promotion", y = "Cumulative sales per average Location [%]", title = "Cumulative Sales per average Location, conditional on Promotion group") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Both top plots together :)

Promotion.Basic.AvgLocation <- data.org %>% 
  group_by(Promotion) %>% summarize(CumSales = sum(SalesInThousands), NumberLocations = n() / 4) %>%
  mutate(CumPercentSales = CumSales / sum(CumSales) * 100,
         CumSales.Normalized = CumSales / NumberLocations) %>%
  mutate(CumPercentSales.Normalized = CumSales.Normalized / sum(CumSales.Normalized) * 100) %>%
  ggplot(aes(x = Promotion, y = CumPercentSales.Normalized, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_text(aes(label = paste0("Nr. of locations = ", NumberLocations)), vjust = -0.8) +
  geom_label(aes(label = paste0("Normalized = ", round(CumPercentSales.Normalized, 1), "% --- ",
                                "Not normalized = ", round(CumPercentSales, 1), "%")), vjust = 1.5) +
  labs(x = "Promotion", y = "Cumulative sales per average location [%]", title = "Cumulative Sales normalized for number of Locations, conditional on Promotion group") +
  theme_bw() + theme(legend.position = "none")

# Test: Is there a difference in average Sales between Promotion groups
# -> Average - Taking into account number of locations per Promotion group :)
# -> Not taking into account weeks - cumulative all 4 weeks together and then average :)

data.Promotion.CumWeeks <- data.org %>% group_by(LocationID, Promotion) %>% summarize(Sales = sum(SalesInThousands))

# Parametric or non parametric test ? -> Non parametric :)
ggplot(data = data.Promotion.CumWeeks, aes(x = Sales)) + geom_density(fill = "grey") + facet_wrap(. ~ Promotion) + theme_bw()
shapiro.test(data.Promotion.CumWeeks %>% filter(Promotion == "1") %>% pull(Sales)) 

# Kruskal Wallis test 
Kruskal.Promotion.LocationNr <- kruskal.test(Sales ~ Promotion, data = data.Promotion.CumWeeks)
# There are significant differences in Sales between Promotion groups - But we don't know which pairs of groups are different.

# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
Pairwise.Wilcox.Promotion.LocationNr <- pairwise.wilcox.test(data.Promotion.CumWeeks$Sales, data.Promotion.CumWeeks$Promotion, p.adjust.method = "BH")
# 1 = 3 & 1 != 2 & 2 != 3

Sales.Promotion.Location.Nr <- data.Promotion.CumWeeks %>% group_by(Promotion) %>% summarize(Avg.Sales = mean(Sales)) %>% arrange(desc(Avg.Sales)) %>%
  mutate(Avg.Sales.Perc = Avg.Sales / sum(Avg.Sales) * 100)
# 1 >= 3 > 2

## 2.3 MarketSize ----

# Average number of Sales in 4 weels conditional on MarketSize
MarketSize.Sales <- data.org %>% group_by(LocationID, MarketSize) %>% summarize(Sales = sum(SalesInThousands)) %>%
  group_by(MarketSize) %>% summarize(Avg.Sales = mean(Sales)) %>% arrange(desc(Avg.Sales))

table(data.org$Promotion, data.org$MarketSize)
# --> Small-60, Medium-320, Large-168 

# Test: Is there a difference in average Sales between Market sizes
# -> Average - Taking into account number of locations per Market size :)
# -> Not taking into account weeks - cumulative all 4 weeks together and then average :)

data.MarketSize.CumWeeks <- data.org %>% group_by(LocationID, MarketSize) %>% summarize(Sales = sum(SalesInThousands))

# Parametric or non parametric test ? -> Non parametric :)
ggplot(data = data.MarketSize.CumWeeks, aes(x = Sales)) + geom_density(fill = "grey") + facet_wrap(. ~ MarketSize) + theme_bw()
shapiro.test(data.MarketSize.CumWeeks %>% filter(MarketSize == "Large") %>% pull(Sales)) 

# Kruskal Wallis test 
Kruskal.MarketSize <- kruskal.test(Sales ~ MarketSize, data = data.MarketSize.CumWeeks)
# There are significant differences in Sales between Market sizes - But we don't know which pairs of sizes are different.

# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
Pairwise.Wilcox.MarketSize <- pairwise.wilcox.test(data.MarketSize.CumWeeks$Sales, data.MarketSize.CumWeeks$MarketSize, p.adjust.method = "BH")
# 1 != 2 != 3

Sales.MarketSize <- data.MarketSize.CumWeeks %>% group_by(MarketSize) %>% summarize(Avg.Sales = mean(Sales)) %>% arrange(desc(Avg.Sales)) %>%
  mutate(Avg.Sales.Perc = Avg.Sales / sum(Avg.Sales) * 100)
# Large > Small > Medium

# How is MarketSize distributed among Promotion groups?
# That is, is there a hidden influence of MarketSize in Promotion? 

# Percentage of MarketSize per each Promotion group
promotionXsize <- data.org %>% 
  group_by(Promotion, MarketSize) %>% summarize(n = n() / 4) %>% 
  mutate(Promotion = paste0("Promotion_", Promotion)) %>%
  pivot_wider(names_from = Promotion, values_from = n) %>%
  mutate(Promotion_1 = Promotion_1 / sum(Promotion_1) * 100,
         Promotion_2 = Promotion_2 / sum(Promotion_2) * 100,
         Promotion_3 = Promotion_3 / sum(Promotion_3) * 100)
# For each Promotion group, medium market size is most common and small market size the least common

# Test: Is there a correlation between MarketSize and Promotion?

data.Size.Promotion <- data.org %>% distinct(LocationID, .keep_all = TRUE) 
data.Size.Promotion.Table <- table(data.Size.Promotion$MarketSize, data.Size.Promotion$Promotion)
  # pivot_wider(names_from = Promotion, values_from = n) %>%
  # setNames(c("MarketSize", "Promotion1", "Promotion2", "Promotion3"))

chisq.test(data.Size.Promotion.Table)
# --> MarketSize and Promotion group are not significantly correlated variables
# --> This means that the number of campaigns does not differ between Promotion group in terms of MarketSize.

# Addition:
# However, if we do not totally believe this test 
# Let's try to get rid of the influence of MarketSize
# We normalize Sales so that the average is 1 within each market Size

# We normalize so that Sales within Small will be of the same value as within Large MarketSize. 
# In this way, we can simply add or average the Sales within Promotion groups, 
# without fearing that the differences are the result of differences between the MakretSize distributions within Promotion groups, 
# which we know affect Sales.

# Average Sales in each MarketSize
MarketSize.AvgSales <- data.org %>% group_by(MarketSize, LocationID) %>% summarize(Sales = sum(SalesInThousands)) %>%
  group_by(MarketSize) %>% summarize(Avg.Sales = mean(Sales))

# Normalize Sales within MarketSize --> Average of Sales is 1 within each market Size
data.Normalized.Size.Sales <- data.org %>% group_by(MarketSize, LocationID, Promotion) %>% summarize(Sales = sum(SalesInThousands)) %>%
  left_join(MarketSize.AvgSales, by = "MarketSize") %>%
  mutate(Sales.Normalized = Sales / Avg.Sales) %>%
  mutate(Sales.Normalized.Avg = Sales.Normalized * 213.8648)
# data.org %>% group_by(LocationID) %>% summarize(Sales = sum(SalesInThousands)) %>% pull(Sales) %>% mean() = 213.8648

# So, how now does the Price differ between Promotions when there is no longer MarketSize effect?
MarketSize.Normalized <- data.Normalized.Size.Sales %>% 
  group_by(Promotion) %>% summarize(Avg.Sales.Normalized = mean(Sales.Normalized.Avg)) %>%
  mutate(Sales.Normalized.AvgPercent = Avg.Sales.Normalized / sum(Avg.Sales.Normalized) * 100) %>%
  ggplot(aes(x = Promotion, y = Sales.Normalized.AvgPercent, fill = Promotion)) + 
  geom_col(color = "black") + 
  geom_label(aes(label = paste0(round(Sales.Normalized.AvgPercent, 1), "% = ", round(Avg.Sales.Normalized), "k")), vjust = 1.5) +
  labs(x = "Promotion", y = "Average normalized sales per location [%]", title = "Effect of Promotion on Sales normalized per MarketSize") +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Test: Is there a significant difference in average Sales between Promotions when normalized for MarketSize?

ggplot(data = data.Normalized.Size.Sales, aes(x = Sales.Normalized.Avg)) + geom_density(fill = "grey") + facet_wrap(. ~ Promotion) + theme_bw()
shapiro.test(data.Normalized.Size.Sales %>% filter(Promotion == "1") %>% pull(Sales.Normalized.Avg))

kruskal.test(Sales.Normalized.Avg ~ Promotion, data = data.Normalized.Size.Sales)
# There are significant differences between groups :)
# But we don't know which pairs of groups are different.
# --> pairwise.wilcox.test() to calculate pairwise comparisons between group levels with corrections for multiple testing.
pairwise.wilcox.test(data.Normalized.Size.Sales$Sales.Normalized.Avg, data.Normalized.Size.Sales$Promotion, p.adjust.method = "BH")
# Promotion 1 = Promotion 3 & Promotion 1 != Promotion 3 & Promotion 2 != Promotion 3
# 1 (35.8% or 230k) = 3 (35.0% or 225k) > 2 (29.2% or 188k)

## 2.4 AgeofStore ----

# Is there a relationship between AgeOfStore and Sales?
AgeOfStoreBasic <- ggplot(data = data.org, aes(x = AgeOfStore, y = SalesInThousands)) + geom_point() + geom_smooth(method = "loess") + theme_bw()
AgeOfStoreLinear <- lm(SalesInThousands ~ AgeOfStore, data = data.org) %>% summary()
# No (linear) relationship

# Is there a relationship between AgeOfStore and Promotion group?
AgeOfStorePromotion <- ggplot(data = data.org %>% distinct(LocationID, .keep_all = TRUE)) + 
  geom_boxplot(aes(x = Promotion, y = AgeOfStore, color = Promotion)) +
  theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
# No visible relationship

# Test :
# Parametric or non parametric test ? -> Non parametric :)
ggplot(data = data.org %>% distinct(LocationID, .keep_all = TRUE), aes(x = AgeOfStore)) + geom_density(fill = "grey") + facet_wrap(. ~ Promotion) + theme_bw()
shapiro.test(data.org %>% distinct(LocationID, .keep_all = TRUE) %>% filter(Promotion == "1") %>% pull(AgeOfStore)) 

# Kruskal Wallis test 
Kruskal.AgeOfStore.Promotion <- kruskal.test(Promotion ~ AgeOfStore, data = data.org %>% distinct(LocationID, .keep_all = TRUE))
# There are significant differences in Ages of stores between Promotion groups.

## 2.5 MarketID ----

# Regarding MarketID, do we have any outliers that could greatly increase Sales, even conditionally per MarketSize?
MarketID.outliers <- ggplot(data = data.org %>% group_by(LocationID, MarketID, MarketSize, Promotion) %>% summarize(SalesInThousands = sum(SalesInThousands))) + 
  geom_point(aes(x = Promotion, y = SalesInThousands, color = MarketSize)) +  facet_wrap(. ~ MarketID) + theme_bw() +
  labs(x = "Promotion", y = "Sales in 4 weeks", title = "MarketID")

