

#### Measures of dispersion ####

#We use Fabian's data for 1961 with the distinction between men and wemen

setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")

library(readxl)
wage_data_1961 <- read_excel("Fabian_1961.xlsx")

library(dplyr)
wage_data_1961 <- wage_data_1961 %>%
  filter(!is.na(men_subalterni), !is.na(women_subalterne))


#Are men_subalterni and women_subaterne significantly different?

#1. HISTOGRAM
library(ggplot2)
hist_m <- ggplot(wage_data_1961, 
                 aes(x = men_subalterni)) +
  geom_histogram(bins = 15, fill = "gray40", 
                 color = "black") +
  theme_classic()
hist_m

library(ggthemes)

hist_f <- ggplot(wage_data_1961, 
                 aes(x = women_subalterne)) +
  geom_histogram(bins = 15, fill = "darkgreen", 
                 color = "black") +
  theme_classic()

hist_f


#Test for normality: Quantile quantile plot - theoretical quantiles vs sample quantiles
#If the points fall approximately along a straight line, the data is normally distributed
qqnorm(wage_data_1961$men_subalterni)
qqnorm(wage_data_1961$women_subalterne)


#Formal test for normality: Shapiro-Wilk Test
#Null hypothesis: the data is normally distributed
shapiro.test(wage_data_1961$men_subalterni)
shapiro.test(wage_data_1961$women_subalterne)


#Are wages for men and for women different?
mean_m <- mean(wage_data_1961$men_subalterni)
mean_f <- mean(wage_data_1961$women_subalterne)

mean_m
mean_f

#2. T-TEST (parametric test)
#Null hypothesis: the means of the two groups are equal

t_test_result <- t.test(wage_data_1961$men_subalterni, 
                        wage_data_1961$women_subalterne)
t_test_result


#Inequality

#Coefficient of variation
cv_m <- sd(wage_data_1961$men_subalterni) / mean(wage_data_1961$men_subalterni)
cv_m

cv_f <- sd(wage_data_1961$women_subalterne) / mean(wage_data_1961$women_subalterne)
cv_f

#Gini
library(ineq)
gini_m <- Gini(wage_data_1961$men_subalterni)
gini_m

gini_f <- Gini(wage_data_1961$women_subalterne)
gini_f





#### Drawing maps ####

#load the geographical data (shapefile)
library(sf)
map_data <- st_read("italian_provinces/Province1981.shp")

library(readxl)
wage_data_compact <- read_excel("wage_data_compact.xlsx")


#Cleaning the data
map_data <- map_data %>%
  mutate(DEN_PROV = ifelse(row_number() == 40, "Forlì", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Bolzano - Bozen", "Bolzano", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Valle d'Aosta", "Aosta", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Massa-Carrara", "Massa Carrara", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Reggio nell'Emilia", "Reggio Emilia", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Reggio di Calabria", "Reggio Calabria", DEN_PROV),
         DEN_PROV = ifelse(DEN_PROV == "Pesaro e Urbino", "Pesaro Urbino", DEN_PROV)
  ) %>%
  rename(province = DEN_PROV)


#Expand the map data to include all years and avoid empty areas - not strictly necessary
provs_years <- expand.grid(province = map_data$province, 
                           year = unique(wage_data_compact$year))

merged_data <- provs_years %>%
  left_join(wage_data_compact, by = c("province", "year"))




