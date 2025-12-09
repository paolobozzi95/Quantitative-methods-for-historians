
setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")


library(readxl)
wage_data_compact <- read_excel("wage_data_compact.xlsx")
GDP <- read_excel("GDP.xlsx")
population <- read_excel ("population.xlsx")
education <- read_excel("education.xlsx")

# Load packages
library(dplyr)

data <- wage_data_compact %>%
  left_join(GDP, by = c("province", "year")) %>%
  left_join(population, by = c("province", "year")) %>%
  left_join(education, by = c("province", "year"))

data <- data %>%
  mutate(wage = (subalterno + qualificato) / 2)

# 2. create share students

data <- data %>%
  mutate(share_students = students / population * 100)

#Adjust GDP

data <- data %>%
  mutate(GDP_pc_lire = GDP * 1000000 * 1936.27 / population)


# 4.Regression
library(fixest)
model1 <- feols(wage ~ share_students + GDP_pc_lire, 
                cluster = ~province, 
                data = data)
summary(model1)


#Calculate predicted wage change if education increases from 2.28 to 3.28
wage_before <- coef(model1)["share_students"] * mean(data$share_students, na.rm = TRUE) + 
               coef(model1)["(Intercept)"] + 
               coef(model1)["GDP_pc_lire"] * mean(data$GDP_pc_lire, na.rm = TRUE)
              
wage_before

wage_after <- coef(model1)["share_students"] * (mean(data$share_students, na.rm = TRUE) + 1) + 
              coef(model1)["(Intercept)"] + 
              coef(model1)["GDP_pc_lire"] * mean(data$GDP_pc_lire, na.rm = TRUE)
wage_after

wage_change <- (wage_after - wage_before) / wage_before * 100
wage_change


#Calculate the change in wages with a 10 per cent increase in share_students
model2 <- feols(log(wage) ~ log(share_students) + log(GDP_pc_lire), 
                cluster = ~province, 
                data = data)
summary(model2)





#Fixed effects 

#errors clustered by province

model3 <- feols(wage ~ share_students + GDP_pc_lire +
                  factor(province) + factor(year), 
                cluster = ~province, data = data)
summary(model3)

data_1961 <- data %>%
  filter(year == 1961)

model4 <- feols(wage ~ share_students + GDP_pc_lire, 
                cluster = ~province, data = data_1961)
summary(model4)
