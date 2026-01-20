
setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")

library(readxl)

wage_data_compact <- read_excel("wage_data_compact.xlsx")
GDP <- read_excel("GDP.xlsx")
population <- read_excel ("population.xlsx")
education <- read_excel("education.xlsx")
data_census <- read_excel("data_census.xlsx")
marriage_data <- read_excel ("marriage_data.xlsx")
tax_and_politics_data <-read_excel ("tax_and_politics_data.xlsx")

master_data <- tax_and_politics_data %>%
  left_join(GDP, by = c("province", "year")) %>%
  left_join(population, by = c("province", "year")) %>%
  left_join(education, by = c("province", "year")) %>%
  left_join(data_census, by = c("province", "year")) %>%
  left_join(wage_data_compact, by = c("province", "year")) %>%
  left_join(marriage_data, by = "province")  

library(dplyr)

#2. Tax burden berechnen und laggen
master_data <- master_data %>%
  arrange(province, year) %>%
  group_by(province) %>%
  mutate(
    tax_burden = tax_revenue / GDP,
    tax_burden_lag = lag(tax_burden, 1)
  ) %>%
  ungroup()

#3. Jahre filtern
master_data_elec <- master_data %>%
  filter(  year == 1963 |
             year == 1968 |
             year == 1972 |
             year == 1979 |
             year == 1983 |
             year == 1987)

#4. Deltas berechnen
master_data_elec <- master_data_elec %>%
  arrange(province, year) %>%
  group_by(province) %>%
  mutate(
    d_tax_burden = tax_burden - lag(tax_burden, 1),
    d_votes_dc   = share_votes_DC - lag(share_votes_DC, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_tax_burden), !is.na(d_votes_dc))


#5. Treatment-Variablen erstellen
#not necessary: our treatment is the variable "north" 

#6. Diff-in-Diff Modell schätzen
library(fixest)

did_model <- feols(d_votes_dc ~ 
                     
        i(year, d_tax_burden, ref = 1972) : north
     +  i(year, d_tax_burden, ref = 1972) 
        
        | province + year,
        data = master_data_elec,
        cluster = ~province
        )

summary(did_model)

#7. Plot the data with ggplot (unfortunately iplot does not work with two i(...) objects)

library(ggplot2)
library(broom)

#Extract the coefficients
















