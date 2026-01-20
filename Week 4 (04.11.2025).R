


# Pakete laden
library(readxl)
library(dplyr)
library(ggplot2)


# Arbeitsverzeichnis setzen
setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")

getwd()

# Excel-Dateien einlesen
Fabian_1961     <- read_excel("Fabian_1961.xlsx")
Korbinian_1971  <- read_excel("Korbinian_1971.xlsx")
Korbinian_1981  <- read_excel("Korbinian_1981.xlsx")
Kristina_1971   <- read_excel("Kristina_1971.xlsx")
Kristina_1981   <- read_excel("Kristina_1981.xlsx")


#Merging the data
data_1961 <- Fabian_1961 %>%
  mutate(subalterno = men_subalterni,
         qualificato = men_operai_qualificati)
  
#Create variable year equal for all the obs in the dataset
data_1961 <- data_1961 %>%
  mutate(year = 1961)

Korbinian_1971 <- Korbinian_1971 %>%
  mutate(year = 1971)

Korbinian_1981 <- Korbinian_1981 %>%
  mutate(year = 1981)

Kristina_1971 <- Kristina_1971 %>%
  mutate(year = 1971)

Kristina_1981 <- Kristina_1981 %>%
  mutate(year = 1981) 

#Merge the datasets
wage_data <- bind_rows(
  data_1961 %>% select(province, subalterno, qualificato, year),
  Korbinian_1971 %>% select(province, qualificato, year),
  Korbinian_1981 %>% select(province, qualificato, year),
  Kristina_1971 %>% select(province, subalterno, year), 
  Kristina_1981 %>% select(province, subalterno, year) 
)

#Save the data file in the wd folder (as excel file)
library(openxlsx)
write.xlsx(wage_data, "wage_data.xlsx")

  
#Calculate the average wage and its standard deviation 
#for each year (1961, 1971, 1981)

wage_data_1961 <- wage_data %>%
  filter(year == 1961)
 
#Using R basic operator "$" 
mean(wage_data_1961$subalterno, na.rm = TRUE)

#Using dplyt "%>%" and "pull()" function
mean(wage_data_1961 %>% pull(subalterno), na.rm = TRUE)

wage_data_1971 <- wage_data %>%
  filter(year == 1971)
mean(wage_data_1971$subalterno, na.rm = TRUE)

wage_data_1981 <- wage_data %>%
  filter(year == 1981)
mean(wage_data_1981$subalterno, na.rm = TRUE)

sd(wage_data_1961$subalterno, na.rm = TRUE)
sd(wage_data_1971$subalterno, na.rm = TRUE)
sd(wage_data_1981$subalterno, na.rm = TRUE)


