
library(fixest)
library(ggplot2)
library(readxl)
library(dplyr)

setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")

wage_data_compact <- read_excel("wage_data_compact.xlsx")
GDP <- read_excel("GDP.xlsx")
#population <- read_excel ("population.xlsx")
education <- read_excel("education.xlsx")
data_census <- read_excel("data_census.xlsx")


#Merge all the datasets
df <-  data_census %>%
  left_join(GDP, by = c("province", "year")) %>%
  left_join(education, by = c("province", "year")) %>%
  left_join(wage_data_compact, by = c("province", "year"))

#Year as a character
df <- df %>%
  mutate(
    province = as.factor(province),
    year = as.factor(year)
  )


#Test for multicollinearity: VIF
library(car)

lm_1 <- lm(
  labour_force_participation_female ~ 
    family_size +
    employment_rate +
  #  employment_tertiary +
 #   employment_retail +  
    employment_industry +
    employment_agriculture
  ,
  data = df
)

vif(lm_1)

#Interpretation:
#   1	No multicollinearity
#   1–5	Moderate (usually acceptable)
#   > 5	Potential problem
#   > 10	Serious multicollinearity

df_iv <- df_iv %>%
  mutate(
    share_students = students / population * 100,
    GDP_pc = GDP * 1000000 * 1936.27 / population
  )

#Model
#View(df_iv)

model_1 <- feols(
  labour_force_participation_female ~ 
     
    labour_force_participation_male +  
    share_students +
  #  GDP_pc +
    share_religious +
    employment_agriculture +
    employment_retail +
    subalterno +
    family_size 
    
  | year, cluster = ~province
  , data = df_iv
  )
    
summary(model_1)
    

model_2 <- feols(
  labour_force_participation_female ~ 
    
    labour_force_participation_male +  
  #  share_students +
    GDP_pc +
    share_religious +
    employment_agriculture +
    employment_retail +
    # subalterno +
    family_size 
  
  | year, cluster = ~province
  , data = df_iv
)
    
summary(model_2)  
    
#print as html with etable

etable(model_1, model_2, tex = FALSE)
    
    
    
    
    
    family_size +
 #   employment_rate +
    employment_tertiary +
 #   employment_retail +
#    employment_industry +
    employment_agriculture |
    province + year,
  data = data_census, cluster = ~province
)

summary(model_1)

#How can we explain the result for the tertiary?






#### Instrumental variable ####

#Marriage data: data manipulation

#Merge the different panels of the excel file 
#name of the panel is the value for the variable year
library(readxl)
library(dplyr)
library(purrr)

# Path to the Excel file
file_path <- "marriage_data.xlsx"

# Get sheet names (each sheet = one panel/year)
sheets <- excel_sheets(file_path)

marriage_data <- map_dfr(sheets, function(s) {
  read_excel(file_path, sheet = s) %>%
    mutate(year = as.character(s))
})

#Merge with df
df_iv <- df %>%
  left_join(marriage_data, by = c("province", "year"))


df_iv <- df_iv %>%
  mutate(
    share_religious = religiosi / (civili + religiosi) * 100
  )

#View(df_iv)


#Is it a good instrument?
model_test <- feols(family_size ~ share_religious,
                    
                    data = df_iv)

summary(model_test)


model_iv <- feols(
  labour_force_participation_female ~ 
    
    1 
    #Controls
 #   employment_tertiary +
#    employment_agriculture 
                  
    
  |  year + province
    
    #Instrument
  |  family_size ~ share_religious, 
  
  cluster = ~province, data = df_iv)

summary(model_iv)


feols(labour_force_participation_female ~ share_religious 
      
      | province + year,
      cluster = ~province, data = df_iv)
