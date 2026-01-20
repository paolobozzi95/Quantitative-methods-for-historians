
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

#NB: GDP is in millions, tax_revenue in thousands 
master_data <- master_data %>%
  arrange(province, year) %>%
  group_by(province) %>%
  mutate(
    tax_burden = tax_revenue / GDP / 1000,
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


library(fixest)
library(broom)
library(dplyr)
library(ggplot2)
library(stringr)

# 1. Extract coefficients
res <- tidy(did_model, conf.int = TRUE)

# 2. Clean and Organize Data
plot_data <- res %>%
  # Filter for the year interaction terms
  filter(str_detect(term, "year::")) %>%
  mutate(
    # Extract year as character
    year = str_extract(term, "\\d{4}"),
    
    # Label: If "north" is in the name, it's the North interaction, else Baseline
    group = ifelse(str_detect(term, "north"), "North", "Baseline")
  ) %>%
  select(year, estimate, conf.low, conf.high, group)

# 3. Add the Reference Year (1972) manually
ref_row <- data.frame(
  year = "1972",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  group = c("North", "Baseline")
)

plot_data <- bind_rows(plot_data, ref_row) %>%
  arrange(year)

# Define how much to separate them (0.5 is usually a good starting point)
pd <- position_dodge(width = 0.5)

ggplot(plot_data, aes(x = year, y = estimate, color = group, group = group)) +
  
  # 1. Zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # 2. Error Bars (Apply dodge here)
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                position = pd,  # <--- Moves the bars
                alpha = 0.5) +
  
  # 3. Lines (Apply dodge here so lines connect the shifted points)
  geom_line(position = pd) +    # <--- Moves the lines
  
  # 4. Points (Apply dodge here)
  geom_point(size = 3, position = pd) + # <--- Moves the dots
  
  # 5. Custom Colors (Red for North, Black for Baseline)
  scale_color_manual(values = c("North" = "red", "Baseline" = "black")) +
  
  # 6. Labels and Formatting
  labs(x = "Year", y = "% change in DC voting", title = "Impact of the reform on DC voting") +
  #theme_minimal, no grid, but solid x and y axes
  theme_minimal() +
  theme(legend.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )













