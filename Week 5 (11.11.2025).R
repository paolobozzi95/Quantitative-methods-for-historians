

#Loading packages
library(readxl)
library(dplyr)
library(ggplot2)


#Setting working directory
setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/QMH data")

#Checking it worked
getwd()

#load the data we cleaned last week
wage_data <- read_excel("wage_data.xlsx")


#let's have a look at it
head(wage_data)
#View(wage_data)

#It does not look very good: multiple rows for the same provinces in the same year
#We use coalesce : it takes multiple vectors and returns a single vector
wage_data_compact <- wage_data %>%
  group_by(province, year) %>%
  reframe(
    subalterno = coalesce(subalterno[!is.na(subalterno)], NA),
    qualificato = coalesce(qualificato[!is.na(qualificato)], NA))



#View(wage_data_compact_2)

#let's create an identifier (id) for each observation using paste0 
wage_data_compact$id <- paste0(wage_data_compact$province, 
                               wage_data_compact$year)          # , sep = "_")

#create id with mutate - same thing as above
wage_data_compact <- wage_data_compact %>%
  mutate(id = paste0(province, year))       


#View(wage_data_compact)

library(openxlsx)
write.xlsx(wage_data_compact, "wage_data_compact.xlsx")


#### t - test ####


#let's calculate the average wage in the North and in the South

#load the north_data
north_data <- read_excel("north_data.xlsx")

df <- wage_data_compact %>%
  inner_join(north_data, by = "province")


#create a new variable for wages_north and one for wages_south
df <- df %>%
  mutate(wages_north = ifelse(north == 1, subalterno, NA),
         wages_south = ifelse(north == 0, subalterno, NA))

#Calculate the means in 1971
df_means_1971 <- df %>%
  filter(year == 1971)

mean(df_means$wages_north, na.rm = TRUE)
mean(df_means$wages_south, na.rm = TRUE)

df_means_1981 <- df %>%
  filter(year == 1981)

mean(df_means$wages_north, na.rm = TRUE)
mean(df_means$wages_south, na.rm = TRUE)


#Is the difference significant? t-test
t.test(df_means_1971$wages_north, df_means_1971$wages_south)

#Visualize the data with a boxplot for 1971 and 1981
# Create boxplot with basic r
boxplot(subalterno ~ north, 
        data = df_means_1971,
        col = c("lightblue", "darkgreen"))

#year as character
df <- df %>%
  mutate(year = as.factor(year))

#let's have a look across the years
boxplot(log(subalterno) ~ north + year, 
        data = df,
        col = c("navy", "darkgreen"))


#with GGPLOT
ggplot(df, aes(x = factor(year), y = subalterno, fill = factor(north))) +
  geom_boxplot() +
  labs(title = "Wages in North vs South (1971 and 1981)",
       x = "Year",
       y = "Wages",
       fill = "Region") +
  scale_fill_manual(labels = c("South", "North"), values = c("lightblue", "lightgreen")) +
  #theme economist
  theme_bw()
  
#load data gdp per capita
population <- read_excel ("population.xlsx")
GDP <- read_excel ("GDP.xlsx")

merged_data <- wage_data %>%
  left_join(population, by = c("province", "year")) %>%
  left_join(GDP, by = c("province", "year"))

merged_data <- merged_data %>%
  mutate(GDP_pc = GDP / population)


#plot gdp pc and wages with ggplot
ggplot(merged_data, aes(x = log(GDP_pc), y = log(subalterno), 
                        color = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Wages vs GDP per capita",
       x = "GDP per capita",
       y = "Wages") +
  theme_minimal()





  
  






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

#Attach map geometry
merged_data <- map_data %>%
  left_join(merged_data, by = "province")


#Let's draw the map for wages in 1971
library(ggplot2)
map <- merged_data  %>%
  filter(year == 1971) %>%
  ggplot() +
  geom_sf(aes(fill = subalterno), color = "black") +
  theme_void() +
  scale_fill_gradient(low = "lightblue", high = "navy", na.value = "grey80")

map





