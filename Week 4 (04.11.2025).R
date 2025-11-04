


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

Fabian_1961 <- Fabian_1961 %>%
  mutate(year = 1961)

Korbinian_1971 <- Korbinian_1971 %>%
  mutate(year = 1971)

Korbinian_1981 <- Korbinian_1981 %>%
  mutate(year = 1981)

Kristina_1971 <- Kristina_1971 %>%
  mutate(year = 1971)

Kristina_1981 <- Kristina_1981 %>%
  mutate(year = 1981) 

#All variables as numeric



data <- bind_rows(
  data_1961 %>% select(province, subalterno, qualificato, year),
  Korbinian_1971 %>% select(province, qualificato, year),
  Korbinian_1981 %>% select(province, qualificato, year),
  Kristina_1971 %>% select(province, subalterno, year), 
  Kristina_1981 %>% select(province, subalterno, year) 
)
  















#------------------------------------------------------------
#Mittelwert und SD
mean_1961_fabian <- Fabian_1961 %>%
  select(men_subalterni, men_operai_qualificati,
         women_subalterne, women_operaie_qualificate) %>%
  mutate(across(everything(), as.numeric)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1961_fabian


mean_1971_korbinian <- Korbinian_1971 %>%
  select(qualificato) %>%
  mutate(across(everything(), as.numeric)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1971_korbinian

mean_1981_korbinian <- Korbinian_1981 %>%
  select(qualificato) %>%
  mutate(across(everything(), as.numeric)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1981_korbinian


mean_1971_kristina <- Kristina_1971 %>%
  select(subalterno) %>%
  mutate(across(everything(), as.numeric)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1971_kristina

mean_1981_kristina <- Kristina_1981 %>%
  select(subalterno) %>%
  mutate(across(everything(), as.numeric)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1981_kristina

#1971 kombiniert
data_1971 <- bind_rows(
  Korbinian_1971,
  Kristina_1971
)

cols_1971 <- c("subalterno", "qualificato")

data_1971 <- data_1971 %>%
  mutate(across(all_of(cols_1971), ~ na_if(., "n.d."))) %>%
  mutate(across(all_of(cols_1971), ~ na_if(., "-"))) %>%
  mutate(across(all_of(cols_1971), ~ na_if(., "?"))) %>%
  mutate(across(all_of(cols_1971), as.numeric))

mean_1971 <- data_1971 %>%
  select(all_of(cols_1971)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1971

#1981 kombiniert
data_1981 <- bind_rows(
  Korbinian_1981,
  Kristina_1981
)

cols_1981 <- c("subalterno", "qualificato")

mean_1981 <- data_1981 %>%
  select(all_of(cols_1981)) %>%
  unlist() %>%
  mean(na.rm = TRUE)

mean_1981

#SD
# Fabian 1961
cols_1961 <- c("men_subalterni", "men_operai_qualificati", 
               "women_subalterne", "women_operaie_qualificate")
sd_1961_fabian <- sd(unlist(Fabian_1961[cols_1961]), na.rm = TRUE)
sd_1961_fabian

# 1971 kombiniert
data_1971 <- bind_rows(Korbinian_1971, Kristina_1971)
cols_1971 <- c("subalterno", "qualificato")
sd_1971 <- sd(unlist(data_1971[cols_1971]), na.rm = TRUE)
sd_1971

# 1981 kombiniert
data_1981 <- bind_rows(Korbinian_1981, Kristina_1981)
cols_1981 <- c("subalterno", "qualificato")
sd_1981 <- sd(unlist(data_1981[cols_1981]), na.rm = TRUE)
sd_1981

#----------------------------------------------------------------
#Provinz mit höchstem Gehalt

cols_1961 <- c("men_subalterni", "men_operai_qualificati", 
               "women_subalterne", "women_operaie_qualificate")
Fabian_1961[cols_1961] <- Fabian_1961[cols_1961] %>%
  mutate(across(everything(), ~na_if(., "n.d."))) %>%
  mutate(across(everything(), ~na_if(., "-"))) %>%
  mutate(across(everything(), ~na_if(., "?"))) %>%
  mutate(across(everything(), as.numeric))

# Für 1961 (Fabian)
Fabian_1961 %>%
  filter(!is.na(men_subalterni)) %>%
  arrange(desc(men_subalterni)) %>%
  slice(1) %>%
  select(province, men_subalterni)

# Für 1971 (angenommen, Korbinian_1971 & Kristina_1971 zusammenführen)
data_1971 <- bind_rows(Korbinian_1971, Kristina_1971)
data_1971 %>%
  filter(!is.na(subalterno)) %>%
  arrange(desc(subalterno)) %>%
  slice(1) %>%
  select(province, subalterno)

# Für 1981 (analog)
data_1981 <- bind_rows(Korbinian_1981, Kristina_1981)
data_1981 %>%
  filter(!is.na(subalterno)) %>%
  arrange(desc(subalterno)) %>%
  slice(1) %>%
  select(province, subalterno)
#-------------------------------------------------------
#Streudiagramm
# Für 1961: Durchschnitt je Provinz berechnen (über die 4 Spalten)
Fabian_1961$mean_1961 <- rowMeans(Fabian_1961[, c("men_subalterni", "men_operai_qualificati", 
                                                  "women_subalterne", "women_operaie_qualificate")], 
                                  na.rm = TRUE)
fabian_top <- Fabian_1961 %>%
  arrange(desc(mean_1961)) %>%
  slice(1:5)
fabian_bottom <- Fabian_1961 %>%
  arrange(mean_1961) %>%
  slice(1:5)
fabian_selected <- bind_rows(fabian_top, fabian_bottom) %>%
  mutate(year = 1961, value = mean_1961) %>%
  select(province, year, value)

# Für 1971
data_1971 <- bind_rows(Korbinian_1971, Kristina_1971) %>%
  mutate(across(c("subalterno", "qualificato"), as.numeric),
         mean_1971 = rowMeans(across(c(subalterno, qualificato)), na.rm = TRUE))

data_1971_top <- data_1971 %>%
  arrange(desc(mean_1971)) %>%
  slice(1:5)
data_1971_bottom <- data_1971 %>%
  arrange(mean_1971) %>%
  slice(1:5)
data_1971_selected <- bind_rows(data_1971_top, data_1971_bottom) %>%
  mutate(year = 1971, value = mean_1971) %>%
  select(province, year, value)

# Für 1981
data_1981 <- bind_rows(Korbinian_1981, Kristina_1981) %>%
  mutate(across(c("subalterno", "qualificato"), as.numeric),
         mean_1981 = rowMeans(across(c(subalterno, qualificato)), na.rm = TRUE))

data_1981_top <- data_1981 %>%
  arrange(desc(mean_1981)) %>%
  slice(1:5)
data_1981_bottom <- data_1981 %>%
  arrange(mean_1981) %>%
  slice(1:5)
data_1981_selected <- bind_rows(data_1981_top, data_1981_bottom) %>%
  mutate(year = 1981, value = mean_1981) %>%
  select(province, year, value)
```{r}

```

# Zusammenfassen
plot_data <- bind_rows(fabian_selected, data_1971_selected, data_1981_selected)

# Scatterplot
ggplot(plot_data, aes(x = year, y = value, color = province)) +
  geom_point(size = 3) +
  geom_line(aes(group = province), alpha = 0.6) +
  labs(title = "Provinzen mit den höchsten und niedrigsten Durchschnittslöhnen",
       x = "Jahr", y = "Durchschnittslohn") +
  theme_minimal()

#----------------------------------------
#Histogramm

data_1971 <- bind_rows(Korbinian_1971, Kristina_1971) %>%
  mutate(across(c("subalterno", "qualificato"), ~na_if(., "n.d."))) %>%
  mutate(across(c("subalterno", "qualificato"), ~na_if(., "-"))) %>%
  mutate(across(c("subalterno", "qualificato"), ~na_if(., "?"))) %>%
  mutate(across(c("subalterno", "qualificato"), as.numeric)) %>%
  rowwise() %>%
  mutate(wages = mean(c(subalterno, qualificato), na.rm = TRUE)) %>%
  ungroup()

data_1971 %>% 
  filter(is.na(wages)) %>%
  select(province, subalterno, qualificato)

plot_data <- data_1971 %>%
  filter(!is.na(wages))

data_1971 <- bind_rows(Korbinian_1971, Kristina_1971) %>%
  mutate(across(
    c("subalterno", "qualificato"),
    ~suppressWarnings(as.numeric(
      na_if(na_if(na_if(., "n.d."), "-"), "?")
    ))
  ))






#Assignment for today
