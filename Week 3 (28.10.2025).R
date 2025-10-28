
#R very basic operations


#creating a vector
v <- c(1, 2, 3, 4)


#calculate the mean






v <- c(1, 2, 3, 4)
#sum of vectors


#basic functions 

mean(v)

#standard deviation
sd(v)

#if you do not know: use
?sd


#Operator	Description

#  +	  #addition
#  -	  #subtraction
#  *	  #multiplication
#  /	  #division
#  ^ **	#exponentiation
#  x %% y	#modulus (x mod y) 5%%2 is 1
#  x %/% y	#integer division 5%/%2 is 2
#
#  <	  #less than
#  <=	#less than or equal to
#  >	  #greater than
#  >=	#greater than or equal to
#  ==	#exactly equal to - logical operator
#  !=	#not equal to
#  !x	#Not x
#  x | y	#x OR y
#  x & y	#x AND y
#  %>% #pipe operator in "magrittr"

#summary statistics
summary()


#Set a new working directory#
setwd("C:/Users/paolo/Dropbox/WiSe 2025-2026/QMH (WiSe 2025-2026)/Quantitative-methods-for-historians")

getwd()

#install.packages("readxl")
library(readxl)

Fabian_1961    <- read_excel("Fabian_1961.xlsx")
Korbinian_1971 <- read_excel("Korbinian_1971.xlsx")
Korbinian_1981 <- read_excel("Korbinian_1981.xlsx")
Kristina_1971  <- read_excel("Kristina_1971.xlsx")
Kristina_1981  <- read_excel("Kristina_1981.xlsx")







#Open the data (example task)
GDP <- read_excel("GDP.xlsx")

data <- read_excel("education.xlsx")

####Part 1 - Exploring data####


#some data cleaning first

#install.packages("dplyr")
library(dplyr)

data <- data %>% select(-student)

#data <- na.omit(data)


#Question 1: What is the province with most schools?

data <- data %>%
  filter(!is.na(schools_primary) & schools_primary != '') 

  
  
library(ggplot2)

# Create the bar plot
p <- ggplot(school_counts, aes(x = reorder(province, -schools_primary), y = schools_primary)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Schools per Province",
       x = "Province",
       y = "Number of Schools") +
  theme_minimal()

p

#install.packages("plotly")
library(plotly)

interactive_plot <- ggplotly(p)

interactive_plot


#Question 2: plot only the provinces with more than 2000 schools

data_5 <- data %>%
  filter(!is.na(schools_primary) & schools_primary != '') %>%
  filter(schools_primary > 500)

ggplot(data_5, aes(x = province, y = schools_primary)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Schools per Province",
       x = "Province",
       y = "Number of Schools") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Question 3: plot the number of schools into a bar chart
# x = number of schools
# y = number of provinces

ggplot(data, aes(x = schools_primary)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +  # Create histogram with specified bin width
  labs(title = "Distribution of Provinces by Number of Primary Schools",
       x = "Number of Schools",
       y = "Number of Provinces") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#With mean and standard deviation
mean_value <- mean(data$schools_primary, na.rm = TRUE)
sd_value <- sd(data$schools_primary, na.rm = TRUE)

####### Plot with vertical line at mean and annotate standard deviation######
ggplot(data, aes(x = schools_primary)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  geom_vline(xintercept = mean_value, color = "red3", linetype = "dashed", size = 1) +  # Mean line
  geom_vline(xintercept = mean_value + sd_value, color = "darkgreen", linetype = "dashed", size = 1) +  # Mean line
  geom_vline(xintercept = mean_value - sd_value, color = "darkgreen", linetype = "dashed", size = 1) +  # Mean line
  annotate("text", x = mean_value, y = Inf, label = paste0("Mean = ", round(mean_value, 1)),
           color = "red3", vjust = -0.5, angle = 90, size = 3.5) +
  annotate("text", x = mean_value + sd_value, y = Inf, label = paste0("+1 SD = ", round(mean_value + sd_value, 1)),
           color = "darkgreen", vjust = -0.5, angle = 90, size = 3.5) +
  annotate("text", x = mean_value - sd_value, y = Inf, label = paste0("-1 SD = ", round(mean_value - sd_value, 1)),
           color = "darkgreen", vjust = -0.5, angle = 90, size = 3.5) +
  labs(title = "Distribution of Provinces by Number of Primary Schools",
       x = "Number of Schools",
       y = "Number of Provinces") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






######Part 2 - Presenting time series#####

data_ts <-  data %>%
  group_by(year) %>%
  summarise(
    total_primary_schools = sum(schools_primary, na.rm = TRUE),
    total_middle_schools = sum(schools_middle, na.rm = TRUE),
    total_primary_students = sum(students_primary, na.rm = TRUE))

#Since a lot of years are still missing
data_ts <- data_ts %>%
  filter(total_primary_schools > 0 & total_primary_students > 0)

#a very easy plot
plot(data_ts$year, data_ts$total_primary_schools, 
     type = "p", col = "black",pch = 19, 
     xlab = "Year", ylab = "Total Primary Schools", main = "Total Primary Schools Over Years")

#let's put some nice labels
text(data_ts$year, data_ts$total_primary_schools, 
     labels = data_ts$year, pos = 4, cex = 0.7, col = "black")


#let's make a nicer plot with GGPLOT

#install.packages("ggplot2")
library(ggplot2)


ggplot(data_ts, aes(x = factor(year), y = total_primary_schools)) +
  geom_point() +
  #  labs(title = "Distribution of Values by Year",  x = "Year", y = "Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #some random aesthetics


#Question 3: plot the number of and students over the years

ggplot(data_ts, aes(x = factor(year), y = total_primary_students)) +
  geom_point() +
  #  labs(title = "Distribution of Values by Year",  x = "Year", y = "Value") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #some random aesthetics



####3. Let's relate our data with data on population (data2 on Moodle)####


data2 <- read_excel("C:/Users/paolo/Downloads/data2.xlsx")

merged_data <- left_join(data, data2, by = c("year", "province"))

data3 <- read_excel("C:/Users/paolo/Downloads/data3.xlsx")

merged_data <- left_join(merged_data, data3, by = c("year", "province"))


View(merged_data)

merged_data <- merged_data %>%
  mutate(students_primary_pop = students_primary / population * 100,
         students_middle_pop = students_middle / population * 100,
         schools_primary_pop = schools_primary / population * 100,
         schools_middle_pop = schools_middle / population * 100)


#Question4: Plot a histogram with the (x = students/population, y = n. of provinces)

#before plotting, do the summary!
summary(merged_data$students_primary_pop)

ggplot(merged_data, aes(x = students_primary_pop)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Students per Population",
       x = "Students per Population",
       y = "Number of Provinces") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Scatter plots







#Maps




#

install.packages("chattr")
library(chattr)