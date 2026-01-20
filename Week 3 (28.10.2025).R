
#R very basic operations


#creating a vector
v <- c(1, 2, 3, 4, 10)

#calculate the mean for the vector v
mean(v)
median(v)

d <- c(27, 40)

#merge the two vectors
v2 <- c(v, d)

v3 <- v + 1


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
summary(v)


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



