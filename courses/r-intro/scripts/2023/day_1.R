library(openair)
library(dplyr)
library(lubridate)


#read NO
my1_no = read.csv("courses/r-intro/data/taught/part_1/MY1_no_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

#read NO2
my1_no2 = read.csv("courses/r-intro/data/taught/part_1/MY1_no2_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

#read met
my1_met = read.csv("courses/r-intro/data/taught/part_1/MY1_met_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

#read o3
my1_o3 = read.csv("courses/r-intro/data/taught/part_1/MY1_o3_2018.csv") |> 
  tibble() |> 
  mutate(date = ymd_hms(date, tz = "GMT"))

#combine the tables
my1 = my1_no |> 
  left_join(my1_no2, by = "date") |> 
  left_join(my1_o3, by = "date") |> 
  left_join(my1_met, by = "date")

mean(my1$no, na.rm = TRUE)
median(my1$no, na.rm = TRUE)
sd(my1$no, na.rm = TRUE)

#summary of no column
summary(my1$no)

#summary of entire dataframe
summary(my1)
hist(my1$no2)
density(my1$no, na.rm = TRUE) |> plot()

#diagnostic check dates are in order (1:1 rel of date with index)
plot(my1$date, type = "l")
#line plot with explicit y and x axes
plot(my1$date, my1$o3, type = "l")

plot(my1$no2, my1$o3)

#linear model of relationship between no2 and o3
mod = lm(no2 ~ no, data=my1)
#summary of the model
summary(mod)

#gets coefficients from model
coefficients = coef(mod)

#to get r^2 save output of summary
mod_sum = summary(mod)
mod_sum$r.squared
names(mod_sum)

plot(my1$no, my1$no2)
abline(a=coefficients[1], b=coefficients[2], col = "blue")

#colours in R are shown by colours()