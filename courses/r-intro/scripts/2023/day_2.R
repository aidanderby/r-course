# day_2.R
# ~~~~~~~
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

files = list.files("courses/r-intro/data/taught/part_2/",
                   full.names = TRUE)

siteNames = basename(files) |>  
  word(1, 1, sep = "_") |> 
  tolower()

siteList = list()

for(i in 1:length(files)){
  siteList[[i]] = read.csv(files[i]) |> 
    tibble() |> 
    mutate(site = siteNames[i]) |> 
    select(-X)
  
  if(siteNames[i] == "lon6"){
    siteList[[i]] = siteList[[i]] |> 
    mutate(date = as_datetime(date),)
  }else{
      siteList[[i]] = siteList [[i]] |> 
        mutate(date = ymd_hms(date))
    }
    
  if(siteNames[i] == "kc1"){
    siteList[[i]] = siteList[[i]] |> 
      mutate(no = ifelse(no == "missing", NA, no),
              no = as.numeric(no))
    
  }
}

sites_middle = bind_rows(siteList)

sites_middle |> count(site)

#approach 1
mean(sites_middle$no2, na.rm=TRUE)

#summarise/summarize

#workforce dplyr functions = select, rename, mutate, group_by, summarise, arrange

sites_middle |> 
  group_by(site) |> 
  summarise(mean_no2 = mean(no2, na.rm=TRUE),
            st_no2 = sd(no2, na.rm=TRUE)) |> 
              arrange(desc(mean_no2))


#pivot_wider and pivot_longer
#matches up on date (same datetime has 16 diff observations)
sites_wide = sites_middle |> 
  pivot_wider(names_from=site, values_from=c(no2, no, o3))


sites_long = sites_middle |> 
  pivot_longer(c(no, no2, o3), names_to = "species", values_to = "conc")

sites_long |> 
  group_by(site, species) |> 
  summarise(mean_conc = mean(conc, na.rm = TRUE),
            sd_conc = sd(conc, na.rm = TRUE)) |> 
  ungroup() #removes explicit groups from tibble

#mutate also works with group_by
#flag values > 95% quantile - naive outlier detection
sites_long |> 
  group_by(site, species) |> 
  mutate(p_95 = quantile(conc, 0.95, na.rm = T),
         flag = conc> p_95) |> 
  ungroup()

#filter

sites_long |> 
  filter(site == "cll2", #comma combines conditions as ANDs
         species == "no") # double == for equality

sites_long |> 
  filter(site == "cll2" | site == "kc1") |> 
  count(site)

sites_long |> 
  filter(site != "cll2") |> 
  count(site)

sites_long |> 
  filter(site %in% c("cll2", "kc1", "my1")) |> 
  count(site)

sites_long |> 
  filter(species == "no2",
         conc > 100) 

#can also use conditions on datetime
#between function also works with inclusive boundaries

sites_long |> 
  filter(date >= as_datetime("2018-12-25 00:00:00"),
         date < as_datetime("2018-12-26 00:00:00"))

sites_long |> 
  group_by(site, species) |> 
  filter(conc >= quantile(conc, 0.95, na.rm = T)) |> 
  ungroup()

#more time-based functionality
#time-averaging from hourly to daily
sites_long |> 
  mutate(date = floor_date(date, "day")) |> 
  group_by(date, site, species) |> 
  summarise(conc = mean(conc, na.rm = T)) |> 
  ungroup()

#find hour of maximum concentration
sites_long |> 
  mutate(hour_of_day = hour(date)) |> 
  group_by(hour_of_day, site, species) |> 
  summarise(conc = mean(conc, na.rm = T)) |> 
  ungroup() |> 
  group_by(site, species) |> 
  filter(conc == max(conc, na.rm = T)) |> 
  ungroup()

#wday and yday get day of week/year
sites_long |> 
  mutate(week_day = wday(date), # 1 = sunday 
         year_day = yday(date)) 
  
sites_long |> 
  select(date) |> 
  mutate(
    plus_one = date + days(1)
  )

#lag - gets previous row values
sites_long |> 
  group_by(site, species) |> 
  arrange(date) |> 
  mutate(prev_conc = lag(conc),
         conc_diff = conc - prev_conc) |> 
  ungroup() |> 
  arrange(site, species, date) |> 
  slice(8755:8765)


