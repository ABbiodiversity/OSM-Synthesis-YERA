#-------------------------------------------------------------------------------

# Title: YERA data tidy
# Created: September 4, 2019

# Objectives: Tidy YERA data from E.Bayne.


#-------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(fs)
library(readxl)

# Define file path
path <- "./data/base/FINAL-YERA-DATA-FOR-STATUS-REPORTb.xls"

# Read in Erin's YERA data
ls_yera <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

#-------------------------------------------------------------------------------

# Clean up individual sheets

# Raw occupancy
df_occupy <- ls_yera[[1]] %>%
  select(ss:Y20184) %>%
  na_if(".") %>%
  gather(key = "year", value = "occupied", Y20131:Y20184) %>%
  separate(year, into = c("year", "sample"), sep = -1, remove = TRUE) %>%
  mutate(year = str_remove(year, "Y"))

df_count <- df_occupy %>%
  group_by(ss, year) %>%
  mutate(occupied = as.numeric(occupied)) %>%
  summarise(count = sum(occupied, na.rm = TRUE))
  # ^ same as `YERA COUNT per 4` tab

df_occ_summary <- df_occupy %>%
  mutate(occupied = ifelse(occupied == 1, TRUE, FALSE)) %>%
  group_by(ss, year) %>%
  summarise(yera_occupied = ifelse(any(occupied), TRUE, FALSE))
  # ^ same as `FINAL YERA DATA FOR STATUS REPO` tab

# Standardized Day of Year
df_doy <- ls_yera[[3]] %>%
  select(ss:stdsdoy20184) %>%
  na_if(".") %>%
  gather(key = "year", value = "stdsdoy", stdsdoy20131:stdsdoy20184) %>%
  mutate(year = str_sub(year, 8, 11))

df_mclelland <- ls_yera[[16]] %>%
  select(ss, mclelland = McLelland) %>%
  na_if("<Null>")

# Site metadata
df_meta <- ls_yera[[4]] %>%
  select(data_set:longit, mineable = Mineablebin) %>%
  select(ss, everything()) %>%
  mutate(lapr = 1) %>%
  left_join(df_mclelland, by = "ss")

#-------------------------------------------------------------------------------

# Export processed data

write_csv(df_occupy, "./data/processed/yera_occupy_2013-18.csv")

write_csv(df_doy, "./data/processed/yera_doy_2013-18.csv")

write_csv(df_meta, "./data/lookup/yera_ss-meta_2013-18.csv")

#-------------------------------------------------------------------------------


















