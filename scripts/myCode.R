## September 3rd 2026
library(tidyverse)
library(readxl)

# 1. Import sttstj_all_sites and inspect the data set. What is the data structure and the data type of each field?
sttstj_all_sites <- read_xlsx('data/sttstj_all_sites.xlsx', sheet = "locations")
metadata <- read_xlsx('data/sttstj_all_sites.xlsx', sheet = "Metadata")
print(sttstj_all_sites)
# This data is in a wide format for the sheet location with the column names "yr_site" as a character, "year" as a number,    "lat" and "lon" are coordinates as a number, and "habitat" as a character. The metadata sheet explains the column names.

# 2. How many samples are in the data set?
glimpse(sttstj_all_sites)
nrow(sttstj_all_sites) #vector
count(sttstj_all_sites) #tibble
# There are 2505 samples in this data set

# 3. How many unique habitat types are there? What are the names of each habitat?
unique(sttstj_all_sites$hab)
# There are 6 unique habitat types in this data set that are named "PVMT" "AGRF" "SCR"  "PTRF" "BDRK" "HARD"

# 4. How many sites are there in the “PVMT” habitat type?
length(unique(sttstj_all_sites$yr_site[sttstj_all_sites$hab == "PVMT"]))
sum(sttstj_all_sites$hab == "PVMT", na.rm = TRUE)
# There are 571 sites in the habitat "PVMT"

# 5. How many sites are there in the “PVMT” or “AGRF” habitat types?
sum(sttstj_all_sites$hab == "PVMT"|sttstj_all_sites$hab == "AGRF", na.rm = TRUE)
# There are 1538 sites in the habitat "PVMT" or "AGRF"

# 6. How many sites are there in all the habitat types EXCEPT “PVMT”?
sum(sttstj_all_sites$hab != "PVMT", na.rm = TRUE)

# 7. Show the first 5 records (rows) of… all sites sampled in “AGRF” or “SCR” only show the “yr_site” and “hab” columns
firstRows <- sttstj_all_sites |> 
  filter(hab == "AGRF" | hab == "SCR") |> 
  head(5)
firstRows

firstRows_col <- firstRows |> 
  select(yr_site, hab)

# 8. What is the northern most site sampled in the “BDRK” habitat?
north <- sttstj_all_sites |> 
  filter(hab == "BDRK") |> 
  slice_max(lat) |> 
  pull(yr_site)

# 9. What is the western most site sampled in either “AGRF” or “PTRF” habitat?
west <- sttstj_all_sites |> 
  filter(hab == "AGRF" | hab == "PTRF") |> 
  slice_min(lon) |> 
  pull(yr_site)

# 10. How many years were surveyed in the data set?
yrs <- sttstj_all_sites |> 
  distinct(year) 
count(yrs)
# There are 16 years 

# 11. What is the eastern most site sample in the “AGRF” habitat in 2004?
east <- sttstj_all_sites |> 
  filter(hab == "AGRF" & year == 2004) |> 
  slice_max(lon) |> 
  pull(yr_site)

# 12. Of the total number of samples, what percentage was surveyed on patch reef (“PTRF”)?
PTRF <- sttstj_all_sites |> 
  summarize(percent = mean(hab == "PTRF")*100) |> 
  pull(percent)
