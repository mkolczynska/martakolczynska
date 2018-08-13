# R code for "Educational attainment: surveys vs. administrative records"
# mkolczynska.com/post/education-sdr-oecd/
# Marta Kołczyńska
# 10 August 2018

#### Setup

library(OECD) # getting OECD data
library(dataverse) # getting data from Dataverse
library(tidyverse) # cleaning and reshaping data
library(countrycode) # switching between country codes
library(ggplot2) # plots
library(knitr) # for pretty tables (kables)

#### OECD

# Note: The entire database is huge and takes several minutes to download. According to the `oecd` package 
# [instructions](https://github.com/expersso/OECD), it is possible to specify a filter in `get_dataset`. 
# This would be great, since I only need a small subset of the data, but unfortunately I haven't been able 
# to get the filetr to work.
#   
# After downloading the database I select the subset I'm interested in:  
# 1. the indicator "Share of population by educational attainment" (`NEAC_SHARE_EA`),  
# 2.the education level measured "At least upper secondary education" (`L3T8`),  
# 3. year range excluding "Latest available year" (code `9999),  
# 4. the type of the measure `VALUE` (as opposed to standard error, `SE`),  
# 5. both sexes,  
# 6. age range of 25-64 (`Y25T64`),  
# 7. only individual countries, i.e. excluding country groups such as the G20 or the OECD average.  

dataset_list <- get_datasets()
search_dataset("education", data = dataset_list)

dataset <- "EAG_NEAC"

oecd <- get_dataset(dataset = dataset) %>% 
filter(INDICATOR == "NEAC_SHARE_EA", 
       ISC11A == "L3T8",
       obsTime != "9999",
       MEASURE == "VALUE",
       SEX == "T",
       AGE == "Y25T64",
       !COUNTRY %in% c("E22", "OAVG", "G20", "NMEC")) %>%
  mutate(iso3c = COUNTRY, year = as.numeric(obsTime)) %>%
  select(iso3c, year, obsValue)

#### SDR

# The SDR Master File, which contains the harmonized individual-level survey variables, can be 
# downloaded from Dataverse to R with the dataverse package: https://github.com/IQSS/dataverse-client-r. 
# The stata version of the file has the `id = 3006244`, a parameter in the `get_file` function. 
# The data file is pretty large, so it takes a while to download.  

master.raw <- get_file(3006244)
tmp <- tempfile(fileext = ".dta")
writeBin(as.vector(master.raw), tmp)
master <- haven::read_dta(tmp)

# #### Cleaning and merging SDR and OECD data
# 
# 1. Filter the SDR Master File to include only respondents aged between 25 and 64,
# 2. using the "Education level" variable (`t_edu`) create a boolean variable (`hs`) indicating whether 
#    the respondent completed upper secondary education,
# 3. group by national survey (`t_survey_name`, `t_survey_edition`, `t_country_l1u`, `t_country_set`),
# 4. calculate weighted proportions of the `hs` variable by national sample - `mean_hs`,
# 5. calculate the proportion of missing values in the `t_edu` variable,
# 6. filter surveys for which `mean_hs` is available and which cover whole country samples (as opposed 
#    to split-country samples, such as West and East Germany or Wallonia and Flanders),
# 7. covert SDR country codes from ISO2 to ISO3,
# 8. merge the SDR data with the OCD data on the country code and year,
# 9. Construct a flag for survey proportion less than half of the OECD proportion,
# 10. filter out cases with missing values.

merge <- master %>% filter(t_age >= 25 & t_age <= 64) %>%
mutate(hs = ((t_edu == 30 & c_edu_incomplete == 0) | 
(t_edu > 30 & t_edu < 90)) ) %>%
group_by(t_survey_name, t_survey_edition, t_country_l1u, t_country_set) %>%
summarise(mean_hs = 100 * weighted.mean(hs, t_weight_l1u_2, na.rm = TRUE),
year = mean(t_country_year),
prop_edu_mis = mean(is.na(t_edu) | t_edu == 90)) %>%
filter(!is.na(mean_hs), nchar(t_country_l1u) == 2) %>%
mutate(iso2c = substr(t_country_l1u, 1, 2),
iso3c = countrycode(iso2c, 'iso2c', 'iso3c'),
continent = countrycode(iso2c, 'iso2c', 'continent')) %>%
inner_join(oecd, by = c("iso3c", "year")) %>%
mutate(off = (mean_hs < obsValue * 0.5 | mean_hs * 0.5 > obsValue )) %>%
filter(!is.na(obsValue))

# Scatter plot of SDR % respondents with at least upper secondary education and the corresponding OECD value.

ggplot(merge, aes(x=obsValue, y=mean_hs, col = off)) + geom_point(show.legend=F) + 
scale_color_manual(values=c("black", "red")) +
ggtitle("Fig. 1. Educational attainment from SDR and OECD") + 
xlab("OECD: % of population with at least upper secondary education") +
ylab("SDR: % of population with at least upper secondary education") +
xlim(0,100) + ylim(0,100) +
geom_abline(intercept = 0, slope = 1)

# Separate plots for each survey project.

ggplot(merge, aes(x=obsValue, y=mean_hs, col = off)) + geom_point(show.legend=F) + 
scale_color_manual(values=c("black", "red")) +
ggtitle("Fig. 2. Educational attainment from surveys and OECD by project") + 
xlab("OECD: % of population with at least upper secondary education") +
ylab("SDR: % of population with at least upper secondary education") +
xlim(0,100) + ylim(0,100) +
geom_abline(intercept = 0, slope = 1) + 
facet_wrap("t_survey_name")

# Proportions of respondents with at least upper secondary education in ISSP/Switzerland and 
# corresponding OECD data.

merge %>% filter(t_survey_name == "ISSP" & t_country_l1u == "CH") %>%
arrange(t_survey_edition) %>% select(1,2,3,5,10) %>%
kable(digits = 3, align=c("c", "c", "c", "c")) %>%
kable_styling(full_width = F, position = "left") %>%
column_spec(1:5, width = "10em")
