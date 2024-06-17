# I have unnested the QuiltIndex data and renamed columns,
# so now it's time to clean.

install.packages("skimr")
install.packages("janitor")
install.packages("stringdist")
install.packages("maps")
install.packages("humaniformat")
install.packages("naniar")
install.packages("diyar")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library("skimr")
library("rnaturalearth")
library("rnaturalearthdata")
library("diyar")
library("naniar")
library("humaniformat")
library("maps")
library("janitor")
library("stringdist")
library("tidyverse")
library("stringi")

#read data -------------------------------------------
maker_data <- read.csv(
  "02_unnested_data.csv",
  na.strings = c(NA, "NA", "Unknown", "unknown", "NULL", ""),
  colClasses = c(maker_group_ending_date = "character",
                 reservation = "character")
)

read_problems <- problems(maker_data)


#remove unused columns---------------------------------------------------------
#so - although previously I narrowed down the data to include only quilt maker
#data (and not quilt data), there is still some data that I do not intend to
#use for this analysis 
maker_data <- maker_data %>%
  select(-contains("_address")) %>%
  select(-contains("fathers_")) %>%
  select(-contains("mothers_")) %>%
  select(-contains("sell_")) %>%
  select(-contains("belong_to_"))  %>%
  select(-contains("spouses_"))%>%
  select(-contains("_indices"))

gg_miss_var(maker_data, show_pct = TRUE)

maker_data <- maker_data %>%
  select(-c(phone,
            X,
            maiden_name,
            death_date,
            religious_affiliation,
            occupation,
            unknown_date_info,
            quilt_date_other_info,
            maker_group_founding_date,
            maker_group_ending_date,
            maker_group_characteristics,
            marriage_date,
            quiltmaker_why_other,
            teach_quilting,
            county,
            number_children
            ))
  
colnames(maker_data)
#ok. looks good. moving on.

# checking missing data -------------------------------------
pct_miss(maker_data)
pct_miss_case(maker_data)
pct_complete_case(maker_data)

gg_miss_var(maker_data, show_pct = TRUE)

# clean data and handle missing data -----------------------------------------
#there is a lot of missing data. This is to be expected because maker
#data is not available for every quilt. I can extrapolate some data and I will 
#drop some. And in the process I need to clean some.

## remove reservation --------------------------
#reservation is 100% blank so it can be removed
maker_data <- maker_data %>%
  select(-reservation)

##work on country --------------------------------
#checking what currently exists
filtered <- maker_data %>%
  group_by(country) %>%
  count(country)

#I want to use state and province to determine country where possible,
#but first I need to fix records that have both, which would be problematic.
filtered <- maker_data %>%
  filter(!is.na(state) & !is.na(province) & is.na(country))
#ok, none exist, so I can move on.

#And where state and country are missing, state data is often available 
#in the city, so I need to tease that out before moving forward.
filtered <- maker_data %>%
  filter(is.na(country) & is.na(state) &
           grepl("[A-Z][A-Z]", city))

#set the state to the last two letter upper-case abbreviation
#in the city, if there is one.
maker_data <- maker_data %>%
 mutate(state = if_else(
    is.na(country) & is.na(state) & grepl("\\b[A-Z][A-Z]\\b", city),
    map_chr(str_extract_all(city, "\\b[A-Z][A-Z]\\b"), last),
    state)) 
           
#checking...
filtered <- maker_data %>%
  filter(is.na(country) & is.na(state) &
           grepl("[A-Z][A-Z]", city)) %>%
  select(city, state)

#checking...
filtered <- maker_data %>%
  filter(is.na(country) & is.na(state) & !is.na(city)) %>%
  select(city, state, province, country)

###find country based on state -----------------------------

#checking if state can help us determine country
filtered <- maker_data %>%
  filter(is.na(country) & !is.na(state) & is.na(province))

table(filtered$state, useNA = "always")

#for this data, when country is missing and state is not, 
#the country is always United States. (filter out any
#situation where province is also available)
maker_data <- maker_data %>% 
  mutate(country = replace(country, 
                           is.na(country) & 
                             !is.na(state) & 
                             is.na(province), 
                           "United States of America"))

#checking...
filtered <- maker_data %>%
  group_by(country) %>%
  count(country)


gg_miss_var(maker_data, show_pct = TRUE)

###find country based on province ---------------------------

#checking if province can help us determine country
filtered <- maker_data %>%
  filter(is.na(country) & !is.na(province) & is.na(state))

table(filtered$province, useNA = "always")

#for this data, when country is missing and province is not, 
#the province is always in Canada (filter out any situation 
#where state is also available)
maker_data <- maker_data %>% 
  mutate(country = replace(country, 
                           is.na(country) & 
                             !is.na(province) &
                             is.na(state),
                           "Canada"))

#checking...
filtered <- maker_data %>%
  group_by(country) %>%
  count(country)

gg_miss_var(maker_data, show_pct = TRUE)

###find country based on zipcode -------------------------------------
filtered <- maker_data %>%
  filter(is.na(country) & !is.na(zip_code))
#there is never a zip_code without a country so we can't use zip_code to help
#figure out country. 

###find country based on city -------------------------------------
#see if city can help figure out country
filtered <- maker_data %>%
  filter(is.na(country) & !is.na(city))

GetCountryFromCity = function(city, country) {
  if (is.na(country) & !is.na(city)) {
    ifelse(length(world.cities[which(world.cities$name == city), ]$country.etc) == 1,
           world.cities[which(world.cities$name == city), ]$country.etc[[1]],
           country)
  } else {
    country
  }
}

maker_data <- maker_data %>%
  mutate(country = map2_chr(city, country, GetCountryFromCity))

filtered <- maker_data %>%
  group_by(country) %>%
  count(country)

gg_miss_var(maker_data, show_pct = TRUE)

###remove records where country is still missing ------------------------------
#and now that I have done everything I can to figure out the country,
#remove the data where country is missing
maker_data <- maker_data %>%
  filter(!is.na(country))

filtered <- maker_data %>%
  group_by(country) %>%
  count(country)

gg_miss_var(maker_data, show_pct = TRUE)

###clean country data to match rnaturalearth ---------------------
#now make sure that all of the countries are match countries in rnaturalearth
world <- ne_countries()
unique(maker_data$country)[is.na(match(unique(maker_data$country), world$admin))]

#checking various records
filtered <- maker_data %>%
  filter(country == "United Nations")

#standardizing the country
maker_data <- maker_data %>% 
  mutate(country = case_when(
    country == "United States"            ~ "United States of America",
    country == "USA"                      ~ "United States of America",
    country == "England"                  ~ "United Kingdom",
    country == "Tanzania"                 ~ "United Republic of Tanzania",
    country == "United Nations"           ~ "United States of America",
    #this is probably a mis-mark based on state for this record
    country == "Scotland"                 ~ "United Kingdom",
    country == "Bosnia and Herzegowina"   ~ "Bosnia and Herzegovina",
    country == "UK"                       ~ "United Kingdom",
    TRUE ~ country ))

filtered <- maker_data %>%
  group_by(country) %>%
  count(country)

##work on birth_place_country------------------------------------

#checking what currently exists
filtered <- maker_data %>%
  group_by(birth_place_country) %>%
  count(birth_place_country)

# I want to use state and province to determine birth_place_country where
# possible, but first I need to fix records that have both, which would be
# problematic.
filtered <- maker_data %>%
  filter(!is.na(birth_place_state) & 
           !is.na(birth_place_province) & is.na(birth_place_country))

#ok, fix those up: Virginia is wrong and Winslow, Pollacca is wrong.
maker_data <- maker_data %>%
  mutate( birth_place_state = 
            if_else(
              birth_place_state == "Virginia" &
                !is.na(birth_place_province) & 
                is.na(birth_place_country),
              NA,
              birth_place_state))

maker_data <- maker_data %>%
  mutate(birth_place_province = 
           if_else(
              !is.na(birth_place_state) & 
                birth_place_province =="Winslow, Pollacca" & 
                is.na(birth_place_country),
              NA,
              birth_place_province
  ))

#And where state and country are missing, state data is often available 
#in the city, so I need to tease that out before moving forward.
filtered <- maker_data %>%
  filter(is.na(birth_place_country) & is.na(birth_place_state) &
           grepl("[A-Z][A-Z]", birth_place_city))

#set the state to the last two letter upper-case abbreviation
#in the city, if there is one.
maker_data <- maker_data %>%
  mutate(birth_place_state = if_else(
    is.na(birth_place_country) & is.na(birth_place_state) & grepl("\\b[A-Z][A-Z]\\b", birth_place_city),
    map_chr(str_extract_all(birth_place_city, "\\b[A-Z][A-Z]\\b"), last),
    birth_place_state)) 

#checking...
filtered <- maker_data %>%
  filter(is.na(birth_place_country) & is.na(birth_place_state) &
           grepl("[A-Z][A-Z]", birth_place_city)) %>%
  select(birth_place_city, birth_place_state)

#checking...
filtered <- maker_data %>%
  filter(is.na(birth_place_country) &
           is.na(birth_place_state) & !is.na(birth_place_city)) %>%
  select(birth_place_city,
         birth_place_state,
         birth_place_province,
         birth_place_country)


###find birth_place_country based on state --------------------
filtered <- maker_data %>%
  filter(is.na(birth_place_country) & !is.na(birth_place_state) &
           is.na(birth_place_province))

table(filtered$birth_place_state, useNA = "always")

#in this data, all of these states are US
maker_data <- maker_data %>%
  mutate(birth_place_country = replace(
    birth_place_country,
    is.na(birth_place_country) &
      !is.na(birth_place_state) &
      is.na(birth_place_province),
    "United States of America"
  ))

###find birth_place_country based on province --------------------

filtered <- maker_data %>%
  filter(is.na(birth_place_country) & !is.na(birth_place_province) &
           is.na(birth_place_state))

table(filtered$birth_place_province, useNA = "always")

maker_data <- maker_data %>% 
  mutate(birth_place_country = case_when(
    is.na(birth_place_country) & birth_place_province == "2: Ontario"  ~ "Canada",
    is.na(birth_place_country) & birth_place_province == "Alberta"  ~ "Canada",
    is.na(birth_place_country) & birth_place_province == "Bohemia"  ~ "Czechia", 
    is.na(birth_place_country) & birth_place_province == "Germany"  ~ "Germany",
    is.na(birth_place_country) & birth_place_province == "Holland"  ~ "Holland",
    is.na(birth_place_country) & birth_place_province == "Mexico"  ~ "Mexico",
    is.na(birth_place_country) & birth_place_province == "Pennsylvania"  ~ "United States",
    is.na(birth_place_country) & birth_place_province == "Russia"  ~ "Russia",
    !is.na(birth_place_country) ~ birth_place_country))    


###find birth_place_country based on city --------------------

filtered <- maker_data %>%
  filter(is.na(birth_place_country) & !is.na(birth_place_city))

maker_data <- maker_data %>%
  mutate(birth_place_country = map2_chr(birth_place_city, birth_place_country, GetCountryFromCity))

gg_miss_var(maker_data, show_pct = TRUE)

###remove records where birth_place_country is still missing ------------------

maker_data <- maker_data %>%
  filter(!is.na(birth_place_country))

###clean birth_place_country data to match rnaturalearth --------------------
unique(maker_data$birth_place_country)[is.na(match(unique(maker_data$birth_place_country), world$admin))]

maker_data <- maker_data %>% 
  mutate(birth_place_country = case_when(
    birth_place_country == "United States"            ~ "United States of America",
    birth_place_country == "England"                  ~ "United Kingdom",
    birth_place_country == "Russian Federation"       ~ "Russia",
    birth_place_country == "Scotland"                 ~ "United Kingdom",
    birth_place_country == "UK"                       ~ "United Kingdom",
    birth_place_country == "USA"                      ~ "United States of America",
    birth_place_country == "Holland"                  ~ "Netherlands",
    birth_place_country == "United Nations"           ~ "United States of America", 
    #United Nations was a mismark - as determined by birth_place_state
    birth_place_country == "Slovakia (Slovak Republic)" ~ "Slovakia",
    birth_place_country == "Bohemia"                  ~ "Czechia", 
    birth_place_country == "Czech Republic"           ~ "Czechia",
    birth_place_country == "Czechoslovakia"           ~ "Czechia",
    birth_place_country == "Siam"                     ~ "Thailand",
    birth_place_country == "NetherlandsZeeland"       ~ "Netherlands", #the true 
    #meaning of NetherlandsZeeland as determined by ethnicity on this record
    TRUE ~ birth_place_country ))

gg_miss_var(maker_data, show_pct = TRUE)

##work on quilt_country - the place where the quilt was made -----------

#checking what currently exists
filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)

#I want to use state and province to determine country where possible,
#but first I need to fix records that have both, which would be problematic.
filtered <- maker_data %>%
  filter(!is.na(quilt_state) & !is.na(quilt_province) & is.na(quilt_country))
#ok, investigating one record like this is looks like it should be province, 
#not state.
maker_data <- maker_data %>%
  mutate(quilt_state = if_else(
    !is.na(quilt_state) & !is.na(quilt_province) & is.na(quilt_country),
    NA,
    quilt_state))

###find quilt_country based on state -----------------------------

#checking if state can help us determine country
filtered <- maker_data %>%
  filter(is.na(quilt_country) & !is.na(quilt_state) & is.na(quilt_province))

table(filtered$quilt_state, useNA = "always")

#for this data, when country is missing and state is not,
#the country is always United States. (filter out any
#situation where province is also available)
maker_data <- maker_data %>%
  mutate(quilt_country = replace(quilt_country,
                           is.na(quilt_country) &
                             !is.na(quilt_state) &
                             is.na(quilt_province),
                           "United States of America"))

#checking...
filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)


gg_miss_var(maker_data, show_pct = TRUE)

###find quilt_country based on province ---------------------------

#checking if province can help us determine quilt_country
filtered <- maker_data %>%
  filter(is.na(quilt_country) & !is.na(quilt_province) & is.na(quilt_state))

table(filtered$quilt_province, useNA = "always")

#for this data, when country is missing and province is not,
#the province is always in Canada (filter out any situation
#where state is also available)
maker_data <- maker_data %>%
  mutate(quilt_country = replace(quilt_country,
                           is.na(quilt_country) &
                             !is.na(quilt_province) &
                             is.na(quilt_state),
                           "Canada"))

#checking...
filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)

gg_miss_var(maker_data, show_pct = TRUE)

###find quilt_country based on city -------------------------------------
#see if city can help figure out quilt_country
filtered <- maker_data %>%
  filter(is.na(quilt_country) & !is.na(quilt_city))

maker_data <- maker_data %>%
  mutate(quilt_country = map2_chr(quilt_city, quilt_country, GetCountryFromCity))

filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)

gg_miss_var(maker_data, show_pct = TRUE)

###remove records where quilt_country is still missing ------------------------------
maker_data <- maker_data %>%
  filter(!is.na(quilt_country))

filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)

gg_miss_var(maker_data, show_pct = TRUE)

# ###clean quilt_country data to match rnaturalearth ---------------------
unique(maker_data$quilt_country)[is.na(match(unique(maker_data$quilt_country), world$admin))]

#checking various records
filtered <- maker_data %>%
  filter(quilt_country == "American Samoa")

#standardizing the quilt_country
maker_data <- maker_data %>%
  mutate(quilt_country = case_when(
    quilt_country == "United States"            ~ "United States of America",
    quilt_country == "USA"                      ~ "United States of America",
    quilt_country == "England"                  ~ "United Kingdom",
    quilt_country == "United Nations"           ~ "United States of America",
    #this is probably a mis-mark based on state for these records
    quilt_country == "Scotland"                 ~ "United Kingdom",
    quilt_country == "American Samoa"            ~ "United States of America",
    #mismark: the quilt city/state indicate that this should be Iowa and thus USA
    TRUE ~ quilt_country ))

filtered <- maker_data %>%
  group_by(quilt_country) %>%
  count(quilt_country)

##work on province and state --------------------------------------------

###fix mismatches between country and state/province----
countries_with_states <- c("United States of America", 
                           "Germany",
                           "Mexico", 
                           "New Zealand",
                           "Austria",
                           "Australia",
                           "Brazil",
                           "India",
                           "Malaysia") #and others

filtered <-maker_data %>%
  filter(!is.na(province)) %>%
  group_by(country) %>%
  count(country)
#ok, it looks like US is the only one mis-matched to province,
#so work on that...

#province on US, which should be state
filtered <- maker_data %>%
  filter(country == "United States of America" & !is.na(province))

#state was mismarked in province field
maker_data <- maker_data %>%
  mutate(state = if_else(country == "United States of America" & 
                           is.na(state) & 
                           !is.na(province),
                         province,
                         state))

#checking...
filtered <- maker_data %>%
  filter(country == "United States of America" & !is.na(province))

#set province correctly for countries that have states
maker_data <- maker_data %>%
  mutate(province = if_else(country %in% countries_with_states,
                            "Not Applicable",
                            province))
#checking...
filtered <- maker_data %>%
  filter(province == "Not Applicable") %>%
  group_by(country) %>%
  count(country)

#checking....
filtered <- maker_data %>%
  filter(country %in% countries_with_states & is.na(province))%>%
  group_by(country) %>%
  count(country)


#check states on countries that have provinces
filtered <-maker_data %>%
  filter(!is.na(state)) %>%
  group_by(country) %>%
  count(country)
#it looks like both Canada and United Kingdom are mismatched to state
#explore further...
filtered <-maker_data %>%
  filter(!is.na(state) & country != "United States of America")


#based on city and state the United Kingdom seems to be a mis-mark
maker_data <- maker_data %>%
  mutate(
    province = if_else(
      country == "United Kingdom" & !is.na(state),
      "Not Applicable",
      province
    ),
    country = if_else(
      country == "United Kingdom" & !is.na(state),
      "United States of America",
      country
    ))

#set state correctly for countries that have provinces
maker_data <- maker_data %>%
  mutate(state = if_else(!(country %in% countries_with_states),
                                 "Not Applicable",
                                 state))
#checking...
filtered <- maker_data %>%
  filter(state == "Not Applicable") %>%
  group_by(country) %>%
  count(country)

#checking...
filtered <- maker_data %>%
  filter(!(country %in% countries_with_states) & is.na(state))%>%
  group_by(country) %>%
  count(country)

#checking....
filtered <- maker_data %>%
  filter(country %in% countries_with_states & is.na(province))%>%
  group_by(country) %>%
  count(country)

gg_miss_var(maker_data)

###clean the state data to match rnaturalearth states---------------
us <- ne_states(country = "United States of America")

unique(maker_data$state)[is.na(match(unique(maker_data$state), us$name))]

maker_data <- maker_data %>%
  mutate(state = case_when(
    state == "Washington D.C." ~ "District of Columbia (DC)",
    state == "DC" ~ "District of Columbia (DC)",
    TRUE ~ state
  ))

maker_data <- maker_data %>%
  separate(state, sep="\\(", c("state", NA)) %>%
  mutate(state = str_trim(state))

unique(maker_data$state)[is.na(match(unique(maker_data$state), us$name))]

#check some odd records...
filtered <- maker_data %>%
  filter(state == "NE")

maker_data <- maker_data %>%
  mutate(state = case_when(
    state == "PA" ~ "Pennsylvania",          
    state == "NW" ~ "Ohio", #NW was a direction not a state abbreviation           
    state == "VA" ~ "Virginia",            
    state == "IN" ~ "Indiana",           
    state == "OH" ~ "Ohio",            
    state == "NY" ~ "New York",            
    state == "WI" ~ "Wisconsin",            
    state == "WV" ~ "West Virginia",            
    state == "NJ" ~ "New Jersey",            
    state == "NE" ~ "Indiana", #NE was a direction not a state abbreviation
    TRUE ~ state
  ))

#checking
unique(maker_data$state)[is.na(match(unique(maker_data$state), us$name))]

###clean the province data to match rnaturalearth provinces--------
canada <- ne_states(country = "Canada")

unique(maker_data$province)[is.na(match(unique(maker_data$province), canada$name))]

#checking some odd records...
filtered <- maker_data %>%
  filter(province == "Nova Scotia?")

maker_data <- maker_data %>%
  mutate(province = case_when(
    province == "AB" ~ "Alberta",
    province == "MB" ~ "Manitoba",
    province == "Quebec" ~ "Québec",
    province == "Noyan" ~ "Québec",           
    province == "Onatario" ~ "Ontario",
    province == "Hampshire/Alberta" ~ "Alberta",
    province == "SK" ~ "Saskatchewan",
    province == "ALBERTA" ~ "Alberta",
    province == "Saskachewan" ~ "Saskatchewan",
    province == "Nova Scotia?" ~ NA,    
    TRUE ~ province ))

#checking...
unique(maker_data$province)[is.na(match(unique(maker_data$province), canada$name))]

gg_miss_var(maker_data, show_pct = TRUE)

##create immigrant variable -----------------------------------
maker_data <- maker_data %>% 
  mutate(immigrant = birth_place_country != country)

table(maker_data$immigrant)
##work on birth_date-----------------------------------------------------

#remove any record that doesn't have a birth_date
maker_data <- maker_data %>%
  filter(!is.na(birth_date))

#fix dates that are vague. The parser will set these to a specific date that 
#could be up to 99 years off, so this is better.
maker_data <- maker_data %>%
  mutate(birth_date = case_when(
    grepl("early 1800", str_to_lower(birth_date), fixed=TRUE) ~ "1816",
    grepl("mid 1800", str_to_lower(birth_date), fixed=TRUE) ~ "1850",
    grepl("late 1800", str_to_lower(birth_date), fixed=TRUE) ~ "1883",
    grepl("early 1900", str_to_lower(birth_date), fixed=TRUE) ~ "1916",
    grepl("mid 1900", str_to_lower(birth_date), fixed=TRUE) ~ "1950",
    grepl("late 1900", str_to_lower(birth_date), fixed=TRUE) ~ "1983",
    TRUE ~ birth_date
  ))


GetEstimatedYear <- function(date_string) {
  #parse_date_time is really robust and I want it to handle as much as possible
  estimatedYear <- year(parse_date_time2(
    date_string,
    orders = c("Y", "mY", "my", "mdY", "mdy", "dmY", "dmy", "Ymd"),
    cutoff_2000 = 0L 
  ))
  #but when it fails I want a chance to yank the year out with regexp
  estimatedYear <-
    ifelse(is.na(estimatedYear),
           as.numeric(str_extract(date_string, "\\d{4}")), estimatedYear)
  
  #and then remove dates that don't make sense
  ifelse(estimatedYear < 1650 | estimatedYear > 2024, NA, estimatedYear)
}

test_data <- tibble(
  quilt_id = c(1:22),
  birth_date = c(
    "1973",
    "Winter, 1973",
    "c. 1973",
    "January, 1973",
    "1970's",
    "01-1973",
    "01/10/1973",
    "01/10/81973",
    "10/04/73",
    "1973-01-10",
    "abt. 1973",
    "1973 or 1974",
    "06-00-1973",
    "She is quite old",
    "1970 - 1980s",
    "00-00-2007",
    "Oct-00",
    "11-26-64",
    "01-1937",
    "February, 1865",
    "12-20-31",
    "10/04/38"
  )
)

#testing...
test_data <- test_data %>%
  mutate(birth_date_year = map_int(birth_date, GetEstimatedYear))

#real data...
maker_data <- maker_data %>%
  mutate(birth_date_year = map_int(birth_date, GetEstimatedYear))

#let's see what failed...
filtered <- maker_data %>%
  filter(is.na(birth_date_year)) %>%
  select(quilt_id, birth_date, birth_date_year)

###remove records where birth dates are missing -------------------
#none of the rest of the birth_dates are parseable, so remove them
maker_data <- maker_data %>%
  filter(!is.na(birth_date_year))

###add a birth period variable for use in analysis -----------
maker_data <- maker_data %>%
  mutate(birth_time_period = case_when(
    birth_date_year >=1650 & birth_date_year <= 1699 ~ "1650-1699",
    birth_date_year >=1700 & birth_date_year <= 1749 ~ "1700-1749",
    birth_date_year >=1750 & birth_date_year <= 1799 ~ "1750-1799",
    birth_date_year >=1800 & birth_date_year <= 1849 ~ "1800-1849",
    birth_date_year >=1850 & birth_date_year <= 1899 ~ "1850-1899",
    birth_date_year >=1900 & birth_date_year <= 1949 ~ "1900-1949",
    birth_date_year >=1950 & birth_date_year <= 1999 ~ "1950-1999",
    birth_date_year >=2000 & birth_date_year <= 2049 ~ "2000-2049",
    TRUE ~ NA
  ))

gg_miss_var(maker_data, show_pct = TRUE)

##work on quilt dates --------------------------------------------------
filtered <- maker_data %>%
  select (quilt_id, quilt_begun_date, quilt_family_date, quilt_finish_date)

#quilt_finish_date is least missing date, followed by quilt_family_date,
#and then quilt_begun_date. So...for quilt date use those fields in 
#that priority.
maker_data <- maker_data %>%
  mutate(quilt_date = case_when(
    !is.na(quilt_finish_date) ~ quilt_finish_date,
    !is.na(quilt_family_date) ~ quilt_family_date,
    TRUE                      ~ quilt_begun_date))

#check results...
filtered <- maker_data %>%
  filter(is.na(quilt_finish_date)) %>%
  select (quilt_id, quilt_finish_date, 
          quilt_family_date, quilt_begun_date,
          quilt_date)


#exploring...
filtered <- maker_data %>%
  filter(grepl("late", str_to_lower(quilt_date))) %>%
  select(quilt_id, quilt_date)

#fix dates that are vague. The parser will set these to a specific date that 
#could be up to 99 years off, so this is better.
maker_data <- maker_data %>%
  mutate(quilt_date = case_when(
    grepl("early 1800", str_to_lower(quilt_date), fixed=TRUE) ~ "1816",
    grepl("mid 1800", str_to_lower(quilt_date), fixed=TRUE) ~ "1850",
    grepl("late 1800", str_to_lower(quilt_date), fixed=TRUE) ~ "1883",
    grepl("early 1900", str_to_lower(quilt_date), fixed=TRUE) ~ "1916",
    grepl("mid 1900", str_to_lower(quilt_date), fixed=TRUE) ~ "1950",
    grepl("late 1900", str_to_lower(quilt_date), fixed=TRUE) ~ "1983",
    TRUE ~ quilt_date
  ))

#and then get the year
maker_data <- maker_data %>%
  mutate(quilt_date_year = map_int(quilt_date, GetEstimatedYear))


#let's see what failed...
filtered <- maker_data %>%
  filter(is.na(quilt_date_year)) %>%
  select(quilt_date, quilt_date_year, quilt_begun_date, quilt_family_date, 
         quilt_finish_date)

gg_miss_var(maker_data, show_pct = TRUE)

##work on quilt_time_period ------------------------------------------
table(maker_data$quilt_time_period)

maker_data <- maker_data %>%
  separate(
    quilt_time_period,
    sep = "-",
    c("quilt_time_period_start", "quilt_time_period_end"),
    remove = FALSE
  )

#check to see if time period is matching with quilt_date
filtered <- maker_data %>%
  filter(quilt_date_year < quilt_time_period_start |
           quilt_date_year > quilt_time_period_end ) %>%
  select(quilt_time_period_start, quilt_date_year, 
         quilt_time_period_end,
         quilt_begun_date, quilt_family_date, quilt_finish_date)
#it's not.

#force time period to match date when available
maker_data <- maker_data %>%
  mutate(cleaned_quilt_time_period = case_when(
    is.na(quilt_date_year) & quilt_time_period == "Pre-1799" ~ "1700-1799",
    is.na(quilt_date_year) ~ quilt_time_period,
    quilt_date_year >= 1750 & quilt_date_year <=1799 ~ "1700-1799",
    quilt_date_year >= 1800 & quilt_date_year <=1849 ~ "1800-1849",
    quilt_date_year >= 1850 & quilt_date_year <=1875 ~ "1850-1875",
    quilt_date_year >= 1876 & quilt_date_year <=1900 ~ "1876-1900",
    quilt_date_year >= 1901 & quilt_date_year <=1929 ~ "1901-1929",
    quilt_date_year >= 1930 & quilt_date_year <=1949 ~ "1930-1949",
    quilt_date_year >= 1950 & quilt_date_year <=1975 ~ "1950-1975",
    quilt_date_year >= 1976 & quilt_date_year <=1999 ~ "1976-1999",
    quilt_date_year >= 2000 & quilt_date_year <=2025 ~ "2000-2025",
    quilt_date_year >= 2026 & quilt_date_year <=2050 ~ "2026-2050",
    TRUE ~ NA
  )) %>%
  select(-quilt_time_period_start, 
         -quilt_time_period_end)

#and remake these columns based on updated time period info
maker_data <- maker_data %>%
  separate(
    cleaned_quilt_time_period,
    sep = "-",
    c("quilt_time_period_start", "quilt_time_period_end"),
    remove = FALSE
  )

#checking...
table(maker_data$cleaned_quilt_time_period)
table(maker_data$quilt_time_period)

filtered <- maker_data %>%
  filter(quilt_time_period != cleaned_quilt_time_period &
          is.na(quilt_date_year))


###remove records where time period is missing ------------------
#quilt date work is done and time period work is done. If time period
#is still missing the record needs to go
maker_data <- maker_data %>%
  filter(!is.na(cleaned_quilt_time_period) & cleaned_quilt_time_period != "Timespan")

gg_miss_var(maker_data, show_pct = TRUE)

##work on missing gender --------------------------------------------------------
#the analysis requires gender, so remove records where it is missing...
maker_data <- maker_data %>%
  filter(!is.na(gender))

#explore...
filtered <- maker_data %>%
  filter(team_from_gender)

#in the case where gender was marked "Group" in the original data,
#there is no way to tell if the gender was to be applied to the group
#as a whole or to one particular person in the group. So these should
#be considered as not having gender information and thus, removed.
maker_data <- maker_data %>%
  filter(!team_from_gender)

table(maker_data$gender)

gg_miss_var(maker_data, show_pct = TRUE)

##work cleaning how_learned_to_quilt ------------------------
table(maker_data$how_learned_to_quilt)

maker_data <- maker_data %>%
  mutate(how_learned_to_quilt = case_when(
    how_learned_to_quilt == "Self-Taught" ~ "Self-taught",
    how_learned_to_quilt == "From relative" ~ "From Relative",
    how_learned_to_quilt == "From guild or club member" ~ "From Guild or Club Member",
    TRUE ~ how_learned_to_quilt
  ))

##create life-stage of learning variable -------------------
table(maker_data$when_learned_to_quilt)

maker_data <- maker_data %>%
  mutate(learning_life_stage = case_when(
    when_learned_to_quilt == "After an illness" ~ "Unknown", 
    when_learned_to_quilt == "After raising children" ~ "Middle Adult",
    when_learned_to_quilt == "After retiring" ~ "Middle Adult",              
    when_learned_to_quilt == "Age 11-19" ~ "Childhood",
    when_learned_to_quilt == "Age 20-29" ~ "Young Adult",              
    when_learned_to_quilt == "Age 30-39" ~ "Young Adult",              
    when_learned_to_quilt == "Age 40-49" ~ "Middle Adult",         
    when_learned_to_quilt == "Age 50 or over" ~ "Elder Adult", 
    when_learned_to_quilt == "Under 10 years of age" ~ "Childhood",
    TRUE ~ "Unknown"
  ))
    
##create age at production and life-stage at production variables---------
maker_data <- maker_data %>%
  mutate(age_at_production = quilt_date_year - birth_date_year,
         age_at_production = case_when(
           age_at_production < 0 ~ NA,
           age_at_production > 110 ~ NA,
           TRUE ~ age_at_production
         )) 

maker_data <- maker_data %>%
  mutate(production_life_stage = case_when(
    age_at_production < 18 ~ "Childhood",
    age_at_production >= 18 & age_at_production < 45  ~ "Young Adult", 
    age_at_production >= 45 & age_at_production < 65  ~ "Middle Adult",
    age_at_production >= 65 ~ "Elder Adult"
  ))

##work on quilt_why------------------------
table(maker_data$quilt_why, useNA="always")

maker_data <- maker_data %>%
  mutate(quilt_why = case_when(
    quilt_why == "Not described" ~ NA,
    quilt_why == "Challenge or Contest entry" ~ "Challenge or contest entry",
    quilt_why == "Commemorative" ~ "Celebrate an event",
    quilt_why == "Wedding" ~ "Celebrate an event",
    quilt_why == "Anniversary" ~ "Celebrate an event",
    quilt_why == "Reunion" ~ "Celebrate an event",
    quilt_why == "Mourning" ~ "Memorial",
    TRUE ~ quilt_why
  ))

##work on childhood_environment---------------------------------------------
table(maker_data$childhood_environment)

maker_data <- maker_data %>%
  mutate(childhood_environment = case_when(
    childhood_environment == "rural" ~ "Rural",
    childhood_environment == "RURAL" ~ "Rural",
    childhood_environment == "urban" ~ "Urban",
    childhood_environment == "URBAN" ~ "Urban",
    TRUE ~ childhood_environment
  ))

#remove unneeded items ---------------------------------------------------------
colnames(maker_data)

#some items that are no longer needed
maker_data <- maker_data %>%
  select(-c(birth_place_state,
            birth_place_province,
            birth_place_city,
            city,
            zip_code,
            quilt_time_period,
            birth_date,
            quilt_date,
            quilt_finish_date,
            quilt_family_date,
            quilt_begun_date,
            zip_code,
            quilt_province,
            quilt_state,
            quilt_city))
  
gg_miss_var(maker_data, show_pct = TRUE)

#fill NAs ------------------------------------------------------------
maker_data <- maker_data %>%
  replace_na(list(
    maker_group_name="None", 
    other_makers="None"))

gg_miss_var(maker_data, show_pct = TRUE)

#make an intermediate save ----------------------------------------------
write.csv(maker_data, "03_intermediate.csv")

#begin cleaning and deduping quiltmaker name data-------------------------------
maker_data <- read.csv("03_intermediate.csv")
read_problems <- problems(maker_data)

#replace NA quilt top maker name with unnamed+quilt_id+birth_date_year
filtered <- maker_data %>%
  filter(is.na(quilt_top_maker_name))

#testing...
filtered <- filtered %>%
  mutate(quilt_top_maker_unique_id =
           paste(quilt_id, birth_date_year, sep="-")) 

#real data...
maker_data <- maker_data %>%
  mutate(quilt_top_maker_unique_id =
           paste(quilt_id, birth_date_year, sep="-")) 

#testing...
filtered <- filtered %>%
  mutate(quilt_top_maker_name=ifelse(is.na(quilt_top_maker_name), 
                                     paste("unnamed", quilt_top_maker_unique_id), 
                                     quilt_top_maker_name))
           
#go ahead...
maker_data <- maker_data %>%
  mutate(quilt_top_maker_name=ifelse(is.na(quilt_top_maker_name), 
                                     paste("unnamed", quilt_top_maker_unique_id), 
                                     quilt_top_maker_name))


#checking - should be zero
filtered <- maker_data %>%
  filter(is.na(quilt_top_maker_name))

#checking
filtered <- maker_data %>%
  filter(grepl("unnamed", quilt_top_maker_name, fixed=TRUE) ) %>%
  arrange(quilt_top_maker_name) %>%
  select(quilt_id, quilt_top_maker_name, birth_date_year)

#here I stop and go make test data and do a bunch of testing on that data.
#that work has been moved to another file, 3.1_test_name_deduplication.R.
#and once the testing has been done, I move forward on the real data


## work on last names ----------------------------------------------------------

#reverse the name format so all names are formatted the same. 
maker_data <- maker_data %>%
  mutate(last_name = last_name(format_reverse(quilt_top_maker_name)))

#humaniformat needs a lot of help here - if humaniformat can't get the last name, use the
#second word and if that doesn't work just use the full name.
maker_data <- maker_data %>%
  mutate(last_name = ifelse(is.na(last_name),
                            word(format_reverse(quilt_top_maker_name), 2),
                            last_name))

maker_data <- maker_data %>%
  mutate(last_name = ifelse(is.na(last_name) | last_name == "",
                            quilt_top_maker_name,
                            last_name))

#remove some extra punctuation
maker_data <- maker_data %>%
  mutate(last_name = str_remove(last_name, fixed("(")))

#and add a lower case version for easier comparisons
maker_data <- maker_data %>%
  mutate(last_name_lower = tolower(last_name))

#checking...this should be zero
filtered <- maker_data %>%
  filter(is.na(last_name))
filtered <- maker_data %>%
  filter(last_name == "")

##work on first name ------------------------------------------------------------

#humaniformat is not great at parsing out the first name from the free
#form field so I am going to help it a bit more
#Get everything that isn't the last name and adjust it
maker_data <- maker_data %>%
  mutate(first_name =
           str_remove(quilt_top_maker_name, fixed(last_name)) %>%
           str_remove(",") %>%
           str_squish())

#remove punctuation from the first name
maker_data <- maker_data %>%
  mutate(first_name= gsub('[[:punct:] ]+',' ', first_name)%>%
           str_squish())

#get rid of empty strings
maker_data <- maker_data %>%
  mutate(first_name =
           ifelse(
             first_name != "",
             first_name,
             "unidentified"
           ))

#let humaniformat get the first name now that I've cleaned it up
maker_data <- maker_data %>%
  mutate(first_name = ifelse(is.na(first_name(first_name)),
                             first_name,
                             first_name(first_name)))

#and create a lower case version for easier comparisons
maker_data <- maker_data %>%
  mutate(first_name_lower = tolower(first_name))


gg_miss_var(maker_data, show_pct = TRUE)

##----nicknames----------
#nicknames file comes directly from:
#https://github.com/carltonnorthern/nicknames/blob/master/names.csv
#nicknames processing inspired by:
#https://github.com/carltonnorthern/nicknames/blob/master/R-parser.R
names_df <-
  read.csv('nicknames.csv',
           col.names = letters[1:16],
           fill = TRUE,
           header = FALSE)

names(names_df)[names(names_df) == "a"] <- "long_name"
names(names_df)[2:length(names(names_df))] <-
  paste0("diminutive_", seq(1, length(names_df) - 1, 1))
#now back to my own code

##work on nicknames -------------------------------------------------------------

#find nicknames associated with the first name
#join with nicknames file, get a diminutive_ column for each nickname
maker_data <- maker_data %>%
  left_join(names_df, by = c("first_name_lower" = "long_name")) 

#combine all the diminutive_ columns into one string. This doesn't
#seem like what I would normally want to do with data like this, BUT
#the diyar sub-criteria are tricky and this is what works.
maker_data <- unite(maker_data, "nicknames", 
                         starts_with("diminutive_"), 
                         sep = " ", 
                         remove = TRUE, 
                         na.rm = TRUE)


##create criteria and custom criteria functions for de-duplication name matching-
LastNamesMatch <- function(x,y){
  is_unnamed_quilter <- grepl("[0-9]", y[1])
  names_almost_match <- stringdist(x, y[1], method = "lcs") < 2
  match <- !is_unnamed_quilter & names_almost_match
  #print(data.frame(x, y, match))
  #cat("\n")
  return(match)
}

FirstNamesMatch <- function(x,y){
  search_name <- y$a1[1]
  
  comparison_names_list <- x$a1
  comparison_nicknames_list <- x$a2
  
  grep_term <- paste0('\\b',search_name,'\\b')
  
  match <- stringdist(comparison_names_list, search_name, method = "lcs") < 2 | 
    grepl(grep_term, comparison_nicknames_list)
  return(match)
}


first_names_sub_criteria <- sub_criteria(
  attrs(.obj = list(a1 = maker_data$first_name_lower, a2 = maker_data$nicknames)),
  match_funcs = FirstNamesMatch,
  equal_funcs = FirstNamesMatch 
  #not sure about equal_funcs, but it won't run without it 
  #and it doesn't cause problems, and I can't find good documentation, so...
)

last_names_sub_criteria <- sub_criteria(
  maker_data$last_name_lower,
  match_funcs = LastNamesMatch
)

##create data and criteria for de-duplication birth date matching---------------
birth_date_sub_criteria <- sub_criteria(
  maker_data$birth_date_year,
  match_funcs = exact_match
)

## one sub_criteria to rule them all -------------------------------------------

people_match_sub_criteria <- sub_criteria(
  last_names_sub_criteria, 
  first_names_sub_criteria, 
  birth_date_sub_criteria,
  operator = "and")

## do the record linkage -------------------------------------------------------
maker_data$maker_id <- links(
  criteria = "place_holder", 
  sub_criteria = list(cr1 = people_match_sub_criteria),
  batched = "semi",
  recursive = TRUE
)

##create quiltmaker id -----------------
#maker_id is correctly determining linked records, but it isn't easy to work with
#here I make a nice quiltmaker_id that is easier to work with
maker_data <- maker_data %>%
  group_by(maker_id) %>%
  mutate(quiltmaker_id = cur_group_id()) %>%
  ungroup() %>%
  select(-maker_id)

#checking...
filtered <- maker_data %>%
  distinct(quilt_id, quiltmaker_id, quilt_top_maker_name) %>%
  group_by(quiltmaker_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(quilt_id, quilt_top_maker_name, quiltmaker_id, n) %>%
  distinct(quiltmaker_id, quilt_top_maker_name, n) %>%
  arrange(desc(n), desc(quiltmaker_id))


#all looks good

gg_miss_var(maker_data, show_pct = TRUE)

#save dedupe work ----------------------------------------------------------------------
#Save all that hard work.
write.csv(maker_data, "03_cleaned_maker_data.csv")



#create travel data ---------------------------------------------------------
travel_data <- read.csv("03_cleaned_maker_data.csv")
read_problems <- problems(travel_data)

travel_data <- travel_data %>%
  filter(!(birth_place_country == quilt_country & quilt_country == country)) %>%
  distinct(quiltmaker_id, 
           birth_place_country,
           quilt_country,
           country) 

travel_data <- travel_data %>%
  group_by(quiltmaker_id) %>% 
  mutate(index = row_number(),
         path_id = paste(quiltmaker_id, index, sep="-")) %>%
  ungroup() %>%
  select(-c(quiltmaker_id, index))


travel_data <- travel_data %>%
  pivot_longer(cols=c("birth_place_country", "quilt_country", "country"),
               names_to="location",
               values_to="country") 

write.csv(travel_data, "03_travel_data.csv")



#create quiltmaker report ---------------------------------------------------------
report_data <- read.csv("03_cleaned_maker_data.csv")
read_problems <- problems(report_data)


report_data <- report_data %>%
  select(quilt_id, quiltmaker_id, quilt_top_maker_name) %>%
  distinct(quilt_id, quiltmaker_id, quilt_top_maker_name)

report_data <- report_data %>%
  group_by(quiltmaker_id) %>%
  mutate(number_of_quilts = n(),
         index = row_number())

report_data <- report_data %>%
  pivot_wider(names_from=index, names_prefix="quilt_id_", values_from=quilt_id)

report_data <- report_data %>%
  unite(col="quilt_ids", starts_with("quilt_id_"), sep=" ", remove=TRUE, 
        na.rm=TRUE)
  
report_data <- report_data %>%
  group_by(quiltmaker_id) %>%
  mutate(index = row_number()) %>%
  ungroup()

name_data <- report_data %>%
  pivot_wider(id_cols=c(quiltmaker_id,number_of_quilts), 
              names_from=index, names_prefix="name_", 
              values_from=quilt_top_maker_name)

name_data <- name_data %>%
  unite(col="quilt_top_maker_names", starts_with("name_"), sep=" | ", 
        remove=TRUE, na.rm=TRUE)

quilt_id_data <- report_data%>%
  pivot_wider(id_cols=c(quiltmaker_id), names_from=index, 
              names_prefix="quilt_ids_", 
              values_from=quilt_ids,
              values_fill="")

report_data <- 
  left_join(name_data,
            quilt_id_data,
            join_by(quiltmaker_id == quiltmaker_id))

report_data <- report_data %>%
  select(-quiltmaker_id) %>%
  arrange(desc(number_of_quilts))

report_data <- report_data %>%
  filter(number_of_quilts > 1)

write.csv(report_data, "03_top_quilt_makers.csv")