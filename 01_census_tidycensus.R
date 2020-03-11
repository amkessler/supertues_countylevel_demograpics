library(tidyverse)
library(tigris)
library(tidycensus)
library(reshape2)
library(janitor)
library(datapasta)
library(writexl)
options(tigris_class = "sf")

# census_api_key(Sys.getenv("MYCENSUSAPIKEY"), install=TRUE)

# v18 <- load_variables(2018, "acs5", cache = TRUE)
# write_xlsx(v18, "v18.xlsx")

#### SET CENSUS VARIABLES TO BE PULLED ####

myvars <- c(totalpop = "B01003_001",
            medincome = "B19013_001",
            medage = "B01002_001",
            medhomevalue = "B25077_001",
            laborforce.civ.total = "B23025_003",
            laborforce.civ.unemployed = "B23025_005",
            education.total = "B06009_001",
            education.bachelors = "B06009_005",  
            education.gradprofess = "B06009_006"
            )


# # #temp labels to pull individual census variables
# myvars <- c(varname = "GCT2510_001")
# 
# GCT2701_HC01

           
#############################################################

# SET VARIABLE VECTOR AS CURRENT CHOICE TO USE BELOW
currentchoice <- myvars


#### COMPILING COUNTY LEVEL DATA ####


#single state ####
vt <- get_acs(geography = "county",
              variables = c(currentchoice),
              state = "VT")


head(fips_codes) #built into tidycensus
us <- as_tibble(unique(fips_codes$state)[1:51])

#getting data for SUPER TUES and SUPER TUES II states
targetstates <- us %>% 
  filter(value %in% c("AL",
                      "AK",
                      "CA",
                      "CO",
                      "ME",
                      "MA",
                      "MN",
                      "NC",
                      "OK",
                      "TN",
                      "TX",
                      "UT",
                      "VT",
                      "VA",
                      "MI",
                      "WA",
                      "MO",
                      "MI",
                      "ID",
                      "ND")
                     ) %>% 
                  pull()



#download data in wide format to allow for subsequent pct calculations
allcounties_wide_pulled <- map_df(targetstates, function(x) {
  get_acs(geography = "county", variables = c(currentchoice), 
          state = x, output = "wide")
})

#remove MOE columns - they all end with "M"
allcounties_wide <- allcounties_wide_pulled %>% 
  select(-ends_with("M"))

names(allcounties_wide)

#cleaning up and splitting NAME
split <- str_split(allcounties_wide$NAME, ",", simplify = TRUE) #true returns matrix

dist <- split[ ,1]
dist <- str_trim(dist)

st <- split[ ,2]
st <- str_trim(st)

allcounties_wide$county.name <- dist
allcounties_wide$state.name <- st

#bring new columns forward
allcounties_wide <- allcounties_wide %>% 
  select(GEOID, state.name, county.name, everything(), -NAME)

names(allcounties_wide)


#clean up names to remove trailing E
colnames(allcounties_wide) <- sub("E$", "", colnames(allcounties_wide)) # $ means end of string only

names(allcounties_wide)



### PERCENTAGE CALCULATIONS #####

#unemployement (civilians only)
allcounties_wide$pct.civ.unemployed <- round_half_up(allcounties_wide$laborforce.civ.unemployed/allcounties_wide$laborforce.civ.total*100, 3)
#college and grad students need to be added here to make up the numerator
allcounties_wide$pct.ed.college.all <- round_half_up((allcounties_wide$education.bachelors+allcounties_wide$education.gradprofess)/allcounties_wide$education.total*100, 3)


allcounties_wide <- allcounties_wide %>% 
  select(
    GEOID,
    state.name,
    county.name,
    totalpop,
    medincome,
    medage,
    medhomevalue,
    pct.civ.unemployed,
    laborforce.civ.total,
    laborforce.civ.unemployed,
    pct.ed.college.all,
    education.total,
    education.bachelors,
    education.gradprofess
  )


### NATIONAL-LEVEL FIGURES FOR COMPARISON ####

#whole country figures ####
national <- get_acs(geography = "us",
                    variables = c(currentchoice),
                    output = "wide")


#remove MOE columns - they all end with "M"
national <- national %>% 
  select(-ends_with("M"))

names(national)

#clean up names to remove trailing E
colnames(national) <- sub("E$", "", colnames(national)) # $ means end of string only


# national level percentage calculations ####

names(national)

#unemployement (civilians only)
national$pct.civ.unemployed <- round_half_up(national$laborforce.civ.unemployed/national$laborforce.civ.total*100, 3)
#college and grad students need to be added here to make up the numerator
national$pct.ed.college.all <- round_half_up((national$education.bachelors+national$education.gradprofess)/national$education.total*100, 3)


names(national)

#rename and select
national <- national %>% 
  select(
    GEOID,
    NAM,
    natl.medincome = medincome,
    natl.medage = medage,
    natl.medhomevalue = medhomevalue,
    natl.pct.civ.unemployed = pct.civ.unemployed,
    natl.pct.ed.college.all = pct.ed.college.all
  )

national


#### ADD NATIONAL NUMBERS AS COLUMNS TO DISTRICT DATA #####

# single vector value will repeat itself in all rows
allcounties_wide$natl.medincome <- national$natl.medincome
allcounties_wide$natl.medage <- national$natl.medage
allcounties_wide$natl.medhomevalue <- national$natl.medhomevalue
allcounties_wide$natl.pct.civ.unemployed <- national$natl.pct.civ.unemployed
allcounties_wide$natl.pct.ed.college.all <- national$natl.pct.ed.college.all



# calculate whether district figure is above of below national figure ####
allcounties_wide$medincome.abovebelow.natl <- if_else((allcounties_wide$medincome < allcounties_wide$natl.medincome), 'BELOW', 'ABOVE')
allcounties_wide$medage.abovebelow.natl <- if_else((allcounties_wide$medage < allcounties_wide$natl.medage), 'BELOW', 'ABOVE')
allcounties_wide$medhomevalue.abovebelow.natl <- if_else((allcounties_wide$medhomevalue < allcounties_wide$natl.medhomevalue), 'BELOW', 'ABOVE')
allcounties_wide$pct.civ.unemployed.abovebelow.natl <- if_else((allcounties_wide$pct.civ.unemployed < allcounties_wide$natl.pct.civ.unemployed), 'BELOW', 'ABOVE')
allcounties_wide$pct.ed.college.all.abovebelow.natl <- if_else((allcounties_wide$pct.ed.college.all < allcounties_wide$natl.pct.ed.college.all), 'BELOW', 'ABOVE')


# clean up column names of finished table
allcounties_wide <- allcounties_wide %>% 
  clean_names() 


# add state abbreviation column
head(fips_codes) #built into tidycensus

state_codes <- fips_codes %>% 
  select(state_code = state,
         state_name) %>% 
  filter(state_code %in% targetstates) %>% 
  unique()

allcounties_wide <- inner_join(allcounties_wide, state_codes) %>% 
  select(geoid, state_name, state_code, everything())



#### ADDING BLS UNEMPLOYMENT RATES ####

#import file downloaded from bls
# https://www.bls.gov/lau/laucnty18.xlsx

unemployment <- readxl::read_excel("processed_data/laucnty18_reformatted.xlsx")

unemployment <- unemployment %>% 
  clean_names() %>% 
  mutate(
    GEOID = paste0(state_fips, county_fips)
  )




# save result ####
saveRDS(allcounties_wide, "processed_data/allcounties_wide.rds")
write_xlsx(allcounties_wide, "processed_data/allcounties_wide.xlsx")

