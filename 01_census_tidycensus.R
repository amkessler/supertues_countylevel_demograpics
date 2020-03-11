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


#Getting data for every state ####

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
allcounties_wide <- map_df(targetstates, function(x) {
  get_acs(geography = "county", variables = c(currentchoice), 
          state = x, output = "wide")
})

#remove MOE columns - they all end with "M"
allcounties_wide <- allcounties_wide %>% 
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
#percentage calculations 
#-- tricky, since demo groups differ in columns
#-- this might have to be done individually for each demographic grouping


allcounties_wide$pct.born.foreign <- round_half_up(allcounties_wide$natborn.foreign/allcounties_wide$natborn.total*100, 3)
allcounties_wide$pct.mil.veteran <- round_half_up(allcounties_wide$military.veteran/allcounties_wide$military.total*100, 3)
allcounties_wide$pct.race.white <- round_half_up(allcounties_wide$originrace.whitealone/allcounties_wide$originrace.total.all*100, 3)
allcounties_wide$pct.race.nonwhite <- 100-allcounties_wide$pct.race.white 
allcounties_wide$pct.race.white <- NULL

#this has been fixed to account for grad student in the B census table
allcounties_wide$pct.ed.college.all <- round_half_up((allcounties_wide$education.bachelors+allcounties_wide$education.gradprofess)/allcounties_wide$education.total*100, 3)

allcounties_wide$pct.ed.college.white <- round_half_up((allcounties_wide$white.ed.male.bachelors+allcounties_wide$white.ed.female.bachelors)/allcounties_wide$white.ed.totalall*100, 3)

allcounties_wide$pct.ed.college.white.male <- round_half_up(allcounties_wide$white.ed.male.bachelors/allcounties_wide$white.ed.male.total*100, 3)

allcounties_wide$pct.ed.college.white.female <- round_half_up(allcounties_wide$white.ed.female.bachelors/allcounties_wide$white.ed.female.total*100, 3)

#remove unneeded columns
allcounties_wide <- allcounties_wide %>% 
  select(-natborn.total,
         -natborn.foreign,           
         -education.total,
         -education.bachelors, 
         -education.gradprofess,
         -military.total,
         -military.veteran,
         -originrace.total.all,
         -originrace.whitealone,
         -white.ed.totalall,
         -white.ed.male.total,
         -white.ed.male.bachelors,
         -white.ed.female.total,
         -white.ed.female.bachelors
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

national$pct.born.foreign <- round_half_up(national$natborn.foreign/national$natborn.total*100, 3)

national$pct.mil.veteran <- round_half_up(national$military.veteran/national$military.total*100, 3)

national$pct.race.white <- round_half_up(national$originrace.whitealone/national$originrace.total.all*100, 3)
national$pct.race.nonwhite <- 100-national$pct.race.white 
national$pct.race.white <- NULL

national$pct.ed.college.all <- round_half_up((national$education.bachelors+national$education.gradprofess)/national$education.total*100, 3)

national$pct.ed.college.white <- round_half_up((national$white.ed.male.bachelors+national$white.ed.female.bachelors)/national$white.ed.totalall*100, 3)
national$pct.ed.college.white.male <- round_half_up(national$white.ed.male.bachelors/national$white.ed.male.total*100, 3)
national$pct.ed.college.white.female <- round_half_up(national$white.ed.female.bachelors/national$white.ed.female.total*100, 3)


#remove unneeded columns
national <- national %>% 
  select(-natborn.total,
         -natborn.foreign,           
         -education.total,
         -education.bachelors,    
         -education.gradprofess,
         -military.total,
         -military.veteran,
         -originrace.total.all,
         -originrace.whitealone,
         -white.ed.totalall,
         -white.ed.male.total,
         -white.ed.male.bachelors,
         -white.ed.female.total,
         -white.ed.female.bachelors
  )

colnames(national)

colnames(national) <- c("GEOID",
                      "natl.name",
                      "natl.totalpop",
                      "natl.medincome",
                      "natl.medage",
                      "natl.pct.born.foreign",
                      "natl.pct.mil.veteran",
                      "natl.pct.race.nonwhite",
                      "natl.pct.ed.college.all",         
                      "natl.pct.ed.college.white",
                      "natl.pct.ed.college.white.male",
                      "natl.pct.ed.college.white.female"
                      )


#### ADD NATIONAL NUMBERS AS COLUMNS TO DISTRICT DATA #####

# single vector value will repeat itself in all rows
allcounties_wide$natl.medincome <- national$natl.medincome
allcounties_wide$natl.medage <- national$natl.medage
allcounties_wide$natl.pct.born.foreign <- national$natl.pct.born.foreign
allcounties_wide$natl.pct.mil.veteran <- national$natl.pct.mil.veteran
allcounties_wide$natl.pct.race.nonwhite <- national$natl.pct.race.nonwhite
allcounties_wide$natl.pct.ed.college.all <- national$natl.pct.ed.college.all
allcounties_wide$natl.pct.ed.college.white <- national$natl.pct.ed.college.white
allcounties_wide$natl.pct.ed.college.white.male <- national$natl.pct.ed.college.white.male
allcounties_wide$natl.pct.ed.college.white.female <- national$natl.pct.ed.college.white.female


# calculate whether district figure is above of below national figure ####
allcounties_wide$medincome.abovebelow.natl <- if_else((allcounties_wide$medincome < allcounties_wide$natl.medincome), 'BELOW', 'ABOVE')
allcounties_wide$medage.abovebelow.natl <- if_else((allcounties_wide$medage < allcounties_wide$natl.medage), 'BELOW', 'ABOVE')
allcounties_wide$pct.born.foreign.abovebelow.natl <- if_else((allcounties_wide$pct.born.foreign < allcounties_wide$natl.pct.born.foreign), 'BELOW', 'ABOVE')
allcounties_wide$pct.mil.veteran.abovebelow.natl <- if_else((allcounties_wide$pct.mil.veteran < allcounties_wide$natl.pct.mil.veteran), 'BELOW', 'ABOVE')
allcounties_wide$pct.race.nonwhite.abovebelow.natl <- if_else((allcounties_wide$pct.race.nonwhite < allcounties_wide$natl.pct.race.nonwhite), 'BELOW', 'ABOVE')
allcounties_wide$pct.ed.college.all.abovebelow.natl <- if_else((allcounties_wide$pct.ed.college.all < allcounties_wide$natl.pct.ed.college.all), 'BELOW', 'ABOVE')
allcounties_wide$pct.ed.college.white.abovebelow.natl <- if_else((allcounties_wide$pct.ed.college.white < allcounties_wide$natl.pct.ed.college.white), 'BELOW', 'ABOVE')
allcounties_wide$pct.ed.college.white.male.abovebelow.natl <- if_else((allcounties_wide$pct.ed.college.white.male < allcounties_wide$natl.pct.ed.college.white.male), 'BELOW', 'ABOVE')
allcounties_wide$pct.ed.college.white.female.abovebelow.natl <- if_else((allcounties_wide$pct.ed.college.white.female < allcounties_wide$natl.pct.ed.college.white.female), 'BELOW', 'ABOVE')


# clean up column names of finished table
allcounties_wide <- allcounties_wide %>% 
  clean_names() 


# add state abbreviation column
head(fips_codes) #built into tidycensus

state_codes <- fips_codes %>% 
  select(state_code = state,
         state_name) %>% 
  filter(state_code %in% us) %>% 
  unique()

allcounties_wide <- inner_join(allcounties_wide, state_codes) %>% 
  select(geoid, state_name, state_code, everything())


# add population density from downloaded 2010 Census file ####
popdensity2010 <- read_csv("raw_data/DEC_10_SF1_GCTPH1.ST05.csv", 
                                   skip = 1)

popdensity2010 <- popdensity2010 %>% 
  clean_names() %>% 
  rename(geoid = target_geo_id2) 

popdensity2010 <- popdensity2010 %>% 
  select(geoid, 
         area_in_square_miles_land_area)

test <- left_join(allcounties_wide, popdensity2010)

allcounties_wide <- test

allcounties_wide <- allcounties_wide %>% 
  mutate(
    population_density_persqmile = round_half_up(totalpop / area_in_square_miles_land_area, 1)
  ) 


# save result ####
saveRDS(allcounties_wide, "processed_data/allcounties_wide.rds")
write_xlsx(allcounties_wide, "processed_data/allcounties_wide.xlsx")

