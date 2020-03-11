library(tidyverse)
library(tigris)
library(tidycensus)
library(janitor)
library(writexl)
library(reshape2)

# lead table created in step 01 
allcounties_wide <- readRDS("processed_data/allcounties_wide.rds")

glimpse(allcounties_wide)

#bring in prez general election results from MIT election lab
countypres_2000_2016 <- read_csv("raw_data/countypres_2000-2016.csv", 
                                 col_types = cols(FIPS = col_character(), 
                                                  year = col_character(),
                                                  version = col_character()))

countypres_2000_2016 <- countypres_2000_2016 %>% 
  clean_names() 

glimpse(countypres_2000_2016)

#appears the MIT data has some FIPS codes that lost a leading zero
#we'll fix that here
countypres_2000_2016 <- countypres_2000_2016 %>% 
  mutate(
    geoid = case_when(
      str_length(fips) == 5 ~ fips,
      str_length(fips) == 4 ~ str_c("0", fips)
    )
  )

#examine whether any of the county candidate votes don't add up to total?
a <- countypres_2000_2016 %>% 
  filter(year == "2016") 

a <- a %>% 
  group_by(fips) %>% 
  summarise(sumvotes = sum(candidatevotes),
            totvotes = sum(totalvotes)/3)

a %>% 
  mutate(
    samevalue = sumvotes==totvotes
  ) %>% 
  filter(samevalue == FALSE) # appears to be 4 instances to investigate further for explanation

countypres_2000_2016 %>% 
  filter(fips %in% c("31103", "4007", "4009", "4011"),
         year == "2016")



#calculate pct of total votes for each record
countypres_2000_2016 <- countypres_2000_2016 %>% 
  mutate(
    candidate_pct = (candidatevotes/totalvotes)*100
  ) %>% 
  filter(party %in% c("democrat", "republican"))

#only 2016 
countypres_2016 <- countypres_2000_2016 %>% 
  filter(year == "2016")

head(countypres_2016)

#reshape data to put R and D results on same row for each county
ztemp <- countypres_2016 %>% 
  select(geoid, party, candidate_pct) %>% 
  dcast(geoid ~ party, value.var = "candidate_pct", sum) %>% 
  as_tibble()


#check to make sure nothing more than 100
ztemp %>% 
  mutate(
    totalpct = democrat + republican
  ) %>% 
  filter(totalpct > 100)

#what's up with 31103 in Nebraska?
countypres_2016 %>% 
  filter(geoid == "31103")


ztemp16 <- ztemp %>% 
  mutate(
    winner16 = if_else(democrat>republican, "D", "R"),
    margin16 = abs(democrat - republican)
  ) %>% 
  rename(
    dem16 = democrat,
    gop16 = republican,
  )


# join results table to main table ####
z <- left_join(allcounties_wide, ztemp16)

allcounties_wide <- z



### BRING IN SENATE AND GOVERNOR RESULTS FOR 2018 ####

sen_gov_18 <- readRDS("processed_data/all_counties_sen_gov_18.rds")

#rename id file to match
sen_gov_18 <- sen_gov_18 %>% 
  rename(geoid = co_id)

#join to master table
zz <- left_join(allcounties_wide, sen_gov_18)

allcounties_wide <- zz



### BRING IN DEMOCRATIC PRIMARY RESULTS FROM 2016 ####

primary16combined <- readRDS("cnn_ct_data/STATE_PRIMARIES_2016/primary16combined.rds")



allcounties_wide %>% 
  select(co_id = geoid) %>% 
  mutate(
    co_id = str_trim(co_id),
    len = str_length(co_id)
  ) %>% 
  count(len)



# save result ####
saveRDS(allcounties_wide, "processed_data/allcounties_wide_withelex.rds")
write_xlsx(allcounties_wide, "processed_data/allcounties_wide_withelex.xlsx")
