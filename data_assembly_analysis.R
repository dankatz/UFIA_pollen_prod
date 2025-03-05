#This script is for quantifying pollen production in US cities included in the UFIA 
#It uses buffered plot locations instead of using actual plot locations
#The authors are Keily Peralta, and Daniel Katz (dankatz@cornell.edu)


### prepare work environment ###################################################
#install all required packages
list.of.packages <- c("tidycensus", "ggplot2", "dplyr", "sf", "units", "plyr", "stringr",
                      "scales", "here", "stringdist", "tidyr", "purrr", "readr",
                      "lme4", "lmerTest", "ggsignif")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load all required packages
library(tidycensus)
library(ggplot2)
library(dplyr)
library(sf)
library(units)
library(plyr)
library(stringr)
library(scales)
library(here)
library(stringdist)
library(tidyr)
library(purrr)
library(readr)
library(lme4)
library(lmerTest)
library(ggsignif)
#rm(list=ls())

wd <- here::here()
setwd(file.path(wd)) #getwd()

options(scipen=999)
options(tigris_use_cache = TRUE)


#set the directory for the UFIA files:
UFIA_file_path <- "C:/Users/dsk273/Documents/tree_census"

# obtain your own US Census API key, via the following URL:
census_key <- read.table("C:/Users/dsk273/Documents/UFIA_pollen_prod/US census key.txt", stringsAsFactors = F, header = F)
census_api_key(census_key[1], install = TRUE, overwrite = TRUE)



# categorizing cities by location in the country
NE_cities <- c("BaltimoreMD2022Curr", "BurlingtonVT2022Curr",  "ChicagoIL2022Curr", "ClevelandOH2022Curr", 
               "DesMoinesIA2022Curr",
               "MadisonWI2022Curr",  "MilwaukeeWI2022Curr",  "MinneapolMN2022Curr",
               "PittsburghPA2022Curr", "PortlandME2022Curr", 
               "ProvidenceRI2022Curr", "RochesterNY2022Curr", "TrentonNJ2022Curr", "WashingtonDC2022Curr" )
NE_cities_not_eval <- c("BaltimoreMD", "BurlingtonVT",  "ChicagoIL", "ClevelandOH",   "DesMoinesIA",  
                        "MadisonWI",  "MilwaukeeWI",  "MinneapolMN","PittsburghPA", "PortlandME", 
                        "ProvidenceRI", "RochesterNY", "TrentonNJ", "WashingtonDC" )



### adding the Urban FIA data from datamart #####################################
psca <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_PLOT_STRAT_CALC_ASSGN.csv"))
psc <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/POP_STRATUM_CALC.csv"))
plt <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_PLOT.csv")) #summary(plt)
ref_plot_status <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/REF_PLOT_STATUS.csv"))
mtre <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_MOTHER_TREE.csv"))
indiv_tree <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_TREE.csv"))
subp <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_SUBPLOT.csv"))
cnd <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_COND.csv"))
spcnd <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/ID_SUBP_COND.csv"))
ref_species <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/REF_SPECIES.csv"))
ref_species_group <- read.csv(file.path(UFIA_file_path, "data","FIADB_URBAN_ENTIRE_CSV/REF_SPECIES_GROUP.csv"))
plt$PLOT_STATUS_CD_LAB <- ref_plot_status$ABBR[match(plt$PLOT_STATUS_CD, ref_plot_status$VALUE)]




############ this section is copied from the poor trees project, rolling with it for now although
# it should ultimately be switched so it's for a buffer or sent to Charlie for use with the 
# original plot locations
# another issue is the scaling factor, should ask Alex about how to deal with that

######   Now get census tract data for each city ############################### 
#including additional cities that were available on 11/23/2024
evals <- c("AustinTX2022Curr", "BaltimoreMD2022Curr", "BurlingtonVT2022Curr",  "ChicagoIL2022Curr",
           "ClevelandOH2022Curr",  "DesMoinesIA2022Curr", "HoustonTX2022Curr", "KansasCityMO2022Curr",
           "MadisonWI2022Curr",  "MilwaukeeWI2022Curr",  "MinneapolMN2022Curr",  "PittsburghPA2022Curr",
           "PortlandME2022Curr", "PortlandOR2022Curr", "ProvidenceRI2022Curr", "RochesterNY2022Curr", 
           "SanAntonioTX2022Curr", "SanDiegoCA2022Curr", "SpringfielMO2020Curr", "StLouisMO2022Curr",    
           "TrentonNJ2022Curr", "WashingtonDC2022Curr" )

pcv_out <- list() 

for(i in c(1:length(evals))){   # to run all cities. 
  
  # Specify the EVALID (evaluation ID)
  city_choose <- evals[i]  #city_choose <- evals[1]
  
  # subset the Pop stratum Calc (psc) table for a single evaluation
  ## The rows provide the NLCD-based area expansion factors, each plot is assigned to one.
  Stratum <- psc[psc$EVALID == city_choose ,]
  Stratum$STRATUM_CN <- as.factor(Stratum$CN)
  
  ##  Select only the city estimation unit for 3 cities that have multiple estimation units
  if(city_choose == "StLouis2021Curr"){
    Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of St. Louis, MO",] }
  
  if(city_choose == "KansasCity2021Curr"){
    Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of Kansas City, MO",] }
  
  if(city_choose == "SanAntonio2021Curr"){
    Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of San Antonio, TX",] }
  
  # parse Stratum dataframe to identify the city
  parsed_city_name <- sub(" [0-9]{4}.*", "", unique(Stratum$EVAL_NAME))
  
  ## now find which plots are associated with each strata using the psca table.
  ##  Plot stratum calc assign- has plot (PLT_CN) and Stratum (PSC_CN) pairings.
  Plot <- psca[psca$PSC_CN %in% Stratum$CN ,]
  
  # bring the lat and lon over from the plot  (plt) table.
  Plot$LON <- plt$LON[match(Plot$PLT_CN, plt$CN)]
  Plot$LAT <- plt$LAT[match(Plot$PLT_CN, plt$CN)]
  
  # include label for plot status
  Plot$PLOT_STATUS_CD_LAB <- plt$PLOT_STATUS_CD_LAB[match(Plot$PLT_CN, plt$CN)]
  
  # Add the eval ID to the Plot dataframe
  Plot$EVALID <- city_choose
  
  # Set CRS of Plot df to nad 83
  sf_plots <-  st_as_sf(Plot, coords = c("LON", "LAT"),crs = 4269) 
  
  # bring in state code for census API
  sf_plots$state <- plt$STATECD[match(sf_plots$PLT_CN, plt$CN)]
  
  # Make up a random plot number to obfuscate plot_CN 
  #DK: Note: this is no longer needed but maybe not worth removing
  random_numbers <- sample(100000:999999,  dim(sf_plots)[1] , replace = TRUE)
  sf_plots$plot_number <- paste( random_numbers ,sf_plots$EVALID, sep="-")
  
  
  
  ###################################################
  ###################################################
  ## Retrieve the block group values for the state.
  ######   Now get the block group income
  bg <- get_acs(geography = "block group", 
                variables = c(medincome = "B19013_001"), 
                state = unique(sf_plots$state), 
                year = 2020,
                geometry = TRUE) 
  
  places <- get_acs(geography = "place", 
                    variables = c(medincome = "B19013_001"), 
                    state = unique(sf_plots$state), 
                    year = 2020,
                    geometry = TRUE) 
  
  # find the city in the place file. Troubleshooting
  #as.data.frame(places[1400:1500,])
  # places[1474,]
  
  # Find the index of the best match
  best_match_index <- stringdist::amatch(parsed_city_name, places$NAME , maxDist = Inf)
  
  # Get the best matching row from the dataframe
  just_the_place <- places[best_match_index, ]
  
  # spatial subset of the state block groups by the city place boundary
  city_block_groups <- bg[just_the_place, ]
  
  
  # this catches an error with Kansas City and Houston
  if(just_the_place$NAME =="Warsaw city, Missouri"){
    just_the_place <- places[places$NAME=="Kansas City city, Missouri" ,]
    city_block_groups <- bg[just_the_place, ]
  }
  
  # this catches an error with Houston 2021
  if(just_the_place$NAME =="Howe town, Texas"){
    just_the_place <- places[places$NAME=="Houston city, Texas" ,]
    city_block_groups <- bg[just_the_place, ]
  }
  
  # calculate block group area
  city_block_groups<-  city_block_groups %>% mutate(area = st_area(.))
  
  
  ## Use this intersect  to only use block groups in the city of baltimore
  plot_census_value <- st_intersection(city_block_groups, sf_plots) %>% 
    mutate(GEOID_11 = substring(GEOID, 1,11)) #extracting the GEOID of the census tract from the block group
  
  # Indicate which rows were initially NA from the census block group (these get filled with census tract later)
  plot_census_value$na_block_group <- is.na(plot_census_value$estimate)
  
  
  ### extracting median income values that are NA at the block group from the census tract level
  tract <- get_acs(geography = "tract", 
                   variables = c(medincome = "B19013_001"), 
                   state = unique(sf_plots$state), 
                   year = 2020,
                   geometry = TRUE) 
  
  # calculate block group area
  tract <-  tract %>% 
    mutate(area = st_area(.))
  
  tract_join <- dplyr::select(tract, GEOID_11 = GEOID, estimate_tract = estimate,
                              moe_tract = moe, tract_area = area) %>% 
    st_drop_geometry()
  
  
  plot_census_value_tract <- left_join(plot_census_value, tract_join) %>% 
    mutate(estimate = case_when(is.na(estimate) ~ estimate_tract,
                                TRUE ~ estimate),
           moe = case_when(is.na(moe) ~ moe_tract,
                           TRUE ~ moe),
           GEOID = case_when(is.na(estimate) ~ GEOID_11,
                             TRUE ~ GEOID),
           area = case_when(is.na(estimate) ~ tract_area,
                            TRUE ~ area)) %>% 
    dplyr::select(-GEOID_11, -estimate_tract, -moe_tract, -tract_area)
  
  
  ###########################################
  
  #overwrite the original dataframe with the version that filled in NA values in block group median income with tract values
  plot_census_value <- plot_census_value_tract
  
  ###########################################   
  
  # identify which plots use block group values and which use tract values
  plot_census_value[plot_census_value$GEOID %in% tract_join$GEOID, "is_tract_value" ] <- "tract value"
  plot_census_value[is.na(plot_census_value$is_tract_value),"is_tract_value" ] <- "block group value"
  
  table(plot_census_value$is_tract_value)
  table(plot_census_value$na_block_group)
  
  # turn spatial file into a dataframe.
  pcv <- as.data.frame(plot_census_value)
  
  # clean up the columns saved in the excel file
  pcv <- pcv[ , c("GEOID","PLT_CN","plot_number","estimate","moe","area","is_tract_value","na_block_group","PLOT_STATUS_CD_LAB")]
  
  ### extract race/ethnicity for each plot ########################
  bg_race <- get_acs(geography = "block group", 
                     variables = c(c_white = "B03002_003", c_black = "B03002_004", c_latinx = "B03002_012"), 
                     #see description of race/ethnicity here: https://censusreporter.org/topics/race-hispanic/
                     state = unique(sf_plots$state), 
                     summary_var = "B03002_001",
                     year = 2020,
                     geometry = TRUE) %>% 
    mutate(estimate_p = estimate/summary_est,
           moe_p = moe/summary_est) %>%
    dplyr::select(-estimate, -moe, - summary_est, -summary_moe) %>% 
    st_drop_geometry() %>% #pivot_wider isn't working without this
    tidyr::pivot_wider(names_from = variable, values_from = c(estimate_p, moe_p)) #c(estimate_p, moe_p))
  
  pcv <- left_join(pcv, bg_race)  
  
  ### other census variables: building age and neighborhood density ###############
  # v17 <- load_variables(2020, "acs5", cache = TRUE)
  # View(filter(v17, geography == "block group"))
  bg_buildings <- get_acs(geography = "block group", 
                          variables = c(c_building_age = "B25035_001",
                                        c_pop = "B01003_001",
                                        c_poverty_tot = "B17010_001",
                                        c_poverty_below = "B17010_002"), 
                          #see description of race/ethnicity here: https://censusreporter.org/topics/race-hispanic/
                          state = unique(sf_plots$state), 
                          year = 2020,
                          geometry = TRUE) %>% 
    mutate( area = st_area(.)) %>% 
    st_drop_geometry() %>% #pivot_wider isn't working without this
    tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>% 
    mutate(people_km2 = estimate_c_pop / (as.numeric(area)/1000000),
           estimate_c_poverty = estimate_c_poverty_below/estimate_c_poverty_tot,
           estimate_c_building_age = case_when(estimate_c_building_age == 0 ~ NA, 
                                               .default = estimate_c_building_age)) %>% 
    select(-area)
  
  pcv <- left_join(pcv, bg_buildings)  
  
  
  
  #a few other book keeping options
  pcv <- mutate(pcv, city = str_extract(evals[i], "^\\D+")) #adding in the city
  pcv_out <- rbind(pcv_out, pcv)
  
  
  print(str_extract(evals[i], "^\\D+")) 
} #end UFIA data extraction loop


### save the file used in the analysis 
csv_out_path <- file.path(here::here(),"out")
#write.csv(pcv_out, file=file.path( csv_out_path, "plot_data_to_visualize.csv"))

#loading in the data from the 'tree census' project for the poor trees project ("script_for_charlie_v1.1.R")
#pcv_out <- read_csv("C:/Users/dsk273/Documents/tree_census/data_to_analyze/plot_data_to_visualize.csv")


### calculate pollen production for each individual tree #######################

### combining the plot and census data with the individual tree data ###########

### Fig 1: pollen production by city and genus #################################

### Fig 2: plot level pollen production as a function of poverty ###############

### Fig 3: plot level pollen production as a function of race/ethnicity ########


