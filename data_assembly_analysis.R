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
#library(plyr)
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





### calculate pollen production for each individual adult tree #######################

ref_species_join <- ref_species %>% select(SPCD, GENUS, SPECIES, COMMON_NAME)

all_trees_for_pollen_prod <- left_join(mtre, ref_species_join) %>% 
  select(PLT_CN, SPCD, SUBP, STATUSCD, IS_PLANTED, BASAL_AREA, DIA, IS_STREET_TREE,
         GENUS, SPECIES, COMMON_NAME) %>% 
  dplyr::mutate(Genus = GENUS,
         Species = gsub("^\\S+ ", "", SPECIES),
         dbh_cm = DIA *2.54,
         tree_BA = 0.00007854 * dbh_cm^2,
         id = row_number())

#calculate pollen production for each individual tree, now including SDs
for(i in 1:100){
  Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_b <- rnorm(n = 1, mean = 21.98, sd =0.28)
  Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
  Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
  Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)*0.578
  Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
  Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
  it_dbh_genus_np_i <-  #
    all_trees_for_pollen_prod %>%  
    mutate(per_tree_pollen_prod = case_when(
      Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio,
      Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_BA + Acpl_param_b,
      Genus == "Acer" & Species == "rubrum"  ~ ( Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
      Genus == "Acer" & Species == "saccharinum"~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
      Genus == "Betula"  ~ Bepa_param_a* tree_BA + Bepa_param_b,
      Genus == "Gleditsia"  ~ Gltr_param_a * tree_BA + Gltr_param_b,
      Genus == "Juglans"  ~ Juni_param_a * tree_BA + Juni_param_b,
      Genus == "Morus"  ~ (Mosp_param_c * tree_BA^2 + Mosp_param_a * tree_BA + Mosp_param_b) *0.578, #.58 adjusts for sex ratio
      Genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
      Genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
      Genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
      Genus == "Quercus" & Species == "palustris"  ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
      Genus == "Ulmus"  ~ ( Ulsp_param_a * tree_BA + Ulsp_param_b) 
    ),
    iter = i ) #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up
  
  
  ifelse(i == 1,
         it_dbh_genus_np_all <- it_dbh_genus_np_i,
         it_dbh_genus_np_all <- bind_rows(it_dbh_genus_np_all, it_dbh_genus_np_i))
  print(i)
}

indiv_tree_pol_pred <- it_dbh_genus_np_all %>% 
  group_by(id) %>% 
  dplyr::summarize(pol_mean = mean(per_tree_pollen_prod), 
            pol_sd = sd(per_tree_pollen_prod))

all_trees_pollen_prod <- left_join(all_trees_for_pollen_prod, indiv_tree_pol_pred)
  



### combining the plot and census data with the individual tree data ###########

all_trees2 <- left_join( all_trees_pollen_prod, pcv_out)

#stopping here for the moment, need to do some QA/QC on why so many of the rows are missing info from pcv_out




    mutate(trees_alive = case_when(STATUSCD == 2 ~ 0, #STATUSCD 1 == live tree, STATUSCD 2 == dead tree
                                 STATUSCD == 1 ~ 1),
         trees_planted = case_when(IS_PLANTED == 1 ~ 1, #1 == planted
                                   IS_PLANTED == 2 ~ 0, #2 == natural origin
                                   IS_PLANTED == 3 ~ 0),
         stree_tree = IS_STREET_TREE) 



  mutate(estimate_c_building_age = case_when(estimate_c_building_age == 0 ~ NA, #removing odd values
                                             estimate_c_building_age > 100 ~ estimate_c_building_age)) %>%  
  mutate(estimate_c_perc_poverty = 100 * estimate_c_poverty,
         estimate_c_perc_white = 100 * estimate_p_c_white,
         plot_perc_planted = 100 * trees_planted,
         plot_perc_street_tree = 100 * stree_tree) 

  filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
  filter(STATUSCD == 1 | STATUSCD == 2) %>% #removing trees that weren't measured due to no longer being in the sample
  #STATUSCD 0 == tree is not in the remeasured plot, STATUSCD 3 == cut and utilized, STATUSCD 4 == removed



 # filter(city %in% NE_cities_not_eval)



### Fig 1: pollen production by city and genus #################################

### Fig 2: plot level pollen production as a function of poverty ###############

### Fig 3: plot level pollen production as a function of race/ethnicity ########


