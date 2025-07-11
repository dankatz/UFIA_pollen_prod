#This script is for quantifying pollen production in northeastern and midwestern US cities included in the UFIA 
#Author: Daniel Katz (dankatz@cornell.edu), to be used for Keily Peralta's senior thesis
# Pieces of the script are borrowed from Alex Young's work on the 'Poor Trees' project

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
library(stringr)
library(stringi)
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






######  get census tract data for each city ############################### 
# this section is copied from the poor trees project

#including additional cities that were available on 11/23/2024
evals <- NE_cities
# evals <- c("AustinTX2022Curr", "BaltimoreMD2022Curr", "BurlingtonVT2022Curr",  "ChicagoIL2022Curr",
#            "ClevelandOH2022Curr",  "DesMoinesIA2022Curr", "HoustonTX2022Curr", "KansasCityMO2022Curr",
#            "MadisonWI2022Curr",  "MilwaukeeWI2022Curr",  "MinneapolMN2022Curr",  "PittsburghPA2022Curr",
#            "PortlandME2022Curr", "PortlandOR2022Curr", "ProvidenceRI2022Curr", "RochesterNY2022Curr", 
#            "SanAntonioTX2022Curr", "SanDiegoCA2022Curr", "SpringfielMO2020Curr", "StLouisMO2022Curr",    
#            "TrentonNJ2022Curr", "WashingtonDC2022Curr" )

pcv_out <- list() 
for(i in c(1:length(evals))){   # to run all cities. 
  
  # Specify the EVALID (evaluation ID)
  city_choose <- evals[i]  #city_choose <- evals[11]
  
  # subset the Pop stratum Calc (psc) table for a single evaluation
  ## The rows provide the NLCD-based area expansion factors, each plot is assigned to one.
  Stratum <- psc[psc$EVALID == city_choose ,]
  Stratum$STRATUM_CN <- as.factor(Stratum$CN)
  
  
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
  sf_plots <-  st_as_sf(Plot, coords = c("LON", "LAT"),crs = 4269) #filter(sf_plots, PLT_CN == "354918270489998")
  
  # bring in state code for census API
  sf_plots$state <- plt$STATECD[match(sf_plots$PLT_CN, plt$CN)]
  
  ### Retrieve the block group income values for the state.
  bg <- get_acs(geography = "block group", 
                variables = c(medincome = "B19013_001", 
                              c_white = "B03002_003", c_black = "B03002_004", c_latinx = "B03002_012", 
                              c_all_races_ethnicities = "B03002_001",
                              #see description of race/ethnicity here: https://censusreporter.org/topics/race-hispanic/
                              c_building_age = "B25035_001",
                              c_pop = "B01003_001",
                              c_poverty_tot = "B17010_001",
                              c_poverty_below = "B17010_002"),
                state = unique(sf_plots$state), 
                year = 2020,
                geometry = TRUE) %>% 
     tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>% 
     mutate(area = st_area(.)) %>% 
     mutate(people_km2 = estimate_c_pop / (as.numeric(area)/1000000),
           estimate_c_poverty = estimate_c_poverty_below/estimate_c_poverty_tot,
           estimate_c_perc_poverty = 100 * estimate_c_poverty,
           estimate_c_perc_white = 100 * (estimate_c_white/estimate_c_all_races_ethnicities),
           estimate_c_building_age = case_when(estimate_c_building_age < 100 ~ NA,  #removing odd values
                                               .default = estimate_c_building_age))
  
  places <- get_acs(geography = "place", 
                    variables = c(medincome = "B19013_001"), 
                    state = unique(sf_plots$state), 
                    year = 2020,
                    geometry = TRUE) 
     
  # Find the index of the best match
  best_match_index <- stringdist::amatch(parsed_city_name, places$NAME , maxDist = Inf)
  
  # Get the best matching row from the dataframe
  just_the_place <- places[best_match_index, ]
  
  # spatial subset of the state block groups by the city place boundary
  city_block_groups <- bg[just_the_place, ]
  
  #it looks like "Trenton, NJ" actually includes Princeton and some areas outside the city too
  if(parsed_city_name == "Trenton, NJ"){ 
    city_block_groups <-  st_crop(bg, xmin = -75, ymin = 40, xmax = -74, ymax = 40.5) #plot(bg_test["estimate_c_white"])
  }
     

  ### using a buffered extraction to extract poverty and race
  sf_plots_buffer <- st_buffer(sf_plots, dist = 1000)

  # it would be better to use st_intersection to create a weighted mean, but that was prohibitively slow and
  # given that there are a very large number of block groups within each 3km2 circle, this should have a minor effect
  city_block_groups_mean_pov <- dplyr::select(city_block_groups, estimate_c_perc_poverty)
  buffered_plot_mean_pov <- aggregate(city_block_groups_mean_pov, sf_plots_buffer, mean, na.rm = TRUE) %>% 
    st_centroid() %>% st_buffer(1) #adding this in to remove rounding errors from points when trying to join later
  
  city_block_groups_mean_white <- dplyr::select(city_block_groups, estimate_c_perc_white)
  buffered_plot_mean_white <- aggregate(city_block_groups_mean_white, sf_plots_buffer, mean, na.rm = TRUE)%>% 
    st_centroid() %>% st_buffer(1) #adding this in to remove rounding errors from points when trying to join later
  
  pcv <- st_join(sf_plots, buffered_plot_mean_pov) 
  pcv <- st_join(pcv, buffered_plot_mean_white)
  pcv <- pcv %>% 
         dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                       lat = sf::st_coordinates(.)[,2]) %>% 
         st_drop_geometry(pcv) %>% 
         mutate(city = str_extract(evals[i], "^\\D+")) #adding in the city
  #dplyr::select(sf_plots_buffer, "PLT_CN", "income_full.estimate", "PLOT_STATUS_CD_LAB")
  
  # some visual tests
  # plot(just_the_place["estimate"])
  # plot(buffered_plot_mean_pov)
  # plot(bg["estimate_c_poverty"])
  # plot(city_block_groups["estimate_c_poverty"])
  # plot(city_block_groups_mean_pov)
  # plot(buffered_plot_mean_pov %>% st_buffer(500))
  # plot(pcv["estimate_c_perc_poverty"])
  # plot(pcv["estimate_c_perc_white"])
  
  
  pcv_out <- rbind(pcv_out, pcv)
  print(str_extract(evals[i], "^\\D+")) 
} #end UFIA data extraction loop




### save the file used in the analysis 
csv_out_path <- file.path(here::here())
#write_csv(pcv_out, file = file.path( csv_out_path, "plot_data_to_visualize.csv"))
#pcv_out <- read_csv(file = file.path( csv_out_path, "plot_data_to_visualize.csv"))


#some QAQC on rows that are missing poverty data
pcv$PLT_CN
names(pcv_out)
pcv_out %>% 
  group_by(city) %>% 
  summarise(sum_na = sum(is.na(estimate_c_perc_poverty)))
    
pcv_out %>% 
  filter(city == "MadisonWI") %>% 
ggplot(aes(x = lon, y = lat, col = estimate_c_perc_poverty)) + geom_point()   




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
for(i in 1:10){
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

pc <- left_join( all_trees_pollen_prod, pcv_out) %>% 
  mutate(trees_alive = case_when(STATUSCD == 2 ~ 0, #STATUSCD 1 == live tree, STATUSCD 2 == dead tree
                                 STATUSCD == 1 ~ 1),
         trees_planted = case_when(IS_PLANTED == 1 ~ 1, #1 == planted
                                   IS_PLANTED == 2 ~ 0, #2 == natural origin
                                   IS_PLANTED == 3 ~ NA),
         street_tree = IS_STREET_TREE,
         plot_perc_planted = 100 * trees_planted,
         plot_perc_street_tree = 100 * street_tree) %>% 
  
  filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
  filter(STATUSCD == 1 | STATUSCD == 2) %>%  #removing trees that weren't measured due to no longer being in the sample
  #STATUSCD 0 == tree is not in the remeasured plot, STATUSCD 3 == cut and utilized, STATUSCD 4 == removed
  filter(city %in% NE_cities_not_eval) %>% 
  filter(!is.na(pol_mean))

plots_per_city <- left_join( all_trees_pollen_prod, pcv_out) %>%  
  dplyr::group_by(city) %>% 
  dplyr::select(PLT_CN) %>% 
  dplyr::distinct() %>% 
  dplyr::count(city) %>% 
  dplyr::rename(n_plots = n)

plots_per_city %>%  ungroup() %>% 
  filter( !is.na(city)) %>% 
  summarize(n = sum(n_plots))
  
pc <- left_join(pc, plots_per_city) %>% 
  mutate(city = stri_sub(city, 1, -3), #remove state from city name for figures
         city = case_when(city == "Minneapol" ~ "Minneapolis", .default = city)) #expanding truncated name

  #check on any values where the census info didn't get through
  #test <- pc %>% filter(is.na(estimate_c_perc_poverty))


### statistics for paper ################################################

#summing across taxa, which cities have the highest pollen production
pc %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  
  group_by(city) %>% 
  summarize(pol_sum_m2_mean = sum(pol_sum_m2)) %>% 
  arrange(-pol_sum_m2_mean)

#averaging across cities, which taxa have the highest pollen production
pc %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  
  group_by(Genus) %>% 
  summarize(pol_sum_m2_mean = mean(pol_sum_m2),
            pol_sum_m2_sd = sd(pol_sum_m2)) %>% 
  arrange(-pol_sum_m2_mean)

#across cities, what proportion of pollen is from planted vs unplanted trees
planted_vs_unplanted <- 
  pc %>%
  mutate(trees_planted_label = case_when(trees_planted == 0 ~ "natural",
                                         trees_planted == 1 ~ "planted",
                                         is.na(trees_planted) ~ "unknown")) %>% 
  dplyr::group_by(city, trees_planted_label, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%   # Plot area in m2 = 672.4535
  group_by(city, trees_planted_label) %>% 
  summarize(pol_sum_m2_sum = sum(pol_sum_m2)) %>% 
  group_by(trees_planted_label) %>% 
  summarize(pol_sum_m2_sum_mean = mean(pol_sum_m2_sum))

planted_vs_unplanted$pol_sum_m2_sum_mean[planted_vs_unplanted$trees_planted_label == "planted"] /
( planted_vs_unplanted$pol_sum_m2_sum_mean[planted_vs_unplanted$trees_planted_label == "planted"] + 
  planted_vs_unplanted$pol_sum_m2_sum_mean[planted_vs_unplanted$trees_planted_label == "natural"])
  

#across cities, what proportion of pollen is from street trees
street_vs_non_street <- 
  pc %>%
  mutate(street_tree_label = case_when(street_tree == 0 ~ "not a street tree",
                                       street_tree == 1 ~ "street tree")) %>% 
  dplyr::group_by(city, street_tree_label, n_plots, Genus) %>%
  dplyr::summarize(pol_sum = sum(pol_mean)) %>%  
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>% # Plot area in m2 = 672.4535
  group_by(city, street_tree_label) %>% 
  summarize(pol_sum_m2_sum = sum(pol_sum_m2)) %>% 
  group_by(street_tree_label) %>% 
  summarize(pol_sum_m2_sum_mean = mean(pol_sum_m2_sum))

street_vs_non_street$pol_sum_m2_sum_mean[street_vs_non_street$street_tree_label == "street tree"] /
  ( street_vs_non_street$pol_sum_m2_sum_mean[street_vs_non_street$street_tree_label == "not a street tree"] + 
      street_vs_non_street$pol_sum_m2_sum_mean[street_vs_non_street$street_tree_label == "street tree"])




### Fig 1 pollen production per genus ###################################
fig1a <- pc %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  # Plot area in m2 = 672.4535
  ggplot(aes(x = city, y = pol_sum_m2*1000, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen per land area (1,000 grains/"~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy")


ba_per_city <- 
  pc %>% 
  mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
          BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
  dplyr::group_by(city) %>% 
  dplyr::summarize(BA_sum_city = sum(BA_m2))

fig1b <- pc %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  left_join(., ba_per_city) %>% 
  mutate(pol_BA = pol_sum/BA_sum_city) %>% 
  ggplot(aes(x = city, y = pol_BA, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen per basal area (1,000,000 grains/"~m^2~")" )) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy")

cowplot::plot_grid(fig1a, fig1b, nrow = 2)


### Fig 2: basal area by city and genus #################################
pc_npp <- left_join( all_trees_pollen_prod, pcv_out) %>% 
  mutate(trees_alive = case_when(STATUSCD == 2 ~ 0, #STATUSCD 1 == live tree, STATUSCD 2 == dead tree
                                 STATUSCD == 1 ~ 1)) %>% 
  filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
  filter(STATUSCD == 1 ) %>%  #removing trees that weren't measured due to no longer being in the sample
  filter(city %in% NE_cities_not_eval)

#currently this doesn't include plots without any trees nor strata corrections
area_per_city <- left_join( all_trees_pollen_prod, pcv_out) %>%  
  dplyr::group_by(city) %>% 
  dplyr::select(PLT_CN) %>% 
  dplyr::distinct() %>% 
  dplyr::count(city) %>% 
  dplyr::rename(n_plots = n) %>% 
  mutate(area_per_city = n_plots * 672.4535)  # Plot area in m2 = 672.4535)

BA_sum_city <- 
  pc_npp %>% 
  mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
          tree_BA_m2 = (pi * dbh_cm^2)/10000) %>%  
  dplyr::group_by(city) %>% 
  dplyr::summarize(
    BA_sum_city = sum(tree_BA_m2),
    n_trees_city = n())
    
#visualize results
pc_npp %>% 
  mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
          tree_BA_m2 = (pi * dbh_cm^2)/10000) %>%  
  group_by(city, Genus) %>% 
  dplyr::summarize(BA_sum = sum(tree_BA_m2)) %>% 
  ungroup() %>% 
  left_join(., area_per_city) %>% 
  left_join(., BA_sum_city) %>% 
  mutate(Genus = case_when(BA_sum / BA_sum_city < .02 ~ "other",
                           BA_sum / BA_sum_city >= 0.02 ~ Genus),
         area_per_city_ha = area_per_city/10000) %>% 
  mutate(city = stri_sub(city, 1, -3),
         city = case_when(city == "Minneapol" ~ "Minneapolis", .default = city)) %>%
   ggplot(aes(x = city, y = BA_sum/area_per_city_ha, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote(basal~area~(m^2~"/"~ha))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "light")

#ggsave("C:/Users/dsk273/Box/writing/UFIA pollen production/fig2_ba_by_city.pdf", height = 1500, width = 3000, units = "px")




### Fig 3: differences in pollen production between planted and natural trees ################
pc %>%
  mutate(trees_planted_label = case_when(trees_planted == 0 ~ "natural",
                                         trees_planted == 1 ~ "planted",
                                         is.na(trees_planted) ~ "unknown")) %>% 
   dplyr::group_by(city, trees_planted_label, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  # Plot area in m2 = 672.4535
 
   ggplot(aes(x = city, y = pol_sum_m2*1000000, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen production (pollen grains /  "~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy") + facet_wrap(~trees_planted_label, ncol = 1)


### Fig 4: differences between street trees and non-street trees #################
pc %>%
  mutate(street_tree_label = case_when(street_tree == 0 ~ "not a street tree",
                                       street_tree == 1 ~ "street tree")) %>% 
  dplyr::group_by(city, street_tree_label, n_plots, Genus) %>%
  dplyr::summarize(pol_sum = sum(pol_mean)) %>%  
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>% # Plot area in m2 = 672.4535
  ggplot(aes(x = city, y = pol_sum_m2*1000000, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen production (grains / "~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy") + facet_wrap(~street_tree_label, ncol = 1)


### SI: are there associations between poverty and pollen production for any tree genera ##########
pc %>% 
  dplyr::group_by(city, PLT_CN, estimate_c_perc_poverty, estimate_c_perc_white, Genus) %>% 
  dplyr::summarize(pol_sum_plot = sum(pol_mean)) %>% 
  ggplot(aes(x = estimate_c_perc_white , y = pol_sum_plot, color = city)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Genus)

### SI: misc sub-analyses ####################
pc %>% 
  filter(street_tree == 1) %>% 
  dplyr::group_by(city, PLT_CN, estimate_c_perc_poverty, estimate_c_perc_white, Genus) %>% 
  dplyr::summarize(pol_sum_plot = sum(pol_mean)) %>% 
  ggplot(aes(x = estimate_c_perc_white , y = pol_sum_plot)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Genus)

### misc stats and other things  ###########

pc %>% select(PLT_CN) %>% distinct() %>%  summarize(n = n())

# export the dataframe for keily
write_csv(pc, file = "pc.csv")





### SI 1: plot level pollen production as a function of poverty ###############

plot_summary <- pc %>% 
  dplyr::group_by(city, PLT_CN, estimate_c_perc_poverty, estimate_c_perc_white) %>% 
  dplyr::summarize(pol_sum_plot = sum(pol_mean))


#do some stats
m1 <- lmer(pol_sum_plot ~ (1|city) + estimate_c_perc_poverty, data = plot_summary)
m1_summary <- summary(m1) #plot(m1)
m1_slope <- round(m1_summary$coefficients[2,1], 2)
m1_intercept <- round(m1_summary$coefficients[1,1], 2)
m1_slope_p <- round(m1_summary$coefficients[2,5], 2)

#create plot
sjPlot::plot_model(m1, type = "pred") +   
  geom_point(aes(x = estimate_c_perc_poverty, y = pol_sum_plot, color = as.factor(city)),  data = plot_summary) +  
  xlab("households in poverty (%)") + ylab("total pollen production per plot (millions of grains)") +
  ggthemes::theme_few() + scale_color_discrete()+
  ggtitle("association between poverty and pollen production")+
  annotate("text", x = 50, y = 3000, label = paste0("y = ", m1_slope, " * x + ", m1_intercept, ", p = ", m1_slope_p)) 



### SI 2: plot level pollen production as a function of race/ethnicity #############
plot_summary <- pc %>% 
  dplyr::group_by(city, PLT_CN, estimate_c_perc_poverty, estimate_c_perc_white) %>% 
  dplyr::summarize(pol_sum_plot = sum(pol_mean))

#do some stats
m2 <- lmer(pol_sum_plot ~ (1|city) + estimate_c_perc_white, data = plot_summary)
m2_summary <- summary(m2) #plot(m2)
m2_slope <- round(m2_summary$coefficients[2,1], 2)
m2_intercept <- round(m2_summary$coefficients[1,1], 2)
m2_slope_p <- round(m2_summary$coefficients[2,5], 2)

#create plot
sjPlot::plot_model(m2, type = "pred") +   
  geom_point(aes(x = estimate_c_perc_white, y = pol_sum_plot, color = as.factor(city)),  data = plot_summary) +  
  xlab("white (%)") + ylab("total pollen production per plot (millions of grains)") +
  ggthemes::theme_few() + scale_color_discrete()+
  ggtitle("association between race and pollen production")+
  annotate("text", x = 50, y = 3000, label = paste0("y = ", m2_slope, " * x + ", m2_intercept, ", p = ", m2_slope_p)) 

