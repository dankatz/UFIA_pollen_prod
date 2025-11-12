# This script is for quantifying pollen production in northeastern and midwestern US cities included in the UFIA 
# Author: Daniel Katz (dankatz@cornell.edu), to be used for Keily Peralta's senior thesis
# note: an earlier version of this manuscript included comparisons between race and poverty and pollen production; that 
# has been removed




### prepare work environment ###################################################


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


### compile UFIA dataset ########################################################
# for additional information on the complex relational UFIA database, visit the UFIA datamart 
# and download the Urban FIADB USER GUIDES; this version follows V12.0 
  
  ## join the within-plot datasets
  ref_species_join <- ref_species %>% select(SPCD, GENUS, SPECIES, COMMON_NAME) #species names
  
  mtre_join <- mtre %>%  #the "Mother tree" table
    select(TREE, SUBP,  STATUSCD, SPCD, DIA, CROWN_DIA_WIDE, CROWN_DIA_90, AVG_CROWN_WIDTH, IS_STREET_TREE, IS_PLANTED, BASAL_AREA,
           CN, PLT_CN, SBP_CN, CND_CN) %>% 
    left_join(., ref_species_join)
  
  plt_join <- plt %>%  #add information from the plot level table
    select(MEAS_YEAR, MEAS_MONTH, MEAS_DAY, PLOT_STATUS_CD, PLOT_NONSAMPLE_REASN_CD, LAT, LON, CN) %>% 
    rename(PLT_CN = CN)
  
  mtre_plt <- left_join(mtre_join, plt_join) 
  

#get the meta-plot data and then filter down to just target cities in just target years
  psca_join <- psca %>% 
    rename(psca_cn = CN,
           psca_PLT_CN = PLT_CN,
           psca_PSC_CN = PSC_CN)
  
  psc_join <- psc # %>%  filter(EVALID %in% NE_cities)
  
  psc_psca <- left_join(psca_join, psc_join, by = c("psca_PSC_CN" = "CN")) %>% 
          filter(EVALID %in% NE_cities)
  
#add meta plot data to the tree-level data, create derived variables, and filter
  pcv <- left_join(mtre_plt, psc_psca, by = c("PLT_CN" = "psca_PLT_CN")) %>% 
    filter(!is.na(EVALID)) %>% 
    mutate(trees_alive = case_when(STATUSCD == 2 ~ 0, #STATUSCD 1 == live tree, STATUSCD 2 == dead tree
                                   STATUSCD == 1 ~ 1),
           trees_planted = case_when(IS_PLANTED == 1 ~ 1, #1 == planted
                                     IS_PLANTED == 2 ~ 0, #2 == natural origin
                                     IS_PLANTED == 3 ~ NA),
           street_tree = IS_STREET_TREE,
           plot_perc_planted = 100 * trees_planted,
           plot_perc_street_tree = 100 * street_tree,
           city_state = gsub(pattern = "City of ", replacement = "", x = ESTN_UNIT_NAME), 
           city = stri_sub(city_state, 1, -5), #remove state from city name for figures
           city = case_when(city == "Portland, ME Urban Area:Portland" ~ "Portland, ME",
                              city == "Trenton, NJ Urban " ~ "Trenton",
                              .default = city),
           Genus = GENUS,
           Species = gsub("^\\S+ ", "", SPECIES),
           dbh_cm = DIA *2.54,
           tree_BA = 0.00007854 * dbh_cm^2,
           tree_can_diam_ft = case_when( !is.na(CROWN_DIA_WIDE) ~  (CROWN_DIA_WIDE + CROWN_DIA_90)/2,
                                         is.na(CROWN_DIA_WIDE) ~  AVG_CROWN_WIDTH), #use modeled width when diameter wasn't measured
           tree_can_diam_m = tree_can_diam_ft * 0.3048,
           tree_can_radius_m = tree_can_diam_m/2,
           tree_area = pi * (tree_can_radius_m^2), #convert crown radius in m to m2
           id = row_number()) %>% 
    filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
    filter(STATUSCD == 1 | STATUSCD == 2) %>% #removing trees that weren't measured due to no longer being in the sample
    #STATUSCD 0 == tree is not in the remeasured plot, STATUSCD 3 == cut and utilized, STATUSCD 4 == removed
    filter(trees_alive == 1) %>%  #removing dead trees
    filter(STRATUM_LABEL != "Water") #removing plots that are in open water
    
  
  
## calculate the number of plots in the city in that census (including plots without trees)  
  plots_per_city <-
    psc_psca %>% 
    filter(STRATUM_LABEL != "Water") %>% #removing plots that are in open water
    group_by(POPULATION_NAME) %>% 
    summarize(n_plots = n())
  pcv <- left_join(pcv, plots_per_city) 

#calculate land area of city within surveying bounds (i.e., removing water area)
  city_land_acres <- psc_psca %>% 
    mutate(POPULATION_ACRES_land = POPULATION_ACRES - STRATUM_ACRES) %>% 
    filter(STRATUM_LABEL == "Water") %>% #removing plots that are in open water
    select(POPULATION_NAME, POPULATION_ACRES_land) %>% #, POPULATION_ACRES , STRATUM_LABEL, STRATUM_ACRES ) %>% 
     distinct()
  pcv <- left_join(pcv, city_land_acres) 
  
### calculate pollen production for each individual adult tree #######################
# these calculations are based off the data presented in Katz et al. 2020
# additional analyses were conducted to create equations that are more robust for particular taxa
# the script for that is available here: "C:/Users/dsk273/Box/MIpostdoc/trees/pollen per tree/pollen per tree analysis and figs 251031.R"
  all_trees_for_pollen_prod <- pcv 

#calculate pollen production for each individual tree, using a loop allows SDs to be calculated
for(i in 1:100){

### canopy area
  Acne_param_a <- rnorm(n = 1, mean = 0.49, sd = 0.09)
  Acne_param_b <- rnorm(n = 1, mean = -3.16, sd = 3.72)
  Acpl_param_a <- rnorm(n = 1, mean = 0.04, sd = 0.01)
  Acpl_param_b <- rnorm(n = 1, mean = 0.53, sd = 0.42)
  Acru_param_a <- rnorm(n = 1, mean = 0.10, sd = 0.02)
  Acru_param_b <- rnorm(n = 1, mean = -0.10, sd = 0.64)
  Acsa_param_a <- rnorm(n = 1, mean = 0.05, sd =0.02)
  Acsa_param_b <- rnorm(n = 1, mean = 2.59, sd =2.33)
  Bepa_param_a <- rnorm(n = 1, mean = 1.27, sd = 0.55)
  Bepa_param_b <- rnorm(n = 1, mean = -4.73, sd = 8.92)
  Gltr_param_a <- rnorm(n = 1, mean = 0.89, sd = 0.14)
  Gltr_param_b <- rnorm(n = 1, mean = -6.13, sd = 2.58)
  Juni_param_a <- rnorm(n = 1, mean = 0.66, sd = 0.15)
  Juni_param_b <- rnorm(n = 1, mean = 1.42, sd = 8.30)
  Mosp_param_a <- rnorm(n = 1, mean = 0.035375, sd = 0.005136) 
  Mosp_param_b <- rnorm(n = 1, mean = 24.260318, sd = 0.424734)
  Plac_param_a <- rnorm(n = 1, mean = 1.87, sd = 0.48)
  Plac_param_b <- rnorm(n = 1, mean = -26.43, sd = 16.48)
  Posp_param_a <- rnorm(n = 1, mean = 1.2995, sd = 0.3054) 
  Posp_param_b <- rnorm(n = 1, mean = -34.7360, sd = 54.8874)
  Qusp_param_a <- rnorm(n = 1, mean = 0.97, sd = 0.16)
  Qusp_param_b <- rnorm(n = 1, mean = 17.02, sd = 9.54)
  Qupa_param_a <- rnorm(n = 1, mean = 0.65, sd =0.19)
  Qupa_param_b <- rnorm(n = 1, mean = 8.30, sd = 6.88)
  Ulsp_param_a <- rnorm(n = 1, mean = 1.263, sd = 0.420) 
  Ulsp_param_b <- rnorm(n = 1, mean = -38.904, sd = 98.023)  #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
  it_dbh_genus_np_i <-  #
    all_trees_for_pollen_prod %>%  
    
    #protect against unrealistic large trees having overly large numbers by setting canopy area equal to the max recorded in the underlying dataset
    # note: this is only an issue for non-linear relationships, which have been substituted for linear relationships for everything besides Morus
    mutate(tree_area_c = case_when(
                            # Genus == "Acer" & Species == "negundo" & tree_area > 300 ~ 300,
                            # Genus == "Acer" & Species == "platanoides" & tree_area > 200 ~ 200,
                            # Genus == "Acer" & Species == "rubrum"  & tree_area > 150 ~ 150,
                            # Genus == "Acer" & Species == "saccharinum" & tree_area > 500 ~ 500,
                            # Genus == "Betula" & tree_area > 150 ~ 150, 
                            # Genus == "Gleditsia" & tree_area > 300 ~ 300,
                            # Genus == "Juglans" & tree_area > 300 ~ 300,
                            Genus == "Morus" & tree_area > 162 ~ 162,
                            # Genus == "Platanus" & tree_area > 300 ~ 300,
                            # Genus == "Populus" & tree_area > 300 ~ 300,
                            # Genus == "Quercus" & tree_area >  400 ~ 400, #red oaks and unknown oaks
                            # Genus == "Quercus" & Species == "palustris" & tree_area > 150 ~ 150,
                            # Genus == "Ulmus" & tree_area > 400 ~ 400,
                           .default = tree_area
                          )) %>% 
    #calculate per tree pollen production as a function of canopy area
    mutate(per_tree_pollen_prod = case_when(
          Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_area_c + Acne_param_b) *0.558, #.558 is the sex ratio,
          Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_area_c + Acpl_param_b,
          Genus == "Acer" & Species == "rubrum"  ~ ( Acru_param_a * tree_area_c + Acru_param_b) * 0.106, #.106 is the sex ratio
          Genus == "Acer" & Species == "saccharinum"~ Acsa_param_a * tree_area_c + Acsa_param_b, 
          Genus == "Betula"  ~ Bepa_param_a* tree_area_c + Bepa_param_b,
          Genus == "Gleditsia"  ~ Gltr_param_a * tree_area_c + Gltr_param_b,
          Genus == "Juglans"  ~ Juni_param_a * tree_area_c + Juni_param_b,
          Genus == "Morus"  ~ ((exp( Mosp_param_a * tree_area_c + Mosp_param_b) )/1000000000 ) * 0.578, #convert to billions and adjust for sex ratio
          Genus == "Platanus"  ~ Plac_param_a * tree_area_c + Plac_param_b,
          Genus == "Populus"  ~ ( Posp_param_a * tree_area_c + Posp_param_b) * 0.482, #adjust for sex ratio
          Genus == "Quercus"  ~ Qusp_param_a * tree_area_c + Qusp_param_b, #red oaks and unknown oaks
          Genus == "Quercus" & Species == "palustris"  ~ Qupa_param_a * tree_area_c + Qupa_param_b, #pin oaks
          Genus == "Ulmus"  ~ ( Ulsp_param_a * tree_area_c + Ulsp_param_b)
        )) %>% 
    
      #protect against small trees having negative numbers
      mutate(per_tree_pollen_prod = case_when(per_tree_pollen_prod < 0 ~ 0,
                                              per_tree_pollen_prod > 0 ~ per_tree_pollen_prod)) %>% 
      mutate(iter = i ) #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up
  
  
 ### basal area
  Acne_param_ba_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_ba_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_ba_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_ba_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_ba_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_ba_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_ba_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_ba_b <- rnorm(n = 1, mean = 21.98, sd =0.28)
  Bepa_param_ba_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_ba_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_ba_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
  Gltr_param_ba_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_ba_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_ba_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_ba_a <- rnorm(n = 1, mean = 6029.09, sd = 1527.62)
  Mosp_param_ba_b <- rnorm(n = 1, mean = -12.21, sd = 1530.70)*0.578
  Mosp_param_ba_c <- rnorm(n = 1, mean = 255.64, sd = 195.53)
  Plac_param_ba_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_ba_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_ba_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_ba_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_ba_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_ba_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_ba_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_ba_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_ba_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_ba_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  it_dbh_genus_np_ba_i <-  #
    all_trees_for_pollen_prod %>%
    mutate(per_tree_pollen_prod_ba = case_when(
      Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_ba_a * tree_BA + Acne_param_ba_b) *0.558, #.558 is the sex ratio,
      Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_ba_a * tree_BA + Acpl_param_ba_b,
      Genus == "Acer" & Species == "rubrum"  ~ ( Acru_param_ba_a * tree_BA + Acru_param_ba_b) * 0.106, #.106 is the sex ratio
      Genus == "Acer" & Species == "saccharinum"~ (exp( Acsa_param_ba_a * tree_BA + Acsa_param_ba_b))/1000000000, #convert to billions
      Genus == "Betula"  ~ Bepa_param_ba_a* tree_BA + Bepa_param_ba_b,
      Genus == "Gleditsia"  ~ Gltr_param_ba_a * tree_BA + Gltr_param_ba_b,
      Genus == "Juglans"  ~ Juni_param_ba_a * tree_BA + Juni_param_ba_b,
      Genus == "Morus"  ~ (Mosp_param_ba_a * tree_BA^2 + Mosp_param_ba_b * tree_BA + Mosp_param_ba_c) *0.578, #.58 adjusts for sex ratio
      Genus == "Platanus"  ~ Plac_param_ba_a * tree_BA + Plac_param_ba_b,
      Genus == "Populus"  ~ (exp( Posp_param_ba_a * tree_BA + Posp_param_ba_b) * 0.482)/1000000000, #convert to billions
      Genus == "Quercus"  ~ Qusp_param_ba_a * tree_BA + Qusp_param_ba_b, #red oaks and unknown oaks
      Genus == "Quercus" & Species == "palustris"  ~ Qupa_param_ba_a * tree_BA + Qupa_param_ba_b, #pin oaks
      Genus == "Ulmus"  ~ ( Ulsp_param_ba_a * tree_BA + Ulsp_param_ba_b)
    ),
    iter = i ) %>% #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up
  #protect against small trees having negative numbers
  mutate(per_tree_pollen_prod_ba = case_when(per_tree_pollen_prod_ba < 0 ~ 0,
                                             per_tree_pollen_prod_ba > 0 ~ per_tree_pollen_prod_ba))
    
  it_dbh_genus_np_i <- left_join(it_dbh_genus_np_i, it_dbh_genus_np_ba_i)
    
  ifelse(i == 1,
         it_dbh_genus_np_all <- it_dbh_genus_np_i,
         it_dbh_genus_np_all <- bind_rows(it_dbh_genus_np_all, it_dbh_genus_np_i))
  print(i)
}

indiv_tree_pol_pred <- it_dbh_genus_np_all %>% 
  group_by(id) %>% 
  dplyr::summarize(
            pol_mean = mean(per_tree_pollen_prod), 
            pol_sd = sd(per_tree_pollen_prod),
            pol_ba_mean = mean(per_tree_pollen_prod_ba), 
            pol_ba_sd = sd(per_tree_pollen_prod_ba),
            )

all_trees_pollen_prod <- left_join(all_trees_for_pollen_prod, indiv_tree_pol_pred) %>%
  #the equations are a little untrustworthy for Morus canopy area b/c it's exponential and doesn't include much of the observed range, 
  #therefore using the predictions for BA for just this genus
  mutate(pol_mean = case_when(Genus == "Morus" ~ pol_ba_mean, .default = pol_mean), 
         pol_sd = case_when(Genus == "Morus" ~ pol_ba_sd, .default = pol_sd))
 
#add the pollen production back to pcv
all_trees_pollen_prod_join <- all_trees_pollen_prod %>% select(city, PLT_CN, TREE, DIA, LON, LAT, SPECIES, pol_mean, pol_sd, pol_ba_mean,  pol_ba_sd)
pcv <- left_join(pcv, all_trees_pollen_prod_join, by = c("city", "PLT_CN", "TREE", "DIA", "LON", "LAT"))
 
### save the file used in the analysis 
csv_out_path <- file.path(here::here())
#write_csv(pcv, file = file.path( csv_out_path, "tree_data_251112.csv"))
#pcv <- read_csv(file = file.path( csv_out_path, "tree_data_251112.csv"))



# #canopy area pollen production
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_mean)) %>% 
# ggplot(aes(x = tree_area, y = pol_mean, col = Species)) + geom_point(alpha = 0.1) + facet_wrap(~Genus, scales = "free") + theme_bw() 
# 
# #basal area pollen production
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_ba_mean)) %>% 
#   ggplot(aes(x = tree_BA, y = pol_ba_mean, col = Species)) + geom_point(alpha = 0.1) + facet_wrap(~Genus, scales = "free") + theme_bw() 
# 
# #compare pollen prod from canopy area-based approach to DBH based approach
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_mean)) %>% 
#   #filter(Genus == "Quercus" ) %>% 
#   #filter(tree_area < 200) %>% 
#   ggplot(aes(x = pol_mean, y = pol_ba_mean, col = log(tree_BA/tree_area))) + geom_point(alpha = 0.8) + facet_wrap(~Genus, scales = "free") + theme_bw() +
#   geom_abline(slope = 1, intercept = 0, lty = 2) + geom_smooth(method = "lm") + scale_color_viridis_c()
# 
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_mean)) %>% 
#   filter(Genus == "Quercus") %>% 
#   #filter(tree_area < 200) %>% 
#   ggplot(aes(x = tree_area, y = tree_BA, color = log(pol_ba_mean/pol_mean))) + geom_point(alpha = 0.8) + facet_wrap(~Species) + theme_bw() +
#   geom_abline(slope = 1, intercept = 0, lty = 2) + geom_smooth(method = "lm") + scale_color_viridis_c()
# 
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_mean)) %>% 
#   filter(Genus == "Morus" ) %>% 
#   ggplot(aes(x = dbh_cm, fill = Species)) + geom_histogram() + facet_wrap(~Species, scales = "free") + theme_bw() 
# 
# all_trees_pollen_prod %>% 
#   filter(!is.na(pol_mean)) %>% 
#   filter(Genus == "Morus" ) %>% 
#   ggplot(aes(x = tree_area , y = tree_BA  , fill = Species)) + geom_point() + facet_wrap(~Species, scales = "free") + theme_bw() 

### weight pollen production of individuals by relative fraction in landscape #######################################
# EXPNS is the number of acres per plot within a stratum within a city

city_EXPNS <- pcv %>% 
  group_by(city) %>% 
  summarize(mean_EXPNS = mean(EXPNS)) #calculate the mean acres per plot across all plots in the city

pcv <- left_join(pcv, city_EXPNS)

# double check that my interpretation is correct
#   pcv %>% 
#     filter(city == "Burlington") %>% 
#     select(city, STRATUM_ACRES, ESTN_UNIT_ACRES, EXPNS, STRATUM_PLOT_COUNT, POPULATION_PLOT_COUNT, n_plots,  mean_EXPNS) %>% 
#     mutate(stratum_portion_of_city = STRATUM_ACRES/ESTN_UNIT_ACRES,
#            plots_portion = STRATUM_PLOT_COUNT/POPULATION_PLOT_COUNT,
#            expansion_recalculate = STRATUM_ACRES/STRATUM_PLOT_COUNT,
#            plot_EXPNS = EXPNS/mean_EXPNS) %>% 
#     head(.)
  
### statistics for paper ################################################

#summing across taxa, which cities have the highest pollen production per land area
  pcv %>% 
    dplyr::group_by(city, n_plots, Genus) %>% 
    mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS),    #accounting for weighting by stratum
           tree_area_expns = tree_area * (EXPNS/mean_EXPNS) ) %>%   #accounting for weighting by stratum
    dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE), #sum pollen in billions of grains per genus in each city
                     tree_can_area_sum = sum(tree_area_expns, na.rm = TRUE),
                     tree_can_area_mean = mean(tree_area, na.rm = TRUE),
                     n_trees = n()) %>% 
    mutate(pol_sum_m2 = pol_sum /(672.4535 * n_plots)) %>%  #total pollen per genus (in billions) in that city by m2 measured #672.4535 is plot area in m2
    group_by(city) %>% 
    summarize(pol_sum_m2_mean = sum(pol_sum_m2)) %>% #sum across all taxa
    mutate(pol_sum_m2_mean_not_bil = pol_sum_m2_mean * 10^9,#convert back from billions
           pol_sum_m2_mean_mil = pol_sum_m2_mean_not_bil/ 1000000) %>% 
    arrange(-pol_sum_m2_mean_not_bil) 
  
      #comparing to a version that isn't corrected for weighting by stratum area
        # pcv %>% 
        #   dplyr::group_by(city, n_plots, Genus) %>% 
        #   mutate(pol_mean_expns = pol_mean ,   
        #          tree_area_expns = tree_area ) %>%   
        #   dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE), #sum pollen in billions of grains per genus in each city
        #                    tree_can_area_sum = sum(tree_area_expns, na.rm = TRUE),
        #                    tree_can_area_mean = mean(tree_area, na.rm = TRUE),
        #                    n_trees = n()) %>% 
        #   mutate(pol_sum_m2 = pol_sum /(672.4535 * n_plots)) %>%  #total pollen per genus (in billions) in that city by m2 measured #672.4535 is plot area in m2
        #   group_by(city) %>% 
        #   summarize(pol_sum_m2_mean = sum(pol_sum_m2)) %>% #sum across all taxa
        #   mutate(pol_sum_m2_mean_not_bil = pol_sum_m2_mean * 10^9,#convert back from billions
        #          pol_sum_m2_mean_mil = pol_sum_m2_mean_not_bil/ 1000000) %>% 
        #   arrange(-pol_sum_m2_mean_not_bil) 



      
    # #average pollen production across cities
    # ungroup() %>% 
    # summarize(pol_sum_m2_mean_allcities = mean(pol_sum_m2_mean))

#summing across taxa, which cities have the highest pollen production per canopy area
#test <-
pcv %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS),    #accounting for weighting by stratum
         tree_area_expns = tree_area * (EXPNS/mean_EXPNS) ) %>%   #accounting for weighting by stratum
  dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE), #sum pollen in billions of grains per genus in each city
                   tree_can_area_sum = sum(tree_area_expns, na.rm = TRUE)) %>% 
  mutate(pol_sum_m2_can = pol_sum / tree_can_area_sum) %>%  #total pollen per genus (in billions) in that city by m2 measured #672.4535 is plot area in m2
 
  #sum across all taxa within each city
  group_by(city) %>% 
  summarize(pol_sum = sum(pol_sum),
            area_sum = sum(tree_can_area_sum)) %>% 
  mutate( pol_per_m2_across_city_bil = pol_sum/area_sum,
          pol_sum_m2_mean_not_bil = pol_per_m2_across_city_bil * 10^9,#convert back from billions
         pol_sum_m2_mean_mil = pol_sum_m2_mean_not_bil/ 1000000) %>% 
  arrange(-pol_sum_m2_mean_not_bil) 

# ca_per_city <- pcv %>% 
#   mutate(ca_expns = tree_area * EXPNS) %>% 
#   dplyr::group_by(city) %>% 
#   dplyr::summarize(ca_sum_city = sum(tree_area, na.rm = TRUE))
# 
# pcv %>% 
#   dplyr::group_by(city, n_plots) %>% 
#   dplyr::summarize(pol_sum = sum(pol_mean, na.rm = TRUE)) %>% 
#   left_join(., ca_per_city) %>% 
#   mutate(pol_ca = pol_sum/ca_sum_city)%>% 
#   arrange(-pol_ca)  %>% print.data.frame()

# #summing across taxa, which cities have the highest pollen production per basal area
#   ba_per_city <- pc %>% 
#     mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
#             BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
#     dplyr::group_by(city) %>% 
#     dplyr::summarize(BA_sum_city = sum(BA_m2))
#   
#   pc %>% 
#     dplyr::group_by(city, n_plots) %>% 
#     dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
#     left_join(., ba_per_city) %>% 
#     mutate(pol_BA = pol_sum/BA_sum_city)%>% 
#     arrange(-pol_BA)  %>% print.data.frame()


#averaging across all cities, which taxa have the highest pollen production per m2 of all measured land area
  pcv %>% 
    group_by(city, n_plots, Genus) %>% 
    mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>%     #accounting for weighting by stratum
    summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE)) %>% 
    mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  
    group_by(Genus) %>%  
    
    #summarize by genus across all cities
    summarize(pol_sum_m2_mean = mean(pol_sum_m2),
              pol_sum_m2_sd = sd(pol_sum_m2)) %>% 
    arrange(-pol_sum_m2_mean) %>% 
    #ungroup() %>% summarize(pol_sum_m2_mean_sum = sum(pol_sum_m2_mean)) %>% 
    print.data.frame() 
  
  #what percent of known pollen production are the top 4 genera?
  #(0.057765664    + 0.049313470    + 0.016987478    + 0.008710561   )  /  0.162     

  
# ### what portion of total basal area are the taxa that we calculated pollen production for? #####
#   #basal area of pollen trees
#   ba_total_pollen_trees <- 
#     left_join( all_trees_pollen_prod, pcv_out) %>% 
#     filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
#     filter(STATUSCD == 1 | STATUSCD == 2) %>%  #removing trees that weren't measured due to no longer being in the sample
#     filter(city %in% NE_cities_not_eval) %>% 
#     mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
#             BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
#     filter(!is.na(pol_mean)) %>% 
#     group_by(city) %>% 
#     summarize(BA_m2_total_wind = sum(BA_m2, na.rm = TRUE))
#   
#   #basal area of all trees
#   ba_total_all_trees <- 
#     left_join( all_trees_pollen_prod, pcv_out) %>% 
#     filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
#     filter(STATUSCD == 1 | STATUSCD == 2) %>%  #removing trees that weren't measured due to no longer being in the sample
#     filter(city %in% NE_cities_not_eval) %>% 
#     mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
#             BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
#     group_by(city) %>% 
#     #filter(!is.na(pol_mean)) %>% 
#     summarize(BA_m2_total_alltrees = sum(BA_m2, na.rm = TRUE))
#   
#   left_join(ba_total_pollen_trees, ba_total_all_trees) %>% 
#     mutate(fraction_ba_pol = BA_m2_total_wind / BA_m2_total_alltrees) %>% 
#     ungroup() %>% 
#     summarize(mean_fract = mean(fraction_ba_pol))
#  # (ba_total_pollen_trees / ba_total_all_trees) * 100

### what portion of total canopy area are the taxa that we calculated pollen production for? #####
  #canopy area of trees that pollen production was estimated for
  ca_total_pollen_trees <- 
    pcv %>% 
    filter(!is.na(pol_mean)) %>% 
    group_by(city) %>% 
    summarize(ca_m2_total_wind = sum(tree_area, na.rm = TRUE))
  
  #canopy area of all trees
  ca_total_all_trees <- 
    pcv %>% 
    # filter(!is.na(pol_mean)) %>% 
    group_by(city) %>% 
    summarize(ca_m2_total_alltrees = sum(tree_area, na.rm = TRUE))
  
  left_join(ca_total_pollen_trees, ca_total_all_trees) %>% 
    mutate(fraction_ca_pol = ca_m2_total_wind / ca_m2_total_alltrees) %>% 
    ungroup() %>% 
    summarize(mean_fract = mean(fraction_ca_pol))
  
### what portion of canopy area of wind-pollinated taxa are the taxa that we calculated pollen production for? #######
  #total canopy area across all focal cities
    ca_total_all_trees_not_by_city <- 
    ca_total_all_trees %>% 
    ungroup() %>% 
    summarize(ca = sum(ca_m2_total_alltrees))
  
  #relative ca of particular genera
    ca_total_by_genus <- 
      pcv  %>% 
      group_by(GENUS) %>% 
      #filter(!is.na(pol_mean)) %>% 
      summarize(ca_m2_total_alltrees = sum(tree_area, na.rm = TRUE)) %>% 
      rowwise() %>% 
      mutate(ca_m2_total_alltrees_rel = 100 * (ca_m2_total_alltrees/ca_total_all_trees_not_by_city))
    
  #percent of wind pollinated taxa that we included
    windpol_taxa <- c("Acer","Betula", "Carpinus", "Carya", "Cercis", "Corylus", "Fagus", "Fraxinus", "Ginkgo", "Gleditsia", "Juglans", "Juniperus",
                      "Larix", "Metasequoia", "Morus", "Ostrya", "Picea", "Pinus", "Platanus", "Populus", "Pseudotsuga", "Quercus", "Salix",
                      "Taxus", "Thuja", "Tsuga", "Ulmus", "Zelkova")
    # windpol_taxa_subset <- c("Betula", "Carpinus", "Carya", "Cercis", "Corylus", "Fagus",  "Ginkgo", "Gleditsia", "Juglans", "Juniperus",
    #                   "Metasequoia", "Morus", "Ostrya", "Platanus", "Populus", "Quercus", "Salix",
    #                   "Taxus", "Thuja", "Ulmus", "Zelkova")
    
    taxa_we_include <- c("Acer","Betula", "Gleditsia", "Juglans", "Morus",  "Platanus", "Populus", "Quercus", "Ulmus")
    
    ca_total_of_our_focal_taxa <- 
     pcv %>% 
      filter(GENUS %in% taxa_we_include) %>% 
      summarize(ca_m2_total_focal_trees = sum(tree_area, na.rm = TRUE)) 
    
    ca_total_of_all_windpol_taxa <- 
     pcv %>% 
      filter(GENUS %in% windpol_taxa) %>% 
      summarize(ca_m2_total_windpol_trees = sum(tree_area, na.rm = TRUE)) 
    
    #what fraction of total canopy area do our pollen production estimates account for?
    ca_total_of_our_focal_taxa$ca_m2_total_focal_trees / ca_total_all_trees_not_by_city$ca#
  
   #what fraction of total wind-pollinated canopy area do our pollen production estimates account for?
    ca_total_of_our_focal_taxa$ca_m2_total_focal_trees / ca_total_of_all_windpol_taxa$ca_m2_total_windpol_trees
    
  
  # #what portion of basal area of wind-pollinated taxa are the taxa that we calculated pollen production for?
  # #total BA across all focal cities
  #   ba_total_all_trees_not_by_city <- 
  #   ba_total_all_trees %>% 
  #   ungroup() %>% 
  #   summarize(BA = sum(BA_m2_total_alltrees))
  # 
  # #relative BA of particular genera
  #   ba_total_by_genus <- 
  #     left_join( all_trees_pollen_prod, pcv_out) %>% 
  #     filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
  #     filter(STATUSCD == 1 | STATUSCD == 2) %>%  #removing trees that weren't measured due to no longer being in the sample
  #     filter(city %in% NE_cities_not_eval) %>% 
  #     mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
  #             BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
  #     group_by(GENUS) %>% 
  #     #filter(!is.na(pol_mean)) %>% 
  #     summarize(BA_m2_total_alltrees = sum(BA_m2, na.rm = TRUE)) %>% 
  #     rowwise() %>% 
  #     mutate(BA_m2_total_alltrees_rel = 100 * (BA_m2_total_alltrees/ba_total_all_trees_not_by_city))
  #   
  # #percent of wind pollinated taxa that we included
  #   windpol_taxa <- c("Betula", "Carpinus", "Carya", "Cercis", "Corylus", "Fagus", "Fraxinus", "Ginkgo", "Gleditsia", "Juglans", "Juniperus",
  #                     "Larix", "Metasequoia", "Morus", "Ostrya", "Picea", "Pinus", "Platanus", "Populus", "Pseudotsuga", "Quercus", "Salix",
  #                     "Taxus", "Thuja", "Tsuga", "Ulmus", "Zelkova")
  #   windpol_taxa_subset <- c("Betula", "Carpinus", "Carya", "Cercis", "Corylus", "Fagus",  "Ginkgo", "Gleditsia", "Juglans", "Juniperus",
  #                     "Metasequoia", "Morus", "Ostrya", "Platanus", "Populus", "Quercus", "Salix",
  #                     "Taxus", "Thuja", "Ulmus", "Zelkova")
  #   
  #   taxa_we_include <- c("Betula", "Carya", "Gleditsia", "Juglans", "Morus",  "Platanus", "Populus", "Quercus", "Ulmus", "Zelkova")
  #   
  #   ba_total_by_genus_all_windpol <- 
  #     left_join( all_trees_pollen_prod, pcv_out) %>% 
  #     filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
  #     filter(STATUSCD == 1 | STATUSCD == 2) %>%  #removing trees that weren't measured due to no longer being in the sample
  #     filter(city %in% NE_cities_not_eval) %>% 
  #     mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
  #             BA_m2 = (pi * dbh_cm^2)/1000) %>%  #calculate area of circle in cm2 and then convert to m2 
  #     filter(GENUS %in% windpol_taxa_subset) %>% 
  #   #  filter(!is.na(pol_mean)) %>% 
  #     summarize(BA_m2_total_alltrees = sum(BA_m2, na.rm = TRUE)) 
  #   
  #   3372.992/5166.075
  #   3372.992/3773.06
    
#For each city, what proportion of total pollen produced is from planted vs unplanted trees
  pcv %>% #names(pcv)
  mutate(trees_planted_label = case_when(trees_planted == 0 ~ "natural",
                                         trees_planted == 1 ~ "planted",
                                         is.na(trees_planted) ~ "unknown"),
         pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>%     #accounting for weighting by stratum
  group_by(city, trees_planted_label) %>% 
  summarize(pol_sum =sum(pol_mean_expns, na.rm = TRUE)) %>% 
  pivot_wider(names_from = trees_planted_label, values_from = pol_sum) %>% 
  mutate(p_n_ratio = planted / (natural + planted)) %>% 
  arrange(p_n_ratio) %>% 
  ungroup() #%>% 
  
  # #calculate the mean across all cities
  # summarize(mean_p_n_ratio = mean(p_n_ratio))

#for each genus, what proportion of pollen is from natural vs planted trees
  pcv %>% #names(pcv)
    mutate(trees_planted_label = case_when(trees_planted == 0 ~ "natural",
                                           trees_planted == 1 ~ "planted",
                                           is.na(trees_planted) ~ "unknown"),
           pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>%     #accounting for weighting by stratum
    group_by(Genus, trees_planted_label) %>% 
    summarize(pol_sum =sum(pol_mean_expns, na.rm = TRUE)) %>% 
    pivot_wider(names_from = trees_planted_label, values_from = pol_sum) %>% 
    mutate(p_n_ratio = planted / (natural + planted)) %>% 
    arrange(p_n_ratio) %>% 
    ungroup() #%>% 
  


#for each genus, what proportion of pollen is from street vs nonstreet trees
pcv %>%
  mutate(street_tree_label = case_when(street_tree == 0 ~ "not_a_street_tree",
                                       street_tree == 1 ~ "street_tree"),
         pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  group_by(Genus, street_tree_label) %>% 
  summarize(pol_sum =sum(pol_mean_expns, na.rm = TRUE)) %>% 
  pivot_wider(names_from = street_tree_label, values_from = pol_sum) %>% 
  mutate(s_n_ratio = street_tree / (not_a_street_tree + street_tree)) %>% 
  arrange(s_n_ratio) %>% 
  ungroup() #%>% 
      # #calculate the mean across all cities
      # summarize(mean_p_n_ratio = mean(p_n_ratio))

 #across cities, what proportion of pollen is from street vs nonstreet trees
 pcv %>%
   mutate(street_tree_label = case_when(street_tree == 0 ~ "not_a_street_tree",
                                        street_tree == 1 ~ "street_tree"),
          pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
   group_by(city, street_tree_label) %>% 
   summarize(pol_sum =sum(pol_mean_expns, na.rm = TRUE)) %>% 
   pivot_wider(names_from = street_tree_label, values_from = pol_sum) %>% 
   mutate(s_n_ratio = street_tree / (not_a_street_tree + street_tree)) %>% 
   arrange(s_n_ratio) %>% 
   ungroup() #%>% 
     #calculate the mean across all cities
     # summarize(mean_p_n_ratio = mean(p_n_ratio))
 
 
# how much of pollen production is from platanus in a couple of particular cities
 pcv %>%
   filter(city == "Rochester" | city == "Cleveland") %>%
   filter(Genus == "Platanus") %>% 
   mutate(street_tree_label = case_when(street_tree == 0 ~ "not_a_street_tree",
                                        street_tree == 1 ~ "street_tree"),
          pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
   group_by(city, street_tree_label) %>% 
   summarize(pol_sum =sum(pol_mean_expns, na.rm = TRUE)) 
   
   
#hard coded proportion pollen production by Platanus street trees in Cleveland and Rochester
  3042 / 11264 #portion of pollen production in Cleveland from Platanus street trees
  2107 / 11300 #Rochester Platanus


  


### Fig 1 pollen production per genus ###################################
fig1a <- pcv %>% 
    filter(pol_mean > 0) %>% 
    mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean_expns)) %>% #in billions of pollen grains
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  # Plot area in m2 = 672.4535
  ggplot(aes(x = city, y = pol_sum_m2, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen per land area (billion grains /"~~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy")


ca_per_city <- 
  pcv %>% 
  dplyr::group_by(city) %>% 
  mutate(tree_area_expns = tree_area * (EXPNS/mean_EXPNS)) %>% 
  dplyr::summarize(ca_sum_city = sum(tree_area_expns, na.rm = TRUE))

fig1b <- pcv %>% 
  filter(pol_mean > 0) %>% 
  mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, n_plots, Genus) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE)) %>% 
  left_join(., ca_per_city) %>% 
  mutate(pol_ca = pol_sum/ca_sum_city) %>% 
  ggplot(aes(x = city, y = pol_ca, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen per canopy area (billion grains /"~~m^2~")" )) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy")

cowplot::plot_grid(fig1a, fig1b, nrow = 2)

#significance for fig 1a
fig_1a_aov <- pcv %>% 
  mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, PLT_CN) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE)) %>% 
    aov( pol_sum ~ city, data = .)
summary(fig_1a_aov)

#significance for fig 1b
fig_1b_aov <- pcv %>% 
  mutate(pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS),
         tree_area_expns = tree_area * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, PLT_CN) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE),
                   ca_sum = sum(tree_area_expns )) %>% 
  mutate(pol_per_ca = pol_sum/ca_sum) %>% 
  aov( pol_per_ca ~ city, data = .)
summary(fig_1b_aov)



### Fig 2: canopy area and basal area by city and genus #################################
#get the mean expansion factor per city
city_can_area <- pcv %>%
  group_by(city) %>%
  mutate(tree_area_expns = tree_area * (EXPNS/mean_EXPNS)) %>%
  summarize(ca_sum_city = sum(tree_area_expns, na.rm = TRUE),
            mean_EXPNS = mean(mean_EXPNS))

## calculate the number of plots in the city in that census (including plots without trees)  
plots_per_city <-
  psc_psca %>% 
  filter(STRATUM_LABEL != "Water") %>% #removing plots that are in open water
  group_by(POPULATION_NAME) %>% 
  summarize(n_plots = n())

#test <- 
  left_join(mtre_plt, psc_psca, by = c("PLT_CN" = "psca_PLT_CN")) %>% 
  left_join(., plots_per_city) %>% 
    filter(!is.na(EVALID)) %>% 
    filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
    filter(STATUSCD == 1 | STATUSCD == 2) %>% #removing trees that weren't measured due to no longer being in the sample
    #STATUSCD 0 == tree is not in the remeasured plot, STATUSCD 3 == cut and utilized, STATUSCD 4 == removed
    filter(STRATUM_LABEL != "Water") %>%  #removing plots that are in open water
  mutate(city_state = gsub(pattern = "City of ", replacement = "", x = ESTN_UNIT_NAME), 
         city = stri_sub(city_state, 1, -5), #remove state from city name for figures
         city = case_when(city == "Portland, ME Urban Area:Portland" ~ "Portland, ME",
                          city == "Trenton, NJ Urban " ~ "Trenton",
                          .default = city),
         city_area_ha = ESTN_UNIT_ACRES/2.471, #convert from acres to ha
         Genus = GENUS,
         Species = gsub("^\\S+ ", "", SPECIES),
         dbh_cm = DIA *2.54,
         tree_BA = 0.00007854 * dbh_cm^2,
         tree_can_diam_ft = case_when( !is.na(CROWN_DIA_WIDE) ~  (CROWN_DIA_WIDE + CROWN_DIA_90)/2,
                                       is.na(CROWN_DIA_WIDE) ~  AVG_CROWN_WIDTH), #use modeled width when diameter wasn't measured
         tree_can_diam_m = tree_can_diam_ft * 0.3048,
         tree_can_radius_m = tree_can_diam_m/2,
         tree_area = pi * (tree_can_radius_m^2),
         plot_area = 672.4535) %>%  #convert crown radius in m to m2
    left_join(., city_can_area) %>% 
    mutate( tree_area_expns = tree_area * (EXPNS/mean_EXPNS),
            plot_area_expns = n_plots * 672.4535 * (EXPNS/mean_EXPNS)) %>%
  group_by(city, Genus) %>%  #PLT_CN
   summarize(ca_sum = sum(tree_area_expns, na.rm = TRUE),
             plot_area_measured = mean(plot_area_expns, na.rm = TRUE)) %>% 
  mutate(
    prop_ca = ca_sum / plot_area_measured,
    perc_ca = prop_ca * 100,
    Genus = case_when(prop_ca < .02 ~ "other",
                      prop_ca >= .02 ~ Genus)) %>%
  ggplot(aes(x = city, y = prop_ca, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  #ylab("canopy area (%)") +  
  ylab(bquote(canopy~area~per~land~area~(m^2~"/"~m^2))) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "light")

#ggsave("C:/Users/dsk273/Box/writing/UFIA pollen production/fig2_ca_by_city.pdf", height = 1500, width = 3000, units = "px")

      # ### Fig 2: basal area by city and genus #################################
      # pc_npp <- left_join( all_trees_pollen_prod, pcv_out) %>% 
      #   mutate(trees_alive = case_when(STATUSCD == 2 ~ 0, #STATUSCD 1 == live tree, STATUSCD 2 == dead tree
      #                                  STATUSCD == 1 ~ 1)) %>% 
      #   filter(SUBP == 1) %>%  #restricting to non-sapling trees (DBH > 5 in)
      #   filter(STATUSCD == 1 ) %>%  #removing trees that weren't measured due to no longer being in the sample
      #   filter(city %in% NE_cities_not_eval)
      # 
      # #currently this doesn't include plots without any trees nor strata corrections
      # area_per_city <- left_join( all_trees_pollen_prod, pcv_out) %>%  
      #   dplyr::group_by(city) %>% 
      #   dplyr::select(PLT_CN) %>% 
      #   dplyr::distinct() %>% 
      #   dplyr::count(city) %>% 
      #   dplyr::rename(n_plots = n) %>% 
      #   mutate(area_per_city = n_plots * 672.4535)  # Plot area in m2 = 672.4535)
      # 
      # BA_sum_city <- 
      #   pc_npp %>% 
      #   mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
      #           tree_BA_m2 = (pi * dbh_cm^2)/10000) %>%  
      #   dplyr::group_by(city) %>% 
      #   dplyr::summarize(
      #     BA_sum_city = sum(tree_BA_m2),
      #     n_trees_city = n())
      # 
      # #visualize results
      # pc_npp %>% 
      #   mutate( dbh_cm = DIA * 2.54 * 0.5, #convert diameter in inches to radius in cm
      #           tree_BA_m2 = (pi * dbh_cm^2)/10000) %>%  
      #   group_by(city, Genus) %>% 
      #   dplyr::summarize(BA_sum = sum(tree_BA_m2)) %>% 
      #   ungroup() %>% 
      #   left_join(., area_per_city) %>% 
      #   left_join(., BA_sum_city) %>% 
      #   mutate(Genus = case_when(BA_sum / BA_sum_city < .02 ~ "other",
      #                            BA_sum / BA_sum_city >= 0.02 ~ Genus),
      #          area_per_city_ha = area_per_city/10000) %>% 
      #   mutate(city = stri_sub(city, 1, -3),
      #          city = case_when(city == "Minneapol" ~ "Minneapolis", .default = city)) %>%
      #   ggplot(aes(x = city, y = BA_sum/area_per_city_ha, fill = Genus)) + geom_col() + ggthemes::theme_few() +
      #   ylab(bquote(basal~area~(m^2~"/"~ha))) + 
      #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
      #   grafify::scale_fill_grafify(palette = "light")
      # 
      # #ggsave("C:/Users/dsk273/Box/writing/UFIA pollen production/fig2_ba_by_city.pdf", height = 1500, width = 3000, units = "px")




### Fig 3: differences in pollen production between planted and natural trees ################
pcv %>%
  filter(pol_mean > 0) %>% 
  mutate(trees_planted_label = case_when(trees_planted == 0 ~ "natural",
                                         trees_planted == 1 ~ "planted",
                                         is.na(trees_planted) ~ "unknown"),
         pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  group_by(city, trees_planted_label, n_plots, Genus) %>% 
  summarize(pol_sum = sum(pol_mean_expns)) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>%  # Plot area in m2 = 672.4535
 
   ggplot(aes(x = city, y = pol_sum_m2, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen production (billion pollen grains /  "~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy") + facet_wrap(~trees_planted_label, ncol = 1)
  #ggsave("C:/Users/dsk273/Box/writing/UFIA pollen production/fig3_nat_vs_planted.pdf", height = 1500, width = 3000, units = "px")
  

#significance for fig 3
#test <-
pcv %>% 
  filter(pol_mean > 0) %>% 
  dplyr::group_by(city, trees_planted, PLT_CN) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  pivot_wider(names_from = trees_planted, values_from = pol_sum, names_prefix = "planted_") %>% 
  mutate(planted_0 = case_when(is.na(planted_0) ~ 0.1, .default = planted_0),
         planted_1 = case_when(is.na(planted_1) ~ 0.1, .default = planted_1)) %>% 
  mutate(planted_ratio = planted_1/ planted_0,
         planted_ratio_log = log(planted_ratio),
         planted_prop = planted_1 / (planted_0 + planted_1)) %>% 
  #ggplot(aes(x = city, y = planted_prop)) + geom_boxplot() + geom_point() 
  #aov( planted_prop ~ city, data = .)
  #summary(test)
  kruskal.test( planted_prop ~ city, data = .)
  


### Fig 4: differences between street trees and non-street trees #################
pcv %>%
  filter(pol_mean > 0) %>% 
  mutate(street_tree_label = case_when(street_tree == 0 ~ "not a street tree",
                                       street_tree == 1 ~ "street tree"),
         pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, street_tree_label, n_plots, Genus) %>%
  dplyr::summarize(pol_sum = sum(pol_mean_expns)) %>%  
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535)) %>% # Plot area in m2 = 672.4535
  ggplot(aes(x = city, y = pol_sum_m2, fill = Genus)) + geom_col() + ggthemes::theme_few() +
  ylab(bquote("pollen production (billion grains / "~m^2~")")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.text = element_text(face="italic")) +
  grafify::scale_fill_grafify(palette = "fishy") + facet_wrap(~street_tree_label, ncol = 1)


#significance for fig 4
#test <-
pcv %>% 
  dplyr::group_by(city, street_tree , PLT_CN) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean)) %>% 
  pivot_wider(names_from = street_tree , values_from = pol_sum, names_prefix = "street_") %>% 
  mutate(street_0 = case_when(is.na(street_0) ~ 0.1, .default = street_0),
         street_1 = case_when(is.na(street_1) ~ 0.1, .default = street_1)) %>% 
  mutate(street_ratio = street_1/ street_0,
         street_ratio_log = log(street_ratio),
         street_prop = street_1 / (street_0 + street_1)) %>% 
  #ggplot(aes(x = city, y = street_prop)) + geom_boxplot() + geom_point() 
  #aov( street_prop ~ city, data = .)
  #summary(test)
  kruskal.test( street_prop ~ city, data = .)






### SI 1: regression of pollen per area vs canopy area per area ################
fit <- 
  pcv %>% 
  mutate( pol_mean_expns = pol_mean * (EXPNS/mean_EXPNS),
          tree_area_expns = tree_area * (EXPNS/mean_EXPNS)) %>% 
  dplyr::group_by(city, n_plots) %>% 
  dplyr::summarize(pol_sum = sum(pol_mean_expns, na.rm = TRUE),
                   ca_sum = sum(tree_area_expns, na.rm = TRUE )) %>% 
  mutate(pol_sum_m2 = pol_sum/(n_plots * 672.4535), # Plot area in m2 = 672.4535
         ca_sum_m2= (ca_sum/(n_plots * 672.4535))) %>% #
 
   #uncomment this section to make SI X:
  # ggplot(aes(x = ca_sum_m2, y = pol_sum_m2)) + geom_point() + geom_smooth(method = "lm") +
  # ylab(bquote("pollen per land area (billion grains/"~m^2~")")) +  xlab(bquote(canopy~area~(m^2~"/"~m^2))) +  theme_bw()
  
  lm(pol_sum_m2 ~ ca_sum_m2, data = .)
summary(fit)  

