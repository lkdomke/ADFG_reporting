#' Deanna with the SGWD project requested fish species for the eDNA analysis.
#' To do this I'm going to include just the sites were DNA samples took place (+Refugio just case theres wasting disease info there)
#' I'm going to first just include the info from 2019 and then add in 2017 - 2020
#' 
#' First load in the data:
#' 
fish19 <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A347cd81e-15a2-41f9-a73b-e344615548bf"), 
                   stringsAsFactors=FALSE, header = TRUE)
fish18 <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A0cae20e4-8f8e-427e-b7b9-5d8dd7111ec0"), 
                   stringsAsFactors=FALSE, header = TRUE)
fish17 <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A8dba01ba-ebec-4b45-afe5-391947abcc6e"),
                   stringsAsFactors=FALSE, header = TRUE)
fish20 <- read.csv("Data/fish_beach_seine_2020_RAW_unchecked.csv", stringsAsFactors=FALSE, header = TRUE)


# libraries
library(tidyverse)

# list of sites
# AK_A : NFE : NFEI_A
# AK_B : Guktu : GUKT_A
# AK_C : Nossuk : NOSK_A
# AK_D : Natzuhini : NATZ_A
# AK_E : Shinaku : SHIN_A
# AK_F : Refugio : REFU_A

# create a list of the bay_code/sample
sites <- c("NFEI_A", "GUKT_A", "NOSK_E", "NATZ_C", "SHIN_A", "REFU_A")
sites20 <- c("NFEI_A", "GUKT_B", "NOSK_E", "NATZ_C", "SHIN_A", "REFU_A")
sgwd_sites <- c("AK_A", "AK_B", "AK_C", "AK_D", "AK_E", "AK_F")
allsites <- data.frame(sites, sites20, sgwd_sites)

head(fish19)

fish.eg19 <- fish19 %>%
  filter(habitat == "eelgrass") %>%
  unite(universal_code, bay_code:bay_sample) %>%
  subset(universal_code %in% allsites$sites)

fish.eg19$abundance <- as.numeric(ifelse(is.na(fish.eg19$fork_length), paste(fish.eg19$unmeasured), 1))

fish_sp19 <- fish.eg19 %>%
  group_by(place_name, universal_code, latitude, longitude, date, species_common, species_scientific) %>%
  dplyr::summarise(total = sum(abundance)) %>%
  left_join(allsites, by = c("universal_code" = "sites")) %>%
  ungroup() %>%
  dplyr::select(-c(universal_code))

unique19sp <- fish_sp19 %>%
  dplyr::select(species_common) %>%
  distinct()
# unique species from all sites 2017 - 2020
fish <- bind_rows(fish17, fish18, fish19)

# fix 2020 so that we can combine datafiles
fish20.red <- fish20 %>%
  dplyr::select(bay_code, bay_sample, place_name, habitat, date, start_time, end_time, slope, tide_height, 
                tide_time, species_common, species_scientific, taxon, fork_length, unmeasured_sm)

# drop some columns from fish
fish <- fish %>%
  dplyr::select(-c(mass_g, julian, year, latitude, longitude))

all_fish <- fish %>%
  left_join(fish20.red) %>%
  filter(habitat == "eelgrass") %>%
  filter(taxon == "Vertebrata")

fish_sp <- all_fish %>%
  dplyr::select(species_common, species_scientific) %>%
  distinct()

# lets also quickly look at sgwd sites in 2020 and what fish were caught there
# get rid of an extra mistake row
fish20[-1423,]

# make new abundance column 
fish20 <- fish20 %>%
  unite(unmeasured, unmeasured:unmeasured_sm, na.rm = TRUE, remove = FALSE)

fish20$abundance <- as.numeric(ifelse(is.na(fish20$fork_length), paste(fish20$unmeasured), 1))

fish_sp20 <- fish20 %>%
  unite(universal_code, bay_code:bay_sample) %>%
  filter(taxon == "vertebrate") %>%
  group_by(place_name, universal_code, date, species_common, species_scientific) %>%
  dplyr::summarise(total = sum(abundance)) %>%
  left_join(allsites, by = c("universal_code" = "sites20")) %>%
  ungroup() %>%
  dplyr::select(-c(universal_code))


# read out the csvs we're interested in
# write.csv(fish_sp19, "Data/Species_by_site_sgwd_2019.csv")
# write.csv(fish_sp, "Data/Unique_species_allyears.csv")
# write.csv(fish_sp20, "Data/Species_by_site_sgwd_2020.csv")
