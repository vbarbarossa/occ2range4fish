pacman::p_load(vroom, tidyverse)

# read species compiled by Tedesco et al. 2018 Scientific Data-----------------------------
# and use it as a reference to select freshwater fish species
ref_ted <- read.csv('/vol/milkunarc/vbarbarossa/data/Tedesco/Occurrence_Table.csv',sep=';')
ref_ted <- unique(c(
  gsub('\\.',' ',ref_ted$X6.Fishbase.Valid.Species.Name)
  # ,gsub('\\.',' ',ref_ted$X2.Species.Name.in.Source)
  ))

# species available from fishbase
library(rfishbase)
options(FISHBASE_VERSION="19.04")
# sl <- species_list(Class = 'Actinopterygii')
# get metadata about freshwater or not

fishbase <- taxonomy() %>% # get all species available (vector)
  species(.) %>% # get species table for all species
  filter(Fresh == -1) #select only freshwater
ref_fishbase <- unique(fishbase$Species)

# species shared between tedesco and fishbase
sum(ref_fishbase %in% ref_ted) #12207

# species in fishbase and not in tedesco
sum(!ref_fishbase %in% ref_ted) #5002
# the other way around
sum(!ref_ted %in% ref_fishbase) #2821
# quite a lot..


# read gbif raw data, all actinopterygii species available---------------------------------
# use vroom package for faster reading and indexing
# raw <- vroom('/vol/milkunarc/vbarbarossa/data/gbif/tedesco_on_gbif.csv') %>%
#   filter(!is.na(decimalLatitude))
# ~5400 from tedesco_on_gbif.csv in ref_ted

# actinopterygii were already requested only with latlong
raw <- vroom('/vol/milkunarc/vbarbarossa/data/gbif/actinopterygii.csv')

# gbif <- raw %>%
#   filter(species %in% ref_fishbase)
# 
# # species in fishbase database found in gbif:
# length(unique(raw$species))
# length(unique(gbif$species))
# # ~5,000 from actinopterygii.csv (synonyms were only ~100)

# combining fishbase and tedesco names
gbif <- raw %>%
  filter(species %in% unique(c(ref_fishbase,ref_ted)))

# species in Tedesco and fishbase database found in gbif:
length(unique(raw$species))
length(unique(gbif$species))
# 6371, largest overlap (without considering synonyms)


# compare to species within IUCN------------------------------------------------------------
# load IUCN data
iucn <- map_dfr(1:2,function(x) foreign::read.dbf(paste0('/vol/milkunarc/vbarbarossa/data/IUCN/FW_FISH_20181113/FW_FISH_PART_',x,'.dbf')))
ref_iucn <- unique(iucn$binomial) #7242
# validate the iucn names against fishbase data
ref_iucn <- unique(validate_names(ref_iucn)) #7083 records left --> ~200 were synonyms
ref_gbif <- unique(gbif$species)
ref_raw <- unique(raw$species)

# how many additional species are available in gbif
length(ref_gbif) - sum(ref_gbif %in% ref_iucn)#4352 species 
# and how many species are shared between IUCN and gbif
sum(ref_iucn %in% ref_gbif) #2019

# do a rough check whther there are a lot of species for south amwerica in gbif
sa <- gbif %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude),decimalLongitude = as.numeric(decimalLongitude)) %>%
  filter(decimalLatitude > -58 & decimalLatitude < 15 & decimalLongitude > -90 & decimalLongitude < -30)
# no records
nrow(sa) #41166
# no species
length(unique(sa$species)) #2568

# and how many of these are found in the iucn database
sum(unique(sa$species) %in% ref_iucn) # only 297


# # check whether there are a lot of non-actinopterygii species in Tedesco data
# # use gbif API
# library(rgbif)
# pacman::p_load(tidyverse)
# classes = unique(do.call('c',map(ref_names,function(x) unique(rgbif::name_usage(name=x)$data$class))))




