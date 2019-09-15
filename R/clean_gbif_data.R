# read species compiled by Tedesco et al. 2018 Scientific Data
# and use it as a reference to select freshwater fish species
ref_ted <- read.csv('/vol/milkunarc/vbarbarossa/data/Tedesco/Occurrence_Table.csv',sep=';')
ref_names <- unique(c(
  gsub('\\.',' ',ref_ted$X6.Fishbase.Valid.Species.Name)
  # ,gsub('\\.',' ',ref_ted$X2.Species.Name.in.Source)
  ))

# read gbif raw data, all actinopterygii species available
# use vroom package for faster reading and indexing
pacman::p_load(vroom, dplyr)
raw <- vroom('/vol/milkunarc/vbarbarossa/data/gbif/tedesco_on_gbif.csv') %>%
  filter(species %in% ref_names)

# species in Tedesco database found in gbif:
length(unique(raw$species))
# including also synonyms:
# 5840 (~50%) from actinopterygii.csv (synonyms were only ~100)
# 6844 from tedesco_on_gbif.csv, 6747 without synonyms

# check whether there are a lot of non-actinopterygii species in Tedesco data
# use gbif API
library(rgbif)
pacman::p_load(tidyverse)
classes = unique(do.call('c',map(ref_names,function(x) unique(rgbif::name_usage(name=x)$data$class))))




