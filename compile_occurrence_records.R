#Valerio Barbarossa
# stript that compiles the occurrence records from the different datasets
# ala.org.ay
pacman::p_load(tidyverse,vroom)

dir_data <- '/vol/milkunarc/vbarbarossa/data/fish_databases/'
system(paste0('ls ',dir_data,'splink.org'))

#ala
ala <- vroom(paste0(dir_data,'ala.org.au/Fishes-brief.csv'),delim = ',') %>%
  select(name = scientificName,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~3M occ
#7,358 names

#boldsystems

#fishnet2
fishnet <- vroom(paste0(dir_data,'fishnet2/fishnet2.csv')) %>%
  select(name = ScientificName,lon = Longitude,lat = Latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~4M occ
#62,747 names

#gbif
gbif <- vroom(paste0(dir_data,'gbif/actinopterygii.csv')) %>%
  select(name = species,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]
#~17M occ
#62,747 names

#portalbiodiversidade.icmbio.gov.br
bra <- vroom(paste0(dir_data,'portalbiodiversidade.icmbio.gov.br/portalbio_export_17-09-2019-10-21-11.csv')) %>%
  select(name = Especie,lon = Longitude,lat = Latitude) %>%
  .[.$lon != "Acesso Restrito" & .$name != "Sem Informações",]
#~80k occ
#2,701 names

#splink.org
splink <- vroom(paste0(dir_data,'splink.org/speciesLink_all_112728_20190917101630.txt')) %>%
  select(name = scientificname,lon = longitude,lat = latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),]

# make it as a separate script "species_and_synonyms"

# read species compiled by Tedesco et al. 2018 Scientific Data-----------------------------
# and use it as a reference to select freshwater fish species
ref_ted <- read.csv('/vol/milkunarc/vbarbarossa/data/Tedesco/Occurrence_Table.csv',sep=';')
ref_ted <- unique(c(
  gsub('\\.',' ',ref_ted$X6.Fishbase.Valid.Species.Name)
  ,gsub('\\.',' ',ref_ted$X2.Species.Name.in.Source)
))

library(rfishbase)
options(FISHBASE_VERSION="19.04")
# make a table with all possible synonyms to maximize overlap datasets
fb <- taxonomy() %>% # get all species available (vector)
  species(.) %>% # get species table for all species
  filter(Fresh == -1) #select only freshwater
ref_fishbase <- unique(fb$Species)

fb_syn = synonyms(c(ref_fishbase,ref_ted)) %>%
  select(name = Species, synonym = synonym)
# NAs in name and not in synonym.. something is wrong, maybe better do it the other way

