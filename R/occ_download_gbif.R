
# read species compiled by Tedesco et al. 2018 Scientific Data
# and use it as a reference to select freshwater fish species
ref_ted <- read.csv('/vol/milkunarc/vbarbarossa/data/Tedesco/Occurrence_Table.csv',sep=';')
ref_names <- unique(c(
  gsub('\\.',' ',ref_ted$X6.Fishbase.Valid.Species.Name),
  gsub('\\.',' ',ref_ted$X2.Species.Name.in.Source)
))

pacman::p_load(rgbif,tidyverse)
# get keys of species in Tedesco
keys <- unique(do.call('c',map(ref_names, function(x) name_backbone(name = x)$usageKey)))
# downloa request that will be available in gbif profile
occ_download(paste0('taxonKey=',paste(keys,collapse = ',')), format = "SIMPLE_CSV",
             user = 'vbarbarossa', pwd = '1Rocker..11', email = 'v.barbarossa@fnwi.ru.nl')

# need to say only those with location, also there are more names in fishbase not included in tedesco


