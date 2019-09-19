# Valerio Barbarossa, 19 Sep 2019
# this script generates a table of freshwater fish binomial names and synonyms 
# for two datasets
# fishbase
# Tedesco et al. 2018 Scientific Data

# load packages
library(valerioUtils)
libinv(c('dplyr','purrr','rfishbase'))

# read species in fishbase-----------------------------------------------------------------
# set fishbase database version (latest available)
options(FISHBASE_VERSION="19.04")

# retrieve list of freshwater fish species available from fishbase
fb <- taxonomy() %>% # get all species available (vector)
  species(.) %>% # get species table for all species
  filter(Fresh == -1) #select only freshwater

# compile a list of names and associated synonyms
fb_syn <- unique(fb$Species) %>% 
  synonyms() %>%
  select(name = synonym, name_synonym = Species) %>%
  distinct() # removes duplicated rows

# read species compiled by Tedesco et al. 2018 Scientific Data-----------------------------
ted <- read.csv('../data/Tedesco/Occurrence_Table.csv',sep=';') %>%
  select(name = X6.Fishbase.Valid.Species.Name,
         name_synonym = X2.Species.Name.in.Source) %>%
  mutate(name = gsub('\\.',' ',name),name_synonym = gsub('\\.',' ',name)) %>% #fix sep names
  distinct() # removes duplicated rows
# combine the sources----------------------------------------------------------------------
fb_syn$dataset = 'fb'
ted$dataset = 'ted'

tab <- rbind(fb_syn,ted) %>%
  as_tibble() %>%
  distinct(name,name_synonym,.keep_all = T) %>%
  arrange(name)

# write tables-----------------------------------------------------------------------------
dir_out <- dir_('proc/')

write.csv(fb_syn,paste0(dir_out,'names_fishbase.csv'),row.names = F)
write.csv(ted,paste0(dir_out,'names_tedesco.csv'),row.names = F)
write.csv(tab,paste0(dir_out,'names_fishbase_and_tedesco.csv'),row.names = F)

# # could further check synonyms with taxize
# library(taxize)
# taxize::gnr_resolve()
# taxize::synonyms()

