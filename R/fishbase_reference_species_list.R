# Valerio Barbarossa, 19 Sep 2019
# this script generates a table of freshwater fish binomial names and synonyms 
# for two datasets
# fishbase
# Tedesco et al. 2018 Scientific Data

# load packages
pacman::p_load(dplyr,purrr,rfishbase,valerioUtils)

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
ted <- read.csv('/vol/milkunarc/vbarbarossa/data/Tedesco/Occurrence_Table.csv',sep=';') %>%
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

# check with IUCN names
library(rredlist)
token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'

tab <- fb_syn

# slower than mcmapply
# library(furrr)
# st <- Sys.time()
# plan(multiprocess)
# iucn <- future_map_dfr(1:20,function(i) {
#   syn <- rl_synonyms(tab$name[i],key=token)$result
#   if(length(syn) > 0){
#     syn$name_src = tab$name[i]
#     res <- syn %>%
#       select(id_iucn = accepted_id, 
#              name_iucn = accepted_name, 
#              name_iucn_synonym = synonym, 
#              name_src = name_src)
#     
#   }else{
#     res <- data.frame(
#       id_iucn = NA, 
#       name_iucn = NA, 
#       name_iucn_synonym = NA, 
#       name_src = tab$name[i])
#   }
#   return(res)
# },.progress=T)
# Sys.time() - st
# nrow(iucn)

st <- Sys.time()
iucn <- parallel::mcmapply(function(i) {
  syn <- rl_synonyms(tab$name[i],key=token)$result
  if(length(syn) > 0){
    syn$name_src = tab$name[i]
    res <- syn %>%
      select(id_iucn = accepted_id, 
             name_iucn = accepted_name, 
             name_iucn_synonym = synonym, 
             name_src = name_src)
  }else{
    res <- data.frame(
      id_iucn = NA, 
      name_iucn = NA, 
      name_iucn_synonym = NA, 
      name_src = tab$name[i])
  }
  return(res)
},1:100,SIMPLIFY = FALSE,mc.cores = 10) %>% do.call('rbind',.)
Sys.time() - st
nrow(iucn)
