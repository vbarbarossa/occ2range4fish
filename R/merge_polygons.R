#Valerio Barbarossa, 24 Sep 2019
# this script creates a multipolygons dataset with
# IUCN and custom compiled fish ranges

library(valerioUtils)
libinv(c('dplyr','vroom','foreign','sf','rfishbase'))

HB_lev <- '08'

# set data directory
dir_data <- '../data/'

# CUSTOM RANGES-----------------------------------------------------------------------------------------------------
# files from ranges2HB script
range2HB_files <- list.files(path = paste0('proc/occurrence_records_on_hb',HB_lev,'_mollweide/'), full.names = T)

# compile table ranges
tab <- lapply(range2HB_files,readRDS) %>%
  do.call('rbind',.) %>%
  as_tibble() %>%
  distinct() %>%
  select(name,HYBAS_ID = hybas_id)

# load HB
HB <- lapply(c('af','ar','as','au','eu','gr','na','sa','si'),
             function(x) read_sf(paste0(dir_data,'HydroBASINS/global_lev',HB_lev,'/hybas_',x,'_lev',HB_lev,'_v1c.shp'))) %>%
  do.call('rbind',.)

custom_m <- right_join(HB,tab) %>% 
  group_by(name) %>%
  summarise(do_union=T) %>%
  mutate(db = 'custom') %>%
  select(name,db,geometry) %>%
  st_cast('MULTIPOLYGON')

# merge with occ count
count_occ <- readRDS('proc/compiled_occurrence_records.rds') %>%
  group_by(name) %>%
  summarize(no_occ = n())

custom_m <- left_join(custom_m,count_occ) %>%
  select(name,db,no_occ,geometry)

st_write(custom_m,paste0(dir_('out/'),'custom_ranges_poly.gpkg'))
write.csv(custom_m %>% as_tibble() %>% select(name,db,no_occ),
          paste0(dir_('out/'),'custom_ranges_poly.csv'),row.names = F)

# HABITAT TYPE FROM FISHBASE-----------------------------------------------------------------------------------------
options(FISHBASE_VERSION="19.04")

# retrieve list of freshwater fish species available from fishbase
habitat <- ecology(custom_m$name) %>%
  select(name = Species, Stream, Lake = Lakes) %>%
  distinct()

habitat$OnlyLake <- 0
habitat$OnlyLake[habitat$Lake == -1 & (habitat$Stream == 0 | is.na(habitat$Stream))] <- -1

write.csv(habitat,paste0(dir_('out/'),'custom_ranges_habitatFishbase.csv'),row.names = F)


# NOT REALLY NEEDED TO RESHAPE IUCN DATA, DEPRECATED
# # IUCN RANGES-----------------------------------------------------------------------------------------------------
# # load IUCN polygon ranges
# # not the hydrobasins tab as that is referenced on multiple HB levels and on the HB with lakes
# iucn <- lapply(1:2, #read_sf always returns a sf and tibble object
#                function(x) read_sf(paste0(dir_data,'IUCN/FW_FISH_20181113/FW_FISH_PART_',x,'.shp'))) %>%
#   do.call('rbind',.) %>%
#   filter(presence %in% c(1,2)) %>% # select only extant records
#   rename(name = binomial)#renames column for consistency with custom data
# 
# row_count <- iucn %>%
#   as_tibble() %>%
#   group_by(name) %>%
#   summarize(count = n())
# 
# iucn_nomerge <- iucn %>%
#   filter(name %in% row_count$name[row_count$count == 1]) %>%
#   select(name,geometry)
# 
# iucn_merge <- iucn %>%
#   filter(name %in% row_count$name[row_count$count > 1]) %>%
#   arrange(name) %>%
#   .[1:50,] %>%
#   lwgeom::st_make_valid() %>% #avoid TopologyException error by buffering
#   summarise(do_union = T) %>% #merges records of same species
#   st_cast('MULTIPOLYGON')
# 
# iucn_m <- row_bind(iucn_nomerge,iucn_merge) %>%
#   mutate(db = 'IUCN') %>%
#   select(name,db,geometry) %>%
#   st_cast('MULTIPOLYGON')
# 
#   
# st_write(iucn_m,paste0(dir_('out/'),'iucn_ranges_poly.gpkg'))
# 
# # MERGED RANGES--------------------------------------------------------------------------------------------------
# custom_diff <- custom_m %>%
#   filter(!name %in% iucn$name)
# 
# merged <- rbind(iucn,custom_diff)
# 
# st_write(merged,paste0(dir_('out/'),'merged_ranges_poly.gpkg'))



