#Valerio Barbarossa, 24 Sep 2019
# this script creates a multipolygons dataset with
# IUCN and custom compiled fish ranges

library(valerioUtils)
libinv(c('dplyr','vroom','foreign','sf'))

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

  
st_write(custom_m,paste0(dir_('out/'),'custom_ranges_poly.gpkg'))

# IUCN RANGES-----------------------------------------------------------------------------------------------------
# load IUCN polygon ranges
# not the hydrobasins tab as that is referenced on multiple HB levels and on the HB with lakes
iucn <- lapply(1:2, #read_sf always returns a sf and tibble object
               function(x) read_sf(paste0(dir_data,'IUCN/FW_FISH_20181113/FW_FISH_PART_',x,'.shp'))) %>%
  do.call('rbind',.) %>%
  filter(presence %in% c(1,2))# select only extant records

#<<<<<<<<<<<<<<<<<<<< need to merge only doubled poly, exclude others otherwise takes too long
iucn_m <- iucn[100:300,] %>% 
  rename(name = binomial) %>% #renames column for consistency with custom data
  lwgeom::st_make_valid() %>% #avoid TopologyException error by buffering
  group_by(name) %>% #groups by name for merging
  summarise(do_union = T) %>% #merges records of same species
  mutate(db = 'IUCN') %>%
  select(name,db,geometry) %>%
  st_cast('MULTIPOLYGON')

st_write(iucn,paste0(dir_('out/'),'iucn_ranges_poly.gpkg'))

# MERGED RANGES--------------------------------------------------------------------------------------------------
custom_diff <- custom_m %>%
  filter(!name %in% iucn$name)

merged <- rbind(iucn,custom_diff)

st_write(merged,paste0(dir_('out/'),'merged_ranges_poly.gpkg'))



