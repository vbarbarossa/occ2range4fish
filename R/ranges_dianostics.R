# Valerio Barbarossa, 20 Sep 2019
# this script merges all the tables produced by reference2HB
# and runs the following diagnostics:
# 1) calculates total range area per species
# 2) plots a map of the obtained ranges, number of species per HB unit

library(valerioUtils)
libinv(c('dplyr','vroom','foreign','sf'))

HB_lev <- '08'

# files from ranges2HB script
range2HB_files <- list.files(path = paste0('proc/occurrence_records_on_hb',HB_lev,'_mollweide/'), full.names = T)

# set data directory where hydrobasins folder is store
dir_data <- '../data/'
dir_figs <- dir_('figs/')

# compile table ranges
tab <- lapply(range2HB_files,readRDS) %>%
  do.call('rbind',.) %>%
  as_tibble() %>%
  distinct() %>%
  select(name,HYBAS_ID = hybas_id)

cat('Number of species successfully referenced on HB: ',prettyNum(length(unique(tab$name)),big.mark = ','),'\n',
    'Number of HB units: ',prettyNum(nrow(tab),big.mark = ','),
    file = 'ranges_diagnostics.log')

# load HB
HB <- lapply(c('af','ar','as','au','eu','gr','na','sa','si'),
             function(x) read.dbf(paste0(dir_data,'HydroBASINS/global_lev',HB_lev,'/hybas_',x,'_lev',HB_lev,'_v1c.dbf'))) %>%
  do.call('rbind',.) %>%
  as_tibble()

tab_HB <- left_join(tab,HB)

# 1) calculate area per species
area <- tab_HB %>%
  group_by(name) %>%
  summarize(range_area = sum(SUB_AREA))

# 2) no species per HB
count <- tab_HB %>%
  group_by(HYBAS_ID) %>%
  summarize(no_species = n())

# load sf features for HB
HB_sf <- lapply(c('af','ar','as','au','eu','gr','na','sa','si'),
             function(x) st_read(paste0(dir_data,'HydroBASINS/global_lev',HB_lev,'/hybas_',x,'_lev',HB_lev,'_v1c.shp'))) %>%
  do.call('rbind',.)

count_sf <- right_join(HB_sf,count) # need to put first sf data to keep the sf type

pdf(paste0(dir_figs,'ranges_count',HB_lev,'.pdf'))
plot(count_sf['no_species'],border = NA)
dev.off()

# # 3) plot also occurrence points
# occ <- readRDS('proc/compiled_occurrence_records.rds')
# # convert to spatial points
# pts <- occ %>% 
#   filter(!is.na(lon) & !is.na(lat)) %>% #make sure there are no NAs in the coords
#   filter(lon >= -180 & lon <= 180) %>%
#   filter(lat >= -90 & lat <= 90) %>%
#   st_as_sf(coords = c('lon','lat'),crs=4326) #convert to sf
# 
# # and plot
# pdf(paste0(dir_figs,'occurrence_records.pdf'))
# plot(st_geometry(pts),cex = 0.01,pch=20)
# dev.off()
# 
# # basins with occ
# pdf(paste0(dir_figs,'occurrence_records_on_ranges.pdf'))
# plot(st_geometry(count_sf),col = 'gray90',border = NA)
# plot(st_geometry(pts),cex = 0.01,pch=20,add = TRUE)
# dev.off()

# 4) compare area with species in common with IUCN
# iucn <- lapply(1:2,
#                function(x) foreign::read.dbf(paste0(dir_data,'IUCN/FW_FISH_20181113/FW_FISH_PART_',x,'.dbf'))) %>%
#   do.call('rbind',.) %>%
#   as_tibble()
# 
# iucn_area <- iucn %>%
#   group_by(binomial) %>%
#   summarize(range_area_iucn = sum(shape_Area)) %>%
#   select(name = binomial,range_area_iucn)
# 
# valerioUtils::r.squared(compare_area$range_area,compare_area$range_area_iucn) 
# hmmm iucn area looks weird should compare both of them based on HB12 table
# then retrieve data from connectfish

# iucn_HB <- readRDS(paste0(dir_data,'hybas12_fish_w_area.rds')) %>%
#   as_tibble() %>%
#   select(name = binomial, HYBAS_ID, SUB_AREA) %>%
#   arrange(name) %>%
#   distinct()
# count_iucn_HB <- iucn_HB %>%
#   group_by(HYBAS_ID) %>%
#   summarize(no_species = n())

# iucn ranges based on hybas native from IUCN website
iucn_HB <- vroom(paste0(dir_data,'IUCN/FW_FISH_20190923/fish_hybas_table.csv')) %>%
  filter(presence %in% c(1,2)) %>%
  rename(HYBAS_ID = hybas_id)
# iucn_HB_sf <- right_join(HB_sf,iucn_HB)

count_iucn_HB <- iucn_HB %>%
  group_by(HYBAS_ID) %>%
  summarize(no_species = n()) %>%
  right_join(HB_sf,.)

pdf(paste0(dir_figs,'ranges_count_',HB_lev,'_iucn.pdf'))
plot(count_iucn_HB['no_species'],border = NA)
dev.off()

area_iucn_HB <- iucn_HB %>%
  left_join(.,HB_sf) %>%
  select(name = binomial,SUB_AREA) %>%
  group_by(name) %>%
  summarize(range_area_iucn = sum(SUB_AREA,na.rm=T)) %>%
  filter(range_area_iucn > 0)

# compare area tab
compare_area <- inner_join(area,area_iucn_HB)
lm(log10(range_area) ~ log10(range_area_iucn), data = compare_area) %>% summary()
valerioUtils::r.squared(log10(compare_area$range_area),log10(compare_area$range_area_iucn))

pdf(paste0(dir_figs,'ranges_plot_vs.pdf'))
plot(log10(compare_area$range_area),log10(compare_area$range_area_iucn))
dev.off()
