#Valerio Barbarossa
# stript that compiles the occurrence records from the different datasets

library(valerioUtils)
libinv(c('dplyr','vroom'))

diag <- function(df,name_df = '',file_out = ''){
  cat('Dataset: ',name_df,'\n',
  'No. records: ',prettyNum(nrow(df),big.mark = ','),'\n',
  'No. species: ',prettyNum(length(unique(df$name)),big.mark = ','),'\n\n\n',
  file = file_out,append = T)
}

# read reference names that should be used to extract species from the datasets----------------------------
tab <- read.csv('proc/names_fishbase.csv',stringsAsFactors = F)

#assign names that will be used for filtering
filter_names <- unique(tab$name_synonym)

# clean and filter datasets--------------------------------------------------------------------------------
# setup location of datasets
dir_data <- '../data/fish_databases/'

# ala.org.ay
ala <- vroom(paste0(dir_data,'ala.org.au/Fishes-brief.csv'),delim = ',') %>%
  select(name = scientificName,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  filter(name %in% filter_names)
diag(ala,'ala.org.ay','filtering_occurrence_datasets_diag.log')

#boldsystems #not implemented

#fishnet2
fishnet <- vroom(paste0(dir_data,'fishnet2/fishnet2.csv')) %>%
  select(name = ScientificName,lon = Longitude,lat = Latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  filter(name %in% filter_names)
diag(fishnet,'fishnet2','filtering_occurrence_datasets_diag.log')


#gbif
gbif <- vroom(paste0(dir_data,'gbif/actinopterygii.csv')) %>%
  select(name = species,lon = decimalLongitude,lat = decimalLatitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  filter(name %in% filter_names)
diag(gbif,'gbif','filtering_occurrence_datasets_diag.log')


#portalbiodiversidade.icmbio.gov.br
bra <- vroom(paste0(dir_data,'portalbiodiversidade.icmbio.gov.br/portalbio_export_17-09-2019-10-21-11.csv')) %>%
  select(name = Especie,lon = Longitude,lat = Latitude) %>%
  .[.$lon != "Acesso Restrito" & .$name != "Sem Informações",] %>%
  filter(name %in% filter_names)
diag(bra,'portalbiodiversidade.icmbio.gov.br','filtering_occurrence_datasets_diag.log')


#splink.org
splink <- vroom(paste0(dir_data,'splink.org/speciesLink_all_112728_20190917101630.txt')) %>%
  select(name = scientificname,lon = longitude,lat = latitude) %>%
  .[!is.na(.$lon) & !is.na(.$name),] %>%
  filter(name %in% filter_names)
diag(splink,'splink.org','filtering_occurrence_datasets_diag.log')

# combine records from different datasets------------------------------------------------------------------

# bind and filter occurrence records
occ <- rbind(ala,fishnet,gbif,bra,splink) %>%
  arrange(name) %>%
  distinct()

# there are still some lat and lon coordinates with letters in there e.g., 1044730E
# difficult to convert without knowing the CRS, filter out!
# create vector to report rows with letters in latitude or longitude

# understand unique symbols to take out
unique_symbols <- strsplit(paste0(occ$lon,occ$lat),'') %>%
  do.call('c',.) %>%
  unique(.)
# exclude everything that is not numeric (numbers,.,-)
to_exclude <- unique_symbols[!unique_symbols %in% c(as.character(0:9),'.','-')] 
lonlat_to_exclude <- lapply(strsplit(paste0(occ$lon,occ$lat),''),
                          function(x) sum(x %in% to_exclude)> 0 ) %>%
  do.call('c',.)

# and filter occ
cat('Removing ',prettyNum(sum(lonlat_to_exclude),big.mark = ','),' records with letters in coordinates\n',
    file = 'filtering_occurrence_datasets_diag.log')
occ <- occ %>%
  filter(!lonlat_to_exclude) %>%
  mutate(name,lon = as.numeric(lon),lat = as.numeric(lat)) # transform lat lon to numeric


# merge with fishbase synonyms----------------------------------------------------------------------------

# get only synonyms table and adjust columns name
tab_syn <- tab %>%
  filter(name != name_synonym) %>% 
  select(name_fishbase = name,name = name_synonym)

occ_syn <- right_join(occ,tab_syn,by='name') %>% #select only records that are synonyms
  select(name = name_fishbase,lon,lat) # and assign the fishbase name to the name
occ_nosyn <- occ %>%
  filter(!name %in% tab_syn$name)

diag(occ_nosyn,'merged occurrence records (no synonyms)','filtering_occurrence_datasets_diag.log')
diag(occ_syn,'additional merged occurrence records from fishbase synonyms','filtering_occurrence_datasets_diag.log')

occ_total <- rbind(occ_nosyn,occ_syn) %>%
  arrange(name) %>%
  distinct()
diag(occ_total,'final occurrence records cleaned and checked for synonyms','filtering_occurrence_datasets_diag.log')

vroom_write(occ_total,'proc/compiled_occurrence_records.tsv.gz')
saveRDS(occ_total,'proc/compiled_occurrence_records.rds')
