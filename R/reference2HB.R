# Valerio Barbarossa, 20 Sep 2019
# this script references the occurrence records compiled in the script "compile_occurrence_records.R"
# to the HydroBASINS units (level 12)
# should output a table with species name and corresponding HB ids

#sbatch --array=1-11
# get the array number from environment
g <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#no groups to split the data into (= NO ARRAY)
N = 20

# set number of cores
NC = 22

# load libraries
library(valerioUtils)
libinv(c('dplyr','sf'))

# set data directory where hydrobasins folder is store
dir_data <- '../data/'

# load occurrence data compiled in previous scripts
occ <- readRDS('proc/compiled_occurrence_records.rds') 

# split the data to run it in parallel---------------------------------------------------------------------
# need to balance species with a lot of records
set.seed(12345)
rs <- sort(table(occ$name),decreasing = T) # table of records per species sorted from highest
list_names <- list() #init list
for(i in 1:N){
  # select names every N from rs
  list_names[[i]] <- names(rs[seq(i,length(rs),N)])
}

# create a list of filtered records based on list_names and convert to sf spatial points
# each element contains a tibble with selected records and geometry
# list_records <- lapply(list_names,function(x){
#   # sinlge point features
#   p <- occ %>% 
#     filter(name %in% x) %>%
#     filter(!is.na(lon) & !is.na(lat)) %>% #make sure there are no NAs in the coords
#     filter(lon >= -180 & lon <= 180) %>%
#     filter(lat >= -90 & lat <= 90) %>%
#     st_as_sf(coords = c('lon','lat'),crs=4326) #convert to sf
#   # multipoint features
#   mp <- lapply(split(p,p$name),summarize) %>%
#     do.call('rbind',.) %>%
#     mutate(name = unique(p$name)) %>%
#     select(name,geometry) %>%
#     st_cast('MULTIPOINT')
#   return(mp)
# })
# sinlge point features
x <- list_names[[g]]
p <- occ %>% 
  filter(name %in% x) %>%
  filter(!is.na(lon) & !is.na(lat)) %>% #make sure there are no NAs in the coords
  filter(lon >= -180 & lon <= 180) %>%
  filter(lat >= -90 & lat <= 90) %>%
  st_as_sf(coords = c('lon','lat'),crs=4326) #convert to sf
# multipoint features
pts <- lapply(split(p,p$name),summarize) %>%
  do.call('rbind',.) %>%
  mutate(name = unique(p$name)) %>%
  select(name,geometry) %>%
  st_cast('MULTIPOINT')

# load HydroBASINS lev 12 data----------------------------------------------------------------------------
hb_cont <- list()
continents <- c('af','ar','as','au','eu','gr','na','sa','si')
for(i in seq_along(continents)) hb_cont[[i]] <- read_sf(paste0(dir_data,'HydroBASINS/global_lev12/hybas_',continents[i],'_lev12_v1c.shp'))

# define function that extract a table after intersecting HB and occ
define_intersection <- function(hb,r){
  
  hb_ids <- hb$HYBAS_ID
  isc <- st_intersects(r,hb,sparse=T)
  
  tab <- lapply(1:nrow(r),
                function(x) if(length(isc[[x]])>0) data.frame(name = r$name[x], hybas_id = hb_ids[isc[[x]]])) %>%
    do.call('rbind',.)
  return(tab)
}

# define output directory where to store chunked results
dir_out <- dir_('proc/occurrence_records_on_hb/')

# split the data frame in NC chunks
# pts <- list_records[[g]]
n <- NC-1
nr <- nrow(pts)
pts_list <- split(pts, rep(1:ceiling(nr/n), each=n, length.out=nr))

parallel::mcmapply(function(n){
  
  for(i in seq_along(continents)){
    # continent i, chunk n
    t <- define_intersection(hb=hb_cont[[i]],r=pts_list[[n]])
    saveRDS(t,paste0(dir_out,'array_',g,continents[i],'_chunk_',n,'.rds'))
    
  }
  
},seq_along(pts_list),SIMPLIFY = F,mc.cores = NC)


# for diagnostics
# parallel::mcmapply(function(n){
#   
#   for(i in seq_along(continents)){
#     # continent i, chunk n
#     t <- define_intersection(hb=hb_cont[[i]],r=pts_list[[n]])
#     saveRDS(t,paste0(dir_out,'array_',g,continents[i],'_chunk_',n,'.rds'))
#     
#   }
#   
# },1:5,SIMPLIFY = F,mc.cores = 5)



