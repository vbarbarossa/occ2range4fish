# Valerio Barbarossa, 19 Sep 2019
# this script generates a table of freshwater fish binomial names and synonyms
# based on IUCN names 

# get the array number from environment
# g <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

#no groups to split the data into (= NO ARRAY)
# N = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_MAX"))
N = 20

#no cores to use
NC = 22

# start group (in case need to rerun not from begining)
start_no = 14

# output folder
dir_out <- 'proc/iucn_synonyms/'

# check with IUCN names
library(valerioUtils)
libinv(c('rredlist','dplyr'))

token <- 'd361026f05b472e57b0ffe1fa5c9a768aaf3d8391abbb464293e9efe2bbbf733'
# read table of names retrieved from fishbase
fishbase <- read.csv('proc/names_fishbase_and_tedesco.csv',stringsAsFactors = F)
iucn <- lapply(1:2,
               function(x) foreign::read.dbf(paste0('../data/IUCN/FW_FISH_20181113/FW_FISH_PART_',x,'.dbf'))) %>%
  do.call('rbind',.)

names <- unique(c(fishbase$name_synonym,iucn$binomial)) %>%
  .[!is.na(.)]

# select based on array
set.seed(12345)
chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))
list_names <- chunk2(names,N)

for(g in start_no:N){
  print(paste0('Processing group ',g,'/',N,'..'))
  
  names <- list_names[[g]]
  # st <- Sys.time()
  iucn <- parallel::mcmapply(function(i) {
    syn <- rl_synonyms(names[i],key=token)$result
    if(length(syn) > 0){
      syn$name_src = names[i]
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
        name_src = names[i])
    }
    print(i)
    return(res)
    
  },1:length(names),SIMPLIFY = FALSE,mc.cores = NC) %>%
    do.call('rbind',.) %>%
    distinct() # removes eventual duplicated rows
  # Sys.time() - st
  
  write.csv(iucn,paste0(dir_(dir_out),'iucn_names_',g,'.csv'),row.names = F)
  
}
