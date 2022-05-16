#' This script is the process used to generate the "Glottolog_v4.3._table_simple.csv".#' 
#' This code is included for transparency and to allow users to update the
#' data for the latest version of Glottolog. It is not part of the main package.
#' 
#' Notes:
#' 1. Manually removed the "md.ini~" file from the following directory: 
#'      tuuu1241/kwii1241/east2867/vaal1235/sero1239/md.ini~
#'      The reason being that this is the only directory with a file in this 
#'      format and it disrupts the code below

require(tidyverse)

#' NOTE:
#' This requires glottolog to have been downloaded into the directory
#' specified below
glottolog_tree_path <- "P:/Data/Glottolog/glottolog-4.3/languoids/tree/"

tree_structure <- list.files(path = glottolog_tree_path,
                             pattern = NULL,
                             all.files = T,
                             full.names = F,
                             recursive = T)

# Function to process each unit
processing_unit <- function(path_text){
    
  unit_md_file <- read_delim(paste0(glottolog_tree_path, path_text), 
                             delim = "\n", 
                             col_names = FALSE) %>%
    as.data.frame()

  # Extracting unit name  
  name_index <- grepl(pattern = "^name", unit_md_file[,1]) %>% which()
  
  #if(length(name_index) > 1){stop()}
  #' Note: some files have additional name information (e.g. mohe1244) as 
  #' part of retired info. 
  #' This should never be the first entry, so shouldn't be an issue here.
  
  glot_unit_name <- strsplit(unit_md_file[name_index,1], "=")[[1]][2] %>% trimws()
  
  # Extracting unit class
  level_index <- grepl(pattern = "^level", unit_md_file[,1]) %>% which()
  
  glot_unit_level <- strsplit(unit_md_file[level_index,1], "=")[[1]][2] %>% trimws()
  
  # Remove md.ini
  glot_hierarchy <- sub("/md.ini$", "", path_text)
  
  # Glottocode
  glottocode_unit <- str_extract(glot_hierarchy , "[[:lower:]]{4}[[:digit:]]{4}$")
  
  # Unit family
  glot_family <- str_extract(glot_hierarchy , "^[[:lower:]]{4}[[:digit:]]{4}")
  
  return(list(glottocode = glottocode_unit,
              name = glot_unit_name,
              level = glot_unit_level,
              glot_path = glot_hierarchy,
              family = glot_family))
   
}

#' Testing loop used to identify where errors are occurring

testing <- F
if(testing){
  
  for(i in 1:length(tree_structure)){
    
  test <- processing_unit(tree_structure[i]) 
    
  }
    
}

glot_processing <- lapply(tree_structure, processing_unit)
glot_df <- do.call("rbind", glot_processing) %>% 
  as.data.frame()

for(i in 1:ncol(glot_df)){
  
  glot_df[,i] <- as.character(glot_df[,i])
  
}


glot_df[which(is.na(glot_df$glottocode)), ]

na_index <- which(is.na(glot_df$glottocode))

for(i in 1:length(na_index)){
  
  i_split <- strsplit(glot_df$glot_path[na_index[i]], "/") %>% unlist()
  
  glot_df$glottocode[na_index[i]] <- i_split[length(i_split)]
  
}

if(sum(is.na(glot_df$glottocode)) > 0 ) stop("duplicate glottocodes")

#' Writing out the table
write_csv(glot_df, "Glottolog_v4.3._table_simple.csv")

