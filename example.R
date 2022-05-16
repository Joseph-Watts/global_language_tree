#' Example:
#' Building a global tree of societies in the SCCS using language classification
#' data from Glottolog v4.3 and D-Place
#' 
#' Sources:
#' https://d-place.org/
#' https://glottolog.org/

require(tidyverse)

#' Note: This source needs to be located in a folder with the 
#' "Glottolog_v4.3._table_simple.csv" file.
#' Whenever the source and data files are not in the current working directly
#' the chdir argument must be set to T.
source("glottree_function.R",
       chdir = T)

#' Reading glottocodes from D-Place data
dplace_socs <- read.csv("https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/SCCS/societies.csv")

tips_wanted <- dplace_socs$glottocode

#' No duplicates in this case, though it is possible to include duplicates
#' see comments on the glottree_function arguments.
duplicated(tips_wanted) %>% table()

SCCS_tree <- build_glot_tree(tips_wanted = tips_wanted,
                             add_duplicates = T,
                             dia_dist = 1.0,
                             lang_dist = 2.0,
                             subfam_dist = 2.0,
                             fam_dist = 2.0)

plot.phylo(SCCS_tree,
           type = "fan",
           font = 1,
           edge.width = .5, 
           label.offset = 0.1,
           align.tip.label = T,
           cex= 0.5,
           rotate = 0)


