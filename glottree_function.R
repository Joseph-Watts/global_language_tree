#' Glottree is a R library that builds an ultrametric global tree of languages 
#' according to their language classification structure in Glottolog

#' The structure of this tree is massively simplified from the tree structures
#' on Glottolog, and branches are proportional to langauge classification level
#' rather than time or linguistic change.

#' Author: Joseph Watts
#' Email: joseph.watts@otago.ac.nz
#' Website: www.josephwatts.org

#' License: GNU General Public License v2.0
#' 
#' If you use this package, please cite: 
#' 
#' Watts J, Hamerslag EM, Sprules C, Shaver JH, Dunbar RIM (2022). 
#' Food storage facilitates professional religious specialization in 
#' hunter–gatherer societies. 
#' Evolutionary Human Sciences 4, e17, 1–11. https://doi.org/10.1017/ehs.2022.17
#' 
#' AND:
#' 
#' Glottolog: https://glottolog.org/

#' ------------------
#' Notes:
#' 1. Glottolog includes dialects nested within dialects. 
#'    For purposes of this tree, all dialects within a language are treated as 
#'    being at the same level of the tree.
#' 2. Code has been tested and run with R version 4.1.0 and Glottolog v4.3. 
#'    If there are changes to Glottolog formatting, this package will require
#'    updating    
#' 3. The Glottolog data is licences under a Creative Commons Attribution 4.0 
#'    International license.    

library(lingtypology)
library(spaa)
library(ape)
library(phytools)
library(tidyverse)

glot_df <- read_csv("Glottolog_v4.3._table_simple.csv")                         
glot_df_ld <- glot_df[glot_df$level %in% c("language", "dialect"), ]

#' Defining sub-family splits.
#' 
#' These are the splits used when building the tree below.
#' 
#' This splitting method functions in a top down manner. It starts by identifying the 
#' highest order split within a language family. It then checks whether this split on 
#' the tree differentiates a sufficient proportion of languages and dialects within 
#' the language family. 
#' 
#' Whether this condition is met depends on the user specified argument (min_prop_split).
#' The min_pop_split refers to the minimum proportion of languages (including dialects) 
#' within a language family that the split must differentiate. 
#' 
#' For example, in Glottolog v4.3 the highest level split within the Indo-European family 
#' is between Anatolian (10 extinct languages & dialects) and the Classical Indo-European 
#' languages (2588 languages & dialects). 
#' The smaller branch resulting from this split includes less than 1% of entries for this
#' language family. This means that treating the highest order split within a language
#' family captures minimal variation within the tree.
#' 
#' In cases where there highest order split of the tree differentiates at least the 
#' min_prop_split of languages in the family, this split us used for the purposes of 
#' defining the language sub-family.
#' 
#' In cases where the highest order split of the tree does not differentiate at least the
#' min_prop_split of languages in the family, the .. 


define_subfamily <- function(glot_df_ld, min_prop_split){
  
  all_family_codes <- unique(glot_df_ld$family)
  
  for(i in 1:length(all_family_codes)){
    
    if(i == 1){
      
      glot_df_ld_output <- glot_df_ld
      
      #' Creating a subfamily column
      #' If a language is an isolate, subfamily becomes the same as the 
      #' glottocode and family codes
      glot_df_ld_output$subfamily <- NA
      
    }
    
    family_index <- which(glot_df_ld_output$family == all_family_codes[i] &
                            glot_df_ld_output$level == "language")
  
    family_str_split <- strsplit(glot_df_ld_output$glot_path[family_index], "/")
    
    family_n <- length(family_index)
    
    family_max_depth <- lapply(family_str_split, length) %>% 
      unlist() %>% 
      max()
    
    #' Making the family_str_split items all the same length so they can be 
    #' easily added to a df
    for(j in 1:family_n){
      
      n_na_add <- family_max_depth - length(family_str_split[[j]]) 
      family_str_split[[j]] <- c(family_str_split[[j]], rep(NA, n_na_add))
      
    }
    
    # Combining split items into a single df
    family_str_df <- do.call("rbind", family_str_split) %>% 
      as.data.frame()
    
    #' ---
    #' This component will work its way down the largest splits of the tree 
    #' deiving into two groups until the potention condition is met
    family_str_df$subfamily <- NA
    min_prop_met <- F
    subfam_level <- 1
    
    #' Checking to make sure this is not an isolate and that there is more than 
    #' one unit in the family
    #' 
    #' NOTE: Could shift the definition of language isolates to here? 
    #' Might be more efficient, but not a biggie.
    if(family_n == 1){
      
      if(glot_df_ld_output$glottocode[family_index] == 
         glot_df_ld_output$family[family_index]){
        
        family_str_df[1, "subfamily"] <- glot_df_ld_output$glottocode[family_index]
        
        min_prop_met <- T
        
      }else{
        
        stop(paste0("Error defining subfamily. Unit isolated but without the 
                    same name for unit and unit family: ",
                    glot_df_ld_output$glottocode[family_index]))
        
      }
      
    }
    
    # While the min_prop_met not true, search down the tree for splits
    while(!min_prop_met){
      
      if((subfam_level + 1) > family_max_depth){
        stop(paste0("No suitable subfamily identified for ", 
                    all_family_codes[i]))
      }
      
      family_seaching_index <- which(is.na(family_str_df$subfamily))
      
      split_table <- family_str_df[family_seaching_index , subfam_level + 1] %>%
        table() %>%
        sort(decreasing = T) %>%
        as.data.frame(stringsAsFactors = F)
      
      max_split_n <- split_table$Freq[1]
      
      max_glot <- split_table[1, 1]
      
      other_glots <- split_table[-1, 1]
      
      min_prop_met <- (max_split_n / family_n) <= (1 - min_prop_split)
      
      if(min_prop_met){
        
        family_str_df[family_seaching_index, "subfamily"] <- 
          family_str_df[family_seaching_index , subfam_level + 1]
        
      }else{
        
        family_str_df[family_seaching_index, "subfamily"] <- 
          ifelse(family_str_df[family_seaching_index , subfam_level + 1] %in% 
                   other_glots, 
                 family_str_df[family_seaching_index , subfam_level + 1],
                 NA)
        
        subfam_level <- subfam_level + 1
        
      }
      
    }
    
    if(length(family_str_df$subfamily) != length(family_index)){
      stop()
    }
    
    glot_df_ld_output$subfamily[family_index] <- family_str_df$subfamily
    
  }
  
  # Making sure all languages have subfamilies defined
  if(sum(is.na(glot_df_ld_output$subfamily[glot_df_ld_output$level == 
                                           "language"])) > 0){
    stop("Not all languages have subfamilies defined")
  }
  
  #' Define the langauges for each of the dialects and then assign a sub-family
  
  dialect_index <- which(glot_df_ld_output$level == "dialect")
  
  dialect_paths <- strsplit(glot_df_ld_output$glot_path[dialect_index], "/")
  
  glot_df_ld_output$dialect_parent <- NA
  
  for(i in 1:length(dialect_paths)){
    
    parent_match_index <- 1
    
    i_parent_index <- NULL
    
    while(length(i_parent_index) == 0){
      
      i_parent <- dialect_paths[[i]][length(dialect_paths[[i]]) - parent_match_index]
      
      i_parent_index <- which(glot_df_ld_output$glottocode == i_parent)
      
      parent_match_index <- parent_match_index + 1
      
      if(glot_df_ld_output$level[i_parent_index] == "dialect"){
        i_parent_index <- NULL
        }
      
      if(parent_match_index > length(dialect_paths)){
        stop("Parent not found")
        }
      
    }
    
    if(length(i_parent_index) > 1){
      
      stop("More than one parent match found")
      
    }
    
    glot_df_ld_output$dialect_parent[dialect_index[i]] <- i_parent
    
    glot_df_ld_output$subfamily[dialect_index[i]] <- 
      glot_df_ld_output$subfamily[i_parent_index]
    
  }

  return(glot_df_ld_output)
  
}

#' -----------------------------
#' Function that takes a list of names, and adds them to a string that can be 
#' used in newick format e.g. c("A", "B", "C") becomes "A:1,B:1,C:1" if new 
#' edge length is set to 1.
name2newick_string <- function(taxa_names, new_edge_length = 1){
  
  paste0(paste0(taxa_names, collapse = paste0(c(":", new_edge_length, ","),
                                              collapse = "")),
         paste0(":", new_edge_length,""))
  
}


#' -----------------------------
#' Function that adds new tips as a polytomy to an existing tip
#' NOTES: 
#' 1. This function does not provide a way to adding tips to a node on the tree
#' 2. Takes and returns trees in text format, not phylo objects
add_tips_to_tip <- function(tree_text, target_tip, new_tips, new_edge_length = 1){
  
  to_replace <- paste0(c("(", 
                         name2newick_string(new_tips, new_edge_length),
                         ")"),
                       collapse = ""
  )
  
  target_start <- regexpr(paste0("(\\(|,)", target_tip, ":"), tree_text)[1]
  
  opening_character <- substr(tree_text,
                              start = target_start,
                              stop = target_start)
  
  if(opening_character == "("){
    
    sub(pattern = paste0("\\(", target_tip, ":"), 
        replacement = paste0("\\(", to_replace, target_tip, ":"),
        x = tree_text)
    
  }else if(opening_character == ","){
    
    sub(pattern = paste0(",", target_tip, ":"), 
        replacement = paste0(",", to_replace, target_tip, ":"),
        x = tree_text)
    
  }else{
    
    stop(paste0("There is an invlaid character ", 
                opening_character,
                " included in the following target tip: ",
                target_tip,
                " with the following new tips: ",
                new_tips))
    
  }
  
  
}


#' -----------------------------
#' Function that adds new tips as a polytomy to an existing node
#' NOTES: 
#' 1. This will not work as a means to add tips to an existing tips
#' 2. Takes and returns trees in text format, not phylo objects
add_tips_to_node <- function(tree_text, target_node, new_tips, new_edge_length = 1){
  
  to_replace <- paste0(c(",", 
                         name2newick_string(new_tips, new_edge_length),
                         ")"),
                       collapse = ""
  )
  
  sub(pattern = paste0(")", target_node, ":"), 
      replacement = paste0(to_replace, target_node, ":"),
      x = tree_text)
  
}


#' -----------------------------
#' Function that adds tips to a tree, in text format
#' Adds new tips as a polytomy to an existing node or tip

add_tips <- function(tree_text, target, new_tips, new_edge_length = 1){
  
  target_start <- regexpr(paste0(target, ":"), tree_text)[1]
  
  target_type <- ifelse(substr(tree_text,
                               start = target_start - 1,
                               stop = target_start - 1) == ")",
                        "node", 
                        "tip")
  
  if(target_type == "node"){
    
    add_tips_to_node(tree_text = tree_text,
                     target_node = target,
                     new_tips = new_tips,
                     new_edge_length = new_edge_length)
    
  }else if(target_type == "tip"){
    
    add_tips_to_tip(tree_text = tree_text,
                    target_tip = target,
                    new_tips = new_tips,
                    new_edge_length = new_edge_length)
    
  }
  
}


#' -----------------------------
#' Cleaning names
cleaner <- function(x){
  
  x <- gsub(pattern = " ", replacement = "_", x = x)
  x <- gsub(pattern = "\\(", replacement = "", x = x)
  x <- gsub(pattern = "\\)", replacement = "", x = x)
  gsub(pattern = "[[:punct:]]", replacement = "_", x = x)
  
}


#' ------------------
build_glot_tree <- function(
  #' Takes a list of glottocodes.Can also be "all", but will be slow.
  tips_wanted = NULL, 

  #' If there are duplicates of the taxa names supplied in tips_wanted,
  #' these will also be duplicated on the tree when this argument is TRUE.
  #' E.g. xxx1234, xxx1234 will become xxx1234_1 and xxx1234_2 on the tree.
  #' If false, duplicates are discarded and only the first instance of the
  #' glottocode is included on the tree.  
  add_duplicates = F,

  #' Dialects of the same language have a distance of dia_dist
  dia_dist = 0.5,
  
  #' Different languages in the same language sub-family (grouped at the highest 
  #' level of sub-family within that family) have a distance of lang_dist
  lang_dist = 2.0,
  
  #' Languages in the same family but not the same sub-family have a distance of 
  #' subfam_dist
  subfam_dist = 4.0,
  
  #' Languages in different language families have a distance of fam_dist
  fam_dist = 8.0,
  
  #' Parents of dialects
  lang_parent_dist = lang_dist - dia_dist,
  
  #' Length of tree root
  root_length = 1.0,
  
  #' Minimum proportion of languages within a family that a subfamily split 
  #' must differentiate
  min_prop_split = 0.1){
  
  #' Quick checks of function arguments
  if(is.null(tips_wanted))stop("tips_wanted must be defined")

  if(min_prop_split < 0.0001 | min_prop_split > 0.5){
    
    stop("min_prop_split must be between 0.0001 and 0.5")
    
    }
  #' NOTE:
  #' ? Could have 0.5 - 1.0 and it should still work,
  #' ? it would just become the inverse of the proportion of languages included
  #' ? in the largest subfamily?
  
  #' Defining the subfamily using the define_subfamily function
  glot_data <- define_subfamily(glot_df_ld = glot_df_ld,
                   min_prop_split = min_prop_split)
  
  if(tips_wanted[1] == "all"){
    
    tips_wanted <- glot_data$glottocode
    
  }
  
  tips_wanted_unique <- tips_wanted %>% 
    unique()
  
  #' Checking that all tips_wanted are valid glottocodes
  if(sum(!tips_wanted_unique %in% glot_df_ld$glottocode) > 0){
    
    problem_codes <- tips_wanted_unique[!tips_wanted_unique %in% glot_data$glottocode]
    
    valid_problem_codes <- problem_codes[problem_codes %in% glot_df$glottocode]
    invalid_problem_codes <- problem_codes[!problem_codes %in% glot_df$glottocode]
    
    if(length(valid_problem_codes) > 0 & length(invalid_problem_codes) > 0){
      
      stop(cat(paste(c("The following are not langauge or dialect codes (may be subfamily or family codes?):",
                   valid_problem_codes, "\n"), collapse = " "),
               paste(c("AND the following are not valid codes in Glottolog 4.3:",
                       invalid_problem_codes), collapse = " ")))
      
    }else if(length(valid_problem_codes) > 0 & length(invalid_problem_codes) == 0){
      
      stop(paste(c("The following are not langauge or dialect codes:",
                       valid_problem_codes), collapse = " "))
      
    }else{
      
      stop(paste(c("The following are not valid Glottocodes:",
                   invalid_problem_codes), collapse = " "))
      
    }
    
  }
  
  #' ------------------
  #' Checking for overlap in names and addressing duplications

  #' Checking whether the language name is the same as the subfamily name
  glot_data$glottocode %in% glot_data$subfamily %>%
    table()
  
  overlap_index <- which(glot_data$subfamily %in% glot_data$glottocode)
  
  glot_data$subfamily[overlap_index] <- paste0(glot_data$subfamily[overlap_index], 
                                               "_sf")
  
  #' Checking whether the subfamily is the same as the family name
  glot_data$subfamily %in% glot_data$family %>%
    table()
  
  if(sum(glot_data$subfamily %in% glot_data$family) > 0){
    stop("there are subfamily names identical to family names")
    }
  
  #' checking for duplicated language names 
  duplicated(glot_data$glottocode) %>%
    table()
  
  if(sum(duplicated(glot_data$glottocode)) > 0) {
    stop("there are duplicate glottocodes")
  }
  
  if(sum(is.na(glot_data$glottocode)) > 0){
    stop("there are NA values for glottocodes")
  }
  
  #' -----------------
  #' Pruning dataset down to just those languages defined in tips_wanted
  #' 
  
  tip_taxa_parents <- glot_data$dialect_parent[glot_data$glottocode %in%
                                                 tips_wanted_unique] %>%
    na.omit()
  
  glots_wanted <- c(tips_wanted_unique, tip_taxa_parents) %>% 
    unique()
  
  glot_data <- glot_data[glot_data$glottocode %in% glots_wanted, ]
  
  
  #' If add_duplicates is TRUE, add in the following
  
  if(add_duplicates & sum(duplicated(tips_wanted)) > 0) {
    
    tip_freq_table <- tips_wanted %>%
      table() %>%
      as.data.frame() %>%
      subset(Freq > 1)
    
    for(i in 1:nrow(tip_freq_table)){
      
      #' location of existing language entry in glotto data
      i_df_index <- which(glot_data$glottocode == tip_freq_table[i, 1])
      
      #' location where new entries will be added
      new_i_index <- (nrow(glot_data)+1) : (nrow(glot_data)+tip_freq_table$Freq[i])
      
      #' duplication existing entries into new index location
      glot_data[new_i_index, ] <- glot_data[i_df_index, ]
      
      #' Adding in new glottocodes
      glot_data$glottocode[new_i_index] <- paste0(glot_data$glottocode[i_df_index],
                                                  "_", 
                                                  1:tip_freq_table$Freq[i])
      
      #' Setting up the new entries to be treated as dialects
      glot_data$level[new_i_index] <- "dialect"
      
      glot_data$dialect_parent[new_i_index] <- glot_data$glottocode[i_df_index]
      
      #' If the language being duplicated is a dialect, then
      if(glot_data$level[i_df_index] == "dialect"){
        
        #' Use the parent language of that dialect at the parent for the new entries
        glot_data$dialect_parent[new_i_index] <- glot_data$dialect_parent[i_df_index]
        
        #' And then drop the origional dialect entry from the dataset
        glot_data <- glot_data[-i_df_index, ]
        
        #' Else, if the the origional language entry is a language
      }else if(glot_data$level[i_df_index] == "language"){
        
        #' Treat that origonal entry as the parent for the new entries and 
        #' keep the origional entry.
        glot_data$dialect_parent[new_i_index] <- glot_data$glottocode[i_df_index]
        
      }
      
    }
    
  }
  
  
  #' --------------------------------------
  #' Creating a tree with just families
  
  #' Generating the base of the tree
  
  families <- unique(glot_data$family)
  
  tree_text <- paste0(c("(", 
                        name2newick_string(families, new_edge_length = fam_dist/2), 
                        ")root:", 
                        root_length, 
                        ";" ), 
                      collapse = "")

  #' --------------------------------------
  #' Adding in subfamilies to the tree
  
  for(i in 1:length(families)){
    
    fam_i_subfam_index <- which(glot_data$family == families[i])
    
    fam_i_subfam <- glot_data$subfamily[fam_i_subfam_index] %>% 
      unique()
    
    tree_text <- add_tips_to_tip(tree_text = tree_text, 
                                 target_tip = families[i], 
                                 new_tips = fam_i_subfam, 
                                 new_edge_length = subfam_dist/2)
    
    
  }

  #' --------------------------------------
  #' Adding in languages
  
  for(i in 1:length(families)){
    
    fam_i_fam_index <- which(glot_data$family == families[i])
    
    fam_i_subfam <- glot_data$subfamily[fam_i_fam_index] %>%
      unique()
    
    for(j in 1:length(fam_i_subfam)){
      
      #' ------
      #' Starting off by adding languages
      ij_all_index <- which(glot_data$family == families[i] &
                              glot_data$subfamily == fam_i_subfam[j]) 
      
      ij_parent_langs <- glot_data$dialect_parent[ij_all_index] %>% 
        na.omit() %>%
        unique()
      
      ij_lang_index <- ij_all_index[glot_data$level[ij_all_index] == "language"] %>%
        na.omit() %>%
        na.omit()
      
      ij_langs <- glot_data$glottocode[ij_lang_index] %>%
        na.omit() %>%
        unique()
      
      ij_langs <- ij_langs[!ij_langs %in% ij_parent_langs]
      
      if(length(ij_langs) > 0){
        
        tree_text <- add_tips_to_tip(tree_text = tree_text, 
                                     target_tip = fam_i_subfam[j], 
                                     new_tips = ij_langs, 
                                     new_edge_length = lang_dist/2)
        
      }
      
      #' ------
      #' Adding parent languages
      
      if(length(ij_parent_langs) > 0){
        
        if(length(ij_langs) > 0){
          
          tree_text <- add_tips_to_node(tree_text = tree_text, 
                                        target_node = fam_i_subfam[j], 
                                        new_tips = ij_parent_langs, 
                                        new_edge_length = lang_parent_dist/2)
          
        }else{
          
          tree_text <- add_tips_to_tip(tree_text = tree_text, 
                                       target_tip = fam_i_subfam[j], 
                                       new_tips = ij_parent_langs, 
                                       new_edge_length = lang_parent_dist/2)
          
        }
        
      }
      
    }
    
  }

  #' ------ 
  #' Adding dialects to the tree
  #' Note: This is quite a slow part of the process and there would be more 
  #' efficient ways of doing this.
  
  all_parent_langs <- glot_data$dialect_parent %>% 
    na.omit() %>%
    unique()
  
  for(i in 1:length(all_parent_langs)){
    
    lang_index <- which(glot_data$dialect_parent == all_parent_langs[i])
    
    lang_dialects <- glot_data$glottocode[lang_index]
    
    tree_text <- add_tips_to_tip(tree_text = tree_text, 
                                 target_tip = all_parent_langs[i], 
                                 new_tips = lang_dialects, 
                                 new_edge_length = dia_dist/2)
    
  }

  #' ----------------------
  #' Outputing tree
  read.tree(text = tree_text) %>%
    return()
  
}

