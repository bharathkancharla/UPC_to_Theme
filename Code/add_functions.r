keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
keep_theme <- c("'","+")

`%notin%` <- Negate(`%in%`)     

AND_NOT_THEMES <- TaxoNomy[str_detect(TaxoNomy$Category,"AND NOT"),]$Theme
FREE_THEMES <- TaxoNomy[str_detect(tolower(TaxoNomy$Theme),"free$"),]$Theme
FREE_THEMES <- FREE_THEMES[!(FREE_THEMES %in% "Cage/Pen Free")]
CHECK_THEMES <- TaxoNomy[str_detect(TaxoNomy$Category,"CHECK"),]$Theme


# Themes that required additional validate to avoid capturing negative scenario's
ADD_CHECK_THEMES <- c("Artificial Colors","Artificial Flavors",
                      "Artificial Preservatives","Artificial Sweeteners",
                      "Artificial- Total","Fillers","By-Products",
                      "Propylene Glycol","Glyphosate", "Ethoxyquin", 
                      "Carrageenan")

FREE_THEMES <- append(FREE_THEMES, ADD_CHECK_THEMES)

NEGATIVE_WORD_LIST <- c("allergen", "allergic", "allergenic",
                        "made without",	"without","none","non", 
                        "never", "left out", "avoid", "unwanted", "Diet","dont", 
                        "intolerance", "formula", "not", "intolerant", "allergy",
                        "sensitivity", "added", "sensitive", "no", "does not contain",
                        "leave out", "does not", "No", "Low", "Zero","does not"
                        )

NO_ABBR_THEMES <- c("Seafood Sourcing","DairyFree","Sprouted (Grain)","Digestive (ALL)",
                    "Insect Proteins")


PREFIX <- c("No", "Low", "Zero",	"Free of", "allergen", "allergic", 
            "allergenic", "free from", "free of","made without",	
            "without", "non", "never", "none", "left out", "avoid", "unwanted")

SUFFIX <- c("Free", "Diet", "intolerance", "formula", "not", "intolerant", "allergy")

COMMON_WORDS <- c("free","health","food","certified","verified","broth")
common_words_exp <- paste0(COMMON_WORDS,collapse = "|")
common_word_themes <- TaxoNomy[str_detect(tolower(TaxoNomy$Theme),common_words_exp,negate=T),]$Theme


negative_exp <- paste0(NEGATIVE_WORD_LIST,collapse = "|")


# Custom abbreviated function
custom_abbreviate <- function(taxonmoy){
  modified_taxonomy <- c()
  for (taxonmoy_string in tolower(taxonmoy)){
    mod_tax_str <- ""
    if(nchar(taxonmoy_string)>4){
      for (split_word in strsplit(taxonmoy_string,"\\s")[[1]]){
        npunct <- str_count(split_word,"[[:punct:]]")
        # Check if taxonomy word has length greater than 3
        if (nchar(split_word)>3+npunct){
          # removing the characters occuring together in word
          tax_word_i <- gsub("([[:alpha:]])\\1+", "\\1", split_word)
          
          # Remove the vowels from the taxonomy word
          tax_word_i <- gsub("(?<=[a-z])[aeiou]", "", tax_word_i, perl=TRUE)
          
          # If abbreviation length is greater than 5 limit it to 5 characters
          tax_word <- ""
          n <- 1
          
          if ((nchar(tax_word_i) - str_count(tax_word_i, "[[:punct:]]")<6)){
            tax_word <- tax_word_i
          }else{
            for (char in strsplit(tax_word_i,split="")[[1]]){
              if (str_detect(char,"[[:punct:]]",negate = T) & (n<6)){
                n <- n+1
                tax_word <- paste0(tax_word,char)
              }
              else if(str_detect(char,"[[:punct:]]")){
                tax_word <- paste0(tax_word,char)
              }
            }
          }
          # tax_word <- substring(tax_word,1L,(npunct+5L))  
          
            
          # check if abbreviated word has just one word
          if (nchar(tax_word)==1){
            tax_word <- split_word
          }
        }
        else{tax_word <- split_word}
        mod_tax_str <- paste0(mod_tax_str,tax_word,sep=" ")
      }
      modified_taxonomy <- append(modified_taxonomy,str_trim(mod_tax_str))
    }
    else{modified_taxonomy <- append(modified_taxonomy,str_trim(taxonmoy_string))}
  }
  return(modified_taxonomy)
}

condition_finder <- function(TaxoNomy){
  condition_list <- c()
  for (taxonomy_string in TaxoNomy){
    taxonomy_string <- toupper(taxonomy_string)
    and_not <- str_count(taxonomy_string,"AND NOT")
    and <- str_count(taxonomy_string,"AND") - and_not
    dou_brac <- str_count(taxonomy_string,"\\)\\)")
    if (and_not==0 & and==0){
      condition_list <- append(condition_list,"Simple OR")
    }else if (and_not==1 & and==0){
      condition_list <- append(condition_list,"Single AND NOT")
    }else if (dou_brac==1 & str_ends(taxonomy_string,"\\)\\)",negate =T)){
      condition_list <- append(condition_list,"Double closing bracket")
    }else if (and==1 & and_not==0){
      condition_list <- append(condition_list,"Single AND")
    }else if(and==1 & and_not==1){
      condition_list <- append(condition_list,"AND and AND NOT")
    }else if(and==2 & and_not==0 & dou_brac ==1){
      condition_list <- append(condition_list,"Double AND")
    }else {
      condition_list <- append(condition_list,"CHECK")
    }
    # print(condition_list)
  }
  return(condition_list)
}

reorder_Tax <- function(Tax_conditions){
  cond_list <- list()
  for (cond in Tax_conditions){
    cond <- str_squish(cond)
    cond_split <- unique(stri_remove_empty_na(trimws(unlist(str_split(cond,"\\|")))))
    cond_split_ord <- cond_split[order(-nchar(cond_split))]
    cond_list <- append(cond_list,paste0("\\b",paste0(cond_split_ord,collapse = "\\b|\\b"),"\\b"))
  }
  return(unlist(cond_list))
}

prefix_Suffix_val <- function(string, term1, term2, free_t= T,threshold = 6){
  string <- tolower(gsub(", ",",",string))
  validaton_vec <- c()
  fir_str <- paste0("\\b",paste0(term1, collapse = "\\b|\\b"),"\\b")
  sec_str <- paste0("\\b",paste0(term2, collapse = "\\b|\\b"),"\\b")
  
  matched_crt <- unlist(str_extract_all(string,
                                        str_glue("{fir_str}\\s*[^\\.\\?]*?\\s*{sec_str}")))
  
  if (is_empty(matched_crt)|str_detect(fir_str,matched_crt)|str_detect(sec_str,matched_crt)){
    validaton_vec <- return(FALSE)
  }else{}
  # https://www.regular-expressions.info/near.html#:~:text=If%20you%20want%20to%20find,will%20match%20word2%20near%20word2.
  # str_glue("{fir_str}(?:\\W+\\w+){1,6}\\W+{sec_str}")
  
  for (w1 in term1){
    for (w2 in term2){
      # Capture the data only if word1 preceds the word2
      matched_crt <- unlist(str_extract_all(string,
                                               str_glue("{w1}\\s*[^\\.\\?]*?\\s*{w2}")))
      if (is_empty(matched_crt)){
        validaton_vec <- append(validaton_vec, FALSE)
      }else{
        for (mat_sent in matched_crt){
          # Removing the space in prefix words of a string to match
          sent <- mat_sent
          mat_sent <- gsub("free of","freeof", mat_sent)
          mat_sent <- gsub("free from","freefrom", mat_sent)
          mat_sent <- gsub("made without","madewithout", mat_sent)
          mat_sent <- gsub("left out","leftout", mat_sent)
          mat_sent <- gsub("free from","freefrom", mat_sent)
          w1 <- gsub(" ","",w1)
          w2 <- gsub(" ","",w2)
          x <- strsplit(mat_sent, " ")[[1]]
          vad1 <- grep(w1, x) 
          vad2 <- grep(w2, x)
          for (v1 in vad1){
            for (v2 in vad2){
              if(v1 - v2<threshold){
                validaton_vec <- append(validaton_vec, TRUE)
                req_sent <- sent
              }else{validaton_vec <- append(validaton_vec, FALSE)}
            }
          }
        }
      }
    }
  }
  
  if (any(validaton_vec)){
    return(TRUE)
    # return(c(TRUE, req_sent))
  }
  else{return(FALSE)} #(c(FALSE,NA))
}

additional_val_fn <- function(claim_text,main_log,Prefix_itx,Suffix_itx, free_t = T){
  prefix_log <- tolower(paste0("\\b",paste0(Prefix_itx,collapse = "\\b|\\b"),"\\b"))
  suffix_log <- tolower(paste0("\\b",paste0(Suffix_itx,collapse = "\\b|\\b"),"\\b"))
  main_log <- tolower(paste0("\\b",paste0(main_log,collapse = "\\b|\\b"),"\\b"))
  
  prefix_tax_words <- unique(str_extract_all(string = claim_text, pattern = prefix_log)[[1]])
  suffix_tax_words <- unique(str_extract_all(string = claim_text, pattern = suffix_log)[[1]])
  main_tax_words <- unique(str_extract_all(string = claim_text,pattern = main_log)[[1]])
  
  prf_suf_bol <- c()
  if (!is_empty(main_tax_words)){
    if (!is_empty(prefix_tax_words)){
      if(prefix_Suffix_val(claim_text,prefix_tax_words,main_tax_words,free_t)){
        return(TRUE)
      }
      else if(!is_empty(suffix_tax_words)){
        return(prefix_Suffix_val(claim_text,main_tax_words,suffix_tax_words,free_t))
      }else{return(TRUE)}
    }
    else if(!is_empty(suffix_tax_words)){
      return(prefix_Suffix_val(claim_text,main_tax_words,suffix_tax_words,free_t))
    }
    else{return(TRUE)}
  }else{return(TRUE)}
}

p2s_validator <- function(DT,boolean,condition){
  theme_upc <- c()
  print(condition)
  
  if (theme %in% ADD_CHECK_THEMES){
    add_val <- T
  }else{add_val <- F}
  
  boolean <- tolower(boolean)
  if (condition == "Simple OR"){
    mod_condition <- gsub("\\bor\\b", "|",
                          removePunctuation(boolean),
                          ignore.case = T)
    
    mod_condition <- unlist(reorder_Tax(mod_condition))
    print(mod_condition)
    matched_str_DT <- DT[str_detect(tolower(DT$OneSpaceClaim),
                                    tolower(mod_condition)),.(UPC,Claim)]
    # theme_upc <- unique(matched_str_DT$UPC)
    
    # mod_DT <- DT[DT$UPC %notin% unique(theme_upc),]
    add_condition <- mod_condition
  }
  else if(condition == "Single AND"){
    # Split the condition based on word "AND" 
    conditions <- unlist(Map(removePunctuation,str_split(boolean,"\\band\\b")))
    conditions <- str_squish(gsub("\\bor\\b","|",
                                 trimws(unlist(conditions)),
                                 ignore.case = T))
    
    conditions <- tolower(reorder_Tax(conditions))
    print(conditions)
    
    conditon_1 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[1]))
    conditon_2 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[2]))
    
    result_exp <- Map(any,(Map('*',conditon_1,conditon_2)))
    
    matched_str_DT <- DT[unlist(result_exp),.(UPC,Claim)]
    # theme_upc <- append(theme_upc,matched_str_DT$UPC)
    
    # theme_upc <- unique(append(theme_upc,DT[unlist(result_exp),]$UPC))
    
    mod_DT <- DT[DT$UPC %notin% unique(theme_upc),]
    DT[unlist(result_exp),.(UPC,Claim)]
    
    add_condition <- conditions[1]
  }
  else if(condition == "Single AND NOT"){
    print("Single AND NOT")
    conditions <- unlist(Map(removePunctuation,str_split(boolean,"\\band not\\b")))
    conditions <- gsub("\\bor\\b","|", unlist(trimws(conditions)),ignore.case = T)
    conditions <- reorder_Tax(conditions)
    print(conditions)
    
    conditon_1 <- lapply(df$updated, function(x) str_detect(unlist(x),conditions[1]))
    conditon_2 <- lapply(df$updated, function(x) str_detect(unlist(x),conditions[2],negate=TRUE))
    
    result_exp <- Map('*',conditon_1,conditon_2)
    theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
    
    mod_df <- df[df$UPC %notin% unique(theme_upc),]
    add_condition <- conditions[1]
  }
  else if(condition == "AND and AND NOT"){
    print("AND and AND NOT")
    conditions <- unlist(str_split(removePunctuation(boolean),"\\band not\\b"))
    sub_condition <- unlist(str_split(conditions[1],"\\band\\b"))
    all_conditions <- removePunctuation(c(sub_condition[1],sub_condition[2],conditions[2]))
    conditions <- tolower(gsub("\\bor\\b","|", unlist(trimws(all_conditions)),ignore.case = T))
    conditions <- reorder_Tax(conditions)
    print(conditions)
    
    conditon_1 <- lapply(df$updated, function(x) str_detect(unlist(x),conditions[1]))
    conditon_2 <- lapply(df$updated, function(x) str_detect(unlist(x),conditions[2]))
    conditon_3 <- lapply(df$updated, function(x) str_detect(unlist(x),conditions[3], negate=TRUE))
    
    result_exp <- Map('*',conditon_1,conditon_2)
    result_exp <- Map('*',result_exp,conditon_3)
    theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
    
    mod_df <- df[df$UPC %notin% unique(theme_upc),]
    add_condition <- conditions[1]
  }
  else if(condition == "Double AND"){
    conditions <- unlist(Map(removePunctuation,str_split(boolean,"\\band\\b")))
    conditions <- gsub("\\bor\\b","|",trimws(unlist(conditions)),ignore.case = T)
    conditions <- tolower(reorder_Tax(conditions))
    print(conditions)
    
    conditon_1 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[1]))
    conditon_2 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[2]))
    conditon_3 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[3]))
    result_exp <- Map('*',conditon_1,conditon_2)
    result_exp <- Map('*',result_exp,conditon_3)
    theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
    
    mod_df <- df[df$UPC %notin% unique(theme_upc),]
    add_condition <- conditions[1]
  }
  else if(condition=="Double closing bracket"){
    split_conds <- str_trim(strsplit(condition,"\\)\\)"),perl=T)
    if(str_starts(split_conds[2],"\\bor\\b")){
      cond <- str_trim(gsub("or","",split_conds[2]))
      mod_condition <- gsub("\\bor\\b", "|",
                            removePunctuation(cond),
                            ignore.case = T)
      
      mod_condition <- unlist(reorder_Tax(mod_condition))
      # print(mod_condition)
      theme_upc <- unique(DT[str_detect(tolower(DT$OneSpaceClaim),
                                        tolower(mod_condition)),]$UPC)
      
      mod_DT <- DT[DT$UPC %notin% unique(theme_upc),]
    }else{mod_DT <- DT}
    if(str_contains(split_cond[1],"\\band not\\b")){
      sub_split_conds <- unlist(Map(removePunctuation,
                                    str_split(split_cond[1],
                                              "\\band not\\b")))
      
      conditions <- gsub("\\bor\\b","|",trimws(unlist(sub_split_conds)),ignore.case = T)
      conditions <- tolower(reorder_Tax(conditions))
      
      conditon_1 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[1]))
      conditon_2 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[2],negate=TRUE))
      add_condition <- conditions[1]
    }
    else if(str_contains(split_cond[1],"\\band\\b")){
      conditions <- unlist(Map(removePunctuation,str_split(boolean,"\\band\\b")))
      conditions <- gsub("\\bor\\b","|",trimws(unlist(conditions)),ignore.case = T)
      conditions <- tolower(reorder_Tax(conditions))
      print(conditions)
      
      conditon_1 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[1]))
      conditon_2 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[2]))
      if (!is.na(conditions[3])){
        conditon_3 <- lapply(DT$updated, function(x) str_detect(unlist(x),conditions[3]))
        conditon_2 <- Map('*',conditon_3,conditon_2)
      }
      add_condition <- conditions[1]
    }
    result_exp <- Map('*',conditon_1,conditon_2)
    matched_str_DT <- mod_DT[unlist(Map(any, result_exp)),.(UPC,Claim)]
    theme_upc <- append(theme_upc,matched_str_DT$UPC)
    
    mod_DT <- mod_DT[mod_DT$UPC %notin% unique(theme_upc),]
  }
  else{
    tmp9 <- boolean
    
    boolean <- str_replace(gsub("\\s+", " ", str_trim(boolean)), "B", "b")
    boolean <- gsub(paste0(".*?($|'|", paste(paste0("\\", keep_theme), collapse = "|"),
                                 "|[^[:punct:]]).*?"), "\\1", boolean)
    
    boolean_split <- unlist(strsplit(boolean, " or ", fixed = T))
    boolean_split <- unlist(strsplit(boolean_split, " and not ", fixed = T))
    boolean_split <- unlist(strsplit(boolean_split, c(" and "), fixed = T))
    
    boolean_split <- trimws(boolean_split)
    boolean_split <- unique(boolean_split)
    boolean_split <- gsub("-", " ",boolean_split)
    
    boolean_split <- boolean_split[order(-nchar(boolean_split))]
    
    
    print(paste0("length of the taxonomy: ", length(boolean_split)))
    
    tmp9 <- gsub("\\bor\\b", " | ", tmp9)
    tmp9 <- gsub("\\band not\\b", " &! ", tmp9)
    tmp9 <- gsub("\\band\\b", " & ", tmp9)
    tmp9 <- gsub('”', '',gsub('“', '”',tmp9))
    tmp9 <- gsub("+","", tmp9, fixed = T)
    tmp9 <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                        "|[^[:punct:]]).*?"), "\\1", tmp9)
    tmp9 <- str_replace(gsub("\\s+", " ", str_trim(tmp9)), "B", "b")
    tmp10 <- tmp9
    
    #### direct match for every theme
    tic("direct match")
    map_use_1 <- map_use
    #print(paste0("Mapping taxonomy of ", colnames(DataFrame2)[j], " with ", "claims"))
    for (f in 1:length(boolean_split)){
      # print(boolean_split[f])
      map_use_1[[boolean_split[f]]] <- NA
      map_use_1[[boolean_split[f]]] <- 
        ifelse(grepl(paste0("\\b",boolean_split[f], "\\b"), 
                     tolower(map_use_1$OneSpaceClaim)), 1, 0)
    }
    print("Gate operations starting.......")
    
    
    colnames(map_use_1) <- gsub("+","", colnames(map_use_1), fixed = T)
    colnames(map_use_1) <- gsub('”', '',gsub('“', '”',colnames(map_use_1), fixed = T), fixed = T)
    colnames(map_use_1) <- str_replace(gsub("\\s+", " ", str_trim(colnames(map_use_1))), "B", "b")
    
    for (g in 3:ncol(map_use_1)) {
      tmp9 <- gsub(paste0("\\b", colnames(map_use_1)[g], "\\b"),
                   paste0("map_use_1", "[[", g, "]]"), tmp9)
    }
    map_use_1$output <- eval(parse(text = tmp9))
    theme_upc <- append(theme_upc, map_use_1[map_use_1$output,]$UPC)
    # print(paste0("length of upc: ", length(theme_upc)))
    mod_df <- map_use_1[map_use_1$UPC %notin% unique(theme_upc),]
  }
  
  # Validate the prefix, suffix condition
  if (add_val){
    add_val_DT <- DT[UPC = theme_upc, ]
    add_vald <- lapply(add_val_DT$updated, 
                       function(x) additional_val_fn(unlist(x),add_condition))
    
    theme_upc <- unique(add_val_DT[unlist(add_vald),]$UPC)
  }
}



# bol_taxonomy <- read_excel("D:/Nestle/UPC to Theme Tagging Code/Boolean theme categorization.xlsx",sheet = "Sheet2")


# setDT(bol_taxonomy)
# bol_taxonomy <- bol_taxonomy[,"Abb.Theme.Boolean":= custom_abbreviate(`Theme Boolean`)]
# bol_taxonomy <- bol_taxonomy[,"Condition":= condition_finder(`Theme Boolean`)]
# bol_taxonomy <- bol_taxonomy[,.(`Theme Boolean`, Abb.Theme.Boolean,Theme,Condition)]

# fwrite(bol_taxonomy, "C:/Users/bharath.kancharla/Desktop/custom_abbr.csv")
