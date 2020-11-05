## Adding the Synonyms of Negative words
TaxoNomy$Theme.Boolean <- gsub(" no ", " no or never or not or none ", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub(" without ", " without or leave out ", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub(" free from ", " free from or free of ", TaxoNomy$Theme.Boolean)


TaxoNomy_whole <- TaxoNomy %>% group_by() %>% summarise(Theme.Boolean = paste(Theme.Boolean, collapse = " "))
TaxoNomy_whole <- as.character(TaxoNomy_whole)
TaxoNomy_whole <- gsub(")","",gsub("(", ")", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub(",","",gsub(".", ",", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub("\r","",gsub("\n", "\r", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub("[[:punct:]]", " ", TaxoNomy_whole)
TaxoNomy_whole <- tolower(TaxoNomy_whole)
TaxoNomy_whole <- unlist(strsplit(TaxoNomy_whole, " "))
TaxoNomy_whole <- unique(TaxoNomy_whole)
TaxoNomy_whole <- TaxoNomy_whole[!TaxoNomy_whole %in% c("or", "and", "")]


stopwords_1 <- stopwords()
stopwords_1 <- unlist(strsplit(stopwords_1, " "))
stopwords_1 <- unique(stopwords_1)
tmp <- intersect(TaxoNomy_whole, stopwords())
stopwords_2 <- setdiff(stopwords_1, tmp)

stopwords_2 <- stopwords_2[stopwords_2 != "of"]
stopwords_2 <- c(stopwords_2, "and", "or", "the")

replace_misspelling <- function(x, ...){
  
  lower <- text <- replacement <- is_cap <- final <- element_id <- token_id <- NULL
  if (!(is.character(x) | is.factor(x))) stop('`x` must be a character vector')
  is_na <- is.na(x)
  dat <- data.frame(text = as.character(x), stringsAsFactors = FALSE)
  
  token_df <- textshape::split_token(dat, lower = FALSE)[, lower := tolower(text)]
  
  tokens <- grep('[a-z]', unique(token_df[['lower']]), value = TRUE)
  hits <- !hunspell::hunspell_check(tokens, dict = TaxDict)
  
  misspelled <- tokens[hits]
  
  map <- data.table::data.table(
    lower = misspelled,
    replacement = unlist(lapply(hunspell::hunspell_suggest(misspelled, dict = TaxDict), `[`, 1))
  )
  
  fixed_df <- map[token_df, on = "lower"]
  
  fixed_df_a <- fixed_df[!is.na(replacement),][,
                                               is_cap := substring(text, 1, 1) %in% LETTERS][,
                                                                                             final := ifelse(is_cap,  upper_first_letter(replacement), replacement)][]
  
  fixed_df_b <- fixed_df[is.na(replacement),][, final := text][]
  
  bound <- rbind(fixed_df_a, fixed_df_b, fill = TRUE)
  
  out <- data.table::setorder(bound, element_id, token_id)[, 
                                                           list(`final` = paste(final, collapse = ' ')), by = 'element_id'][,
                                                                                                                            `final` := gsub("(\\s+)([.!?,;:])", "\\2", final, perl = TRUE)][['final']]
  out[is_na] <- NA
  out
}

upper_first_letter <- function(x){
  substring(x, 1, 1) <- toupper(substring(x, 1, 1))
  x
}

bigram <- function(given.text){
  tmp <- given.text %>% 
    tidytext::unnest_tokens(word, OneSpace.Claim.Text, token = "ngrams", n=2) %>% 
    separate(word, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stopwords_2) %>%
    filter(!word2 %in% stopwords_2) %>%
    unite(word, word1, word2, sep = " ")
  return(tmp)
}
trigram <- function(given.text){
  given.text <- unlist(strsplit(given.text$col1, " "))
  given.text <- given.text[!given.text %in% stopwords_2]
  given.text <- data.frame(col1 = paste(given.text, collapse = " "))
  
  tmp <- given.text %>% 
    tidytext::unnest_tokens(word, col1, token = "ngrams", n=3) %>% 
    separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
    filter(!word1 %in% stopwords_2) %>%
    filter(!word2 %in% stopwords_2) %>%
    filter(!word3 %in% stopwords_2) %>%
    unite(word, word1, word2, word3, sep = " ")
  tmp <- unique(tmp)
  return(tmp)
}
four_gram <- function(given.text){
  given.text <- unlist(strsplit(given.text$col1, " "))
  given.text <- given.text[!given.text %in% stopwords_2]
  given.text <- data.frame(col1 = paste(given.text, collapse = " "))
  
  tmp <- given.text %>% 
    tidytext::unnest_tokens(word, col1, token = "ngrams", n=4) %>% 
    separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>% 
    unite(word, word1, word2, word3, word4, sep = " ")
  # filter(!word1 %in% stopwords_2) %>%
  # filter(!word2 %in% stopwords_2) %>%
  # filter(!word3 %in% stopwords_2) %>%
  # filter(!word4 %in% stopwords_2) %>%
  tmp <- unique(tmp)
  return(tmp)
}



# DataFrame = OneSpace_Claims_Aggr
# ColumnNameinQuotes <- "OneSpace.Claim.Text"

lemmatize <- function(DataFrame, ColumnNameinQuotes){
  # corpus <- VCorpus(VectorSource(DataFrame[[ColumnNameinQuotes]]))
  corpus <- VCorpus(VectorSource(DataFrame[[ColumnNameinQuotes]]))
  corpus <- tm_map(corpus, lemmatize_strings)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  tmp2 <- data.frame(text=unlist(sapply(corpus, `[[`, "content")), 
                     stringsAsFactors=F)
  tmp2 <- as.data.frame(t(tmp2))
  # DataFrame[ColumnNameinQuotes] <- tmp2$V1
  # return(setDT(DataFrame))
  return(tmp2$V1)
}

############## Plain themes mapping function
# Plain_themes_map(Ingredients_Dt_Plain, "Ingredient.Name", "Ingredients")
# # Plain_themes_map <- function(DataFrame1, ColumnNameinQuotes1, FolderName){
#   DataFrame1 <- DataFrame1[unique(DataFrame1$UPC) %in%
#                              unique(OneSpace_Claims_Plain$UPC),]
#   DataFrame1[[ColumnNameinQuotes1]] <- tolower(DataFrame1[[ColumnNameinQuotes1]])
#   DataFrame1[[ColumnNameinQuotes1]] <- lapply(DataFrame1[[ColumnNameinQuotes1]], 
#                                               function(y) gsub("[[:punct:]]", "", y))
#   DataFrame1[[ColumnNameinQuotes1]] <- str_replace(gsub("\\s+", " ", str_trim(DataFrame1[[ColumnNameinQuotes1]])), "B", "b")
#   
#   for (k in unique(TaxoNomy_Theme$Theme)) {
#     DataFrame1[k] <- NA
#   }
#   for (i in 3:ncol(DataFrame1)) {
#     DataFrame1[,i] <- ifelse(grepl(paste0("\\b", 
#                                           colnames(DataFrame1)[i], "\\b"), 
#                                    DataFrame1[[ColumnNameinQuotes1]]) == TRUE,1,0)
#   }
#   DataFrame1$UPC <- as.character(DataFrame1$UPC)
#   Mapped_Nestle_dcasted$UPC <- as.character(Mapped_Nestle_dcasted$UPC)
#   Mapped_Nestle_dcasted_common <- 
#     Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
#                             unique(DataFrame1$UPC),]
#   DataFrame1_with_Nestle <- merge(DataFrame1, Mapped_Nestle_dcasted_common,
#                                   by = "UPC", all = T)
#   write.csv(DataFrame1_with_Nestle, paste0("C:/Emerging Claims/",
#            FolderName, "/Plain Themes/", FolderName, "_Plain_Themes_v2.csv"), 
#            row.names = F)
#   
#   DataFrame1_Plain_Overall <- DataFrame1 %>% select(-ColumnNameinQuotes1)
#   DataFrame1_Plain_Overall <- as.data.frame(colSums(DataFrame1_Plain_Overall[-1]))
#   colnames(DataFrame1_Plain_Overall) <- "No.of.UPC"
#   DataFrame1_Plain_Overall$Themes <- row.names(DataFrame1_Plain_Overall)
#   row.names(DataFrame1_Plain_Overall) <- NULL
#   DataFrame1_Plain_Overall <- DataFrame1_Plain_Overall %>% select(Themes, No.of.UPC)
#   colnames(DataFrame1_Plain_Overall) <- c("Themes", paste0(FolderName,"_No.of.UPC"))
#   
#   return(list(DataFrame1_with_Nestle, DataFrame1_Plain_Overall))
# }


##### Prefix/ Suffix list 
Prefix <- c("No", "Low", "Zero",	"Free of", "allergen", "allergic", "allergenic", "free from", "free of",
            "made without",	"without", "non", "never", "none", "left out", "avoid", "unwanted")
Prefix <- data.frame(Prefix = as.character(Prefix))
Prefix <- lemmatize(Prefix,"Prefix")
# Prefix <- tolower(as.character(Prefix$Prefix))

Suffix <- c("Free", "Diet", "intolerance", "formula", "not", "intolerant", "allergy")
Suffix <- data.frame(Suffix = as.character(Suffix))
Suffix <- lemmatize(Suffix,"Suffix")
# Suffix <- tolower(as.character(Suffix$Suffix))

both_PS <- c("sensitivity", "added", "sensitive")
both_PS <- data.frame(both_PS = as.character(both_PS))
both_PS <- lemmatize(both_PS,"both_PS")
# both_PS <- tolower(as.character(both_PS$both_PS))

All_negative <- unique(c(Prefix, Suffix, both_PS))

backup_dt <- data.table()
TaxoNomy_Mapping <- function(DataTable2, ColumnNameinQuotes2, FolderName2){
  basic_cols <- ncol(DataTable2)
  cols_req <- c("UPC", ColumnNameinQuotes2)
  map_use <- copy(DataTable2[,..cols_req])
  colnames(map_use) <- c("UPC","OneSpaceClaim")
  map_use$OneSpaceClaim <- as.character(map_use$OneSpaceClaim)
  
  # Create a empty colums
  Keys_DataTable2 <- copy(DataTable2)
  Keys_DataTable2 <- Keys_DataTable2[,str_glue("{unique(TaxoNomy$Theme)}_match_terms"):=character()]
  DataTable2 <- DataTable2[,unique(TaxoNomy$Theme):=character()]
  row_id <- DataTable2[, .I]
  DataTable2 <- cbind(row_id, DataTable2)
  
  keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
  keep_theme <- c("'","+")
  
  # Cage free 
  free_themes <- TaxoNomy[grep("free$", as.character(tolower(TaxoNomy$Theme))),Theme]
  
  for (j in 5:ncol(DataTable2)) {
    # if (colnames(DataTable2)[j] %in% c("Lifestage (ALL)", "Senior Pets", "Clean/Insect/Novel Protein (ALL)", "Hydration")){
    #   j=j+1
    # }
    print(paste0(j, "/", ncol(DataTable2), " - ", colnames(DataTable2)[j]))
    Theme.Boolean <- TaxoNomy$Theme.Boolean[TaxoNomy$Theme==colnames(DataTable2)[j]]
    Theme.Boolean <- tolower(Theme.Boolean)
    Theme.Boolean <- gsub("'", "", Theme.Boolean)
    tmp9 <- Theme.Boolean
    
    
    Theme.Boolean <- str_replace(gsub("\\s+", " ", str_trim(Theme.Boolean)), "B", "b")
    Theme.Boolean <- gsub(paste0(".*?($|'|", paste(paste0("\\", keep_theme), collapse = "|"),
                                 "|[^[:punct:]]).*?"), "\\1", Theme.Boolean)
    
    Theme.Boolean_split <- unlist(strsplit(Theme.Boolean, " or ", fixed = T))
    Theme.Boolean_split <- unlist(strsplit(Theme.Boolean_split, " and not ", fixed = T))
    Theme.Boolean_split <- unlist(strsplit(Theme.Boolean_split, c(" and "), fixed = T))
    
    Theme.Boolean_split <- trimws(Theme.Boolean_split)
    Theme.Boolean_split <- unique(Theme.Boolean_split)
    Theme.Boolean_split <- gsub("-", " ",Theme.Boolean_split)
    
    Theme.Boolean_split <- Theme.Boolean_split[order(-nchar(Theme.Boolean_split))]
    
    print(paste0("length of the taxonomy: ", length(Theme.Boolean_split)))
    
    tmp9 <- gsub("\\bor\\b", " | ", tmp9)
    tmp9 <- gsub("\\band not\\b", " &! ", tmp9)
    tmp9 <- gsub("\\band\\b", " & ", tmp9)
    tmp9 <- gsub('”', '',gsub('“', '”',tmp9))
    tmp9 <- gsub('"','',tmp9)
    tmp9 <- gsub("+","", tmp9, fixed = T)
    tmp9 <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                        "|[^[:punct:]]).*?"), "\\1", tmp9)
    tmp9 <- str_replace(gsub("\\s+", " ", str_trim(tmp9)), "B", "b")
    tmp10 <- tmp9
    
    #### direct match for every theme
    tic("direct match")
    map_use_1 <- copy(map_use)
    print(paste0("Mapping taxonomy of ", colnames(DataTable2)[j], " with ", "claims"))
    for (f in 1:length(Theme.Boolean_split)){
      # print(Theme.Boolean_split[f])
      map_use_1[[Theme.Boolean_split[f]]] <- NA
      map_use_1[[Theme.Boolean_split[f]]] <- 
        ifelse(grepl(paste0("\\b",Theme.Boolean_split[f], "\\b"), 
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
    map_use_2 <- copy(map_use_1)
    map_use_1$output <- eval(parse(text = tmp9))
    DataTable2[[j]] <- ifelse(map_use_1$output==TRUE,1,0)
    toc()
    
    # map_use_1$val <- lapply(apply(map_use_1[3:ncol(map_use_2)], 1, function(x) which(x==1)), names)
    
    
    # 	if (colnames(DataTable2)[j] %in% NoIssueTheme){
    #       Main_words <- intersect(as.character(Theme.Boolean_split), All_negative)
    #       Main_words <- Theme.Boolean_split[!Theme.Boolean_split %in% Main_words]
    #       Prefix_itx <- intersect(as.character(Theme.Boolean_split), Prefix)
    #       Suffix_itx <- intersect(as.character(Theme.Boolean_split), Suffix)
    #       Both_itx <- intersect(as0.character(Theme.Boolean_split), both_PS)
    #       
    #       exception_list <- unique(c(paste0(Main_words, "free"), paste0("no", Main_words)))
    #       exception_list <- Main_words[Main_words %in% exception_list]
    #       
    #       Main_words <- Main_words[!Main_words %in% exception_list]
    #     }
    
    ### Paragraph to Sentence Approach - for specific themes
    matched_ids <- DataTable2[,which(DataTable2[[j]]==1)]
    if (colnames(DataTable2)[j] %in% free_themes){
      Main_words <- intersect(as.character(Theme.Boolean_split), All_negative)
      Main_words <- Theme.Boolean_split[!Theme.Boolean_split %in% Main_words]
      Prefix_itx <- intersect(as.character(Theme.Boolean_split), Prefix)
      Suffix_itx <- intersect(as.character(Theme.Boolean_split), Suffix)
      Both_itx <- intersect(as.character(Theme.Boolean_split), both_PS)
      
      exception_list <- unique(c(paste0(Main_words, "free"), paste0("no", Main_words)))
      exception_list <- Main_words[Main_words %in% exception_list]
      
      Main_words <- Main_words[!Main_words %in% exception_list]
      
      if (is_empty(matched_ids)){
        DataTable2[[j]] <- 0
      }else{
        sub_datatable <- DataTable2[row_id %in% matched_ids, .(row_id, UPC, raw_claim)]
        system.time(for (n in 1:nrow(sub_datatable)){
          DtText <- as.character(sub_datatable[["raw_claim"]][[n]])
          cond <- additional_val_fn(DtText, Main_words, Prefix_itx, Suffix_itx, free_t=T)
          if (cond){
            DataTable2[row_id == sub_datatable[["row_id"]][[n]],j] <- 1
          }
          else{
            DataTable2[row_id == sub_datatable[["row_id"]][[n]],j] <- 0
          }
        })
        rm(sub_datatable) 
      }
    }
    else{
      tic("ngram after direct match")
      if (colnames(DataTable2)[j] %in% free_form_theme){
        for (n in 1:nrow(DataTable2)){
          if (DataTable2[n,j,with=FALSE]==1){
            # print(n)
            DtText <- data.frame(col1=DataTable2[[ColumnNameinQuotes2]][[n]])
            DtText$col1 <- as.character(DtText$col1)
            words_tokens <- unlist(strsplit(DtText$col1, " "))
            words_tokens <- words_tokens[!words_tokens %in% stopwords_2]
            if (length(words_tokens)<= 4){
              trigram_list <- paste0(words_tokens, collapse = " ")
              trigram_list <- data.frame(word = trigram_list)
            } else{
              trigram_list <- four_gram(DtText)
            }
            trigram_list <- unique(trigram_list)
            for (f in 1:length(Theme.Boolean_split)){
              # print(Theme.Boolean_split[f])
              trigram_list[[Theme.Boolean_split[f]]] <- NA
              trigram_list[[Theme.Boolean_split[f]]] <- 
                ifelse(grepl(paste0("\\b",Theme.Boolean_split[f], "\\b"), 
                             tolower(trigram_list$word)), 1, 0)
            }
            if (colnames(DataTable2)[j] %in% NoIssueTheme){
              trigram_list$rowsum <- rowSums(trigram_list[,2:ncol(trigram_list)])
              trigram_list <- trigram_list %>% filter(rowsum>0) %>% select(-rowsum)
              trigram_list_tmp <- trigram_list
              
              colnames(trigram_list_tmp) <- ifelse(colnames(trigram_list_tmp) %in% Main_words, "Main_words",
                                                   ifelse(colnames(trigram_list_tmp) %in% Prefix_itx, "Prefix",
                                                          ifelse(colnames(trigram_list_tmp) %in% Suffix_itx, "Suffix",
                                                                 ifelse(colnames(trigram_list_tmp) %in% exception_list, "Exception",
                                                                        ifelse(colnames(trigram_list_tmp) %in% Both_itx, "both",NA)))))
              colnames(trigram_list_tmp)[1] <- "word"
              row.names(trigram_list_tmp) <- trigram_list_tmp$word
              trigram_list_tmp$word <- NULL
              trigram_list_tmp <- as.data.frame(t(rowsum(t(trigram_list_tmp), group = colnames(trigram_list_tmp), na.rm = T)))
              trigram_list_tmp[trigram_list_tmp>=1] <- 1
              if ("both" %in% colnames(trigram_list_tmp)){
                trigram_list_tmp$Main_both <- ifelse(trigram_list_tmp$both==1 & trigram_list_tmp$Main_words==1,1,0)
              } else {
                trigram_list_tmp$Main_both <- NA
              }
              
              if (any(trigram_list_tmp$Exception == 1)==T | any(trigram_list_tmp$Main_both==1, na.rm = T)==T){
                DataTable2[n,j] <- 1
              } else {
                if ("Prefix" %in% colnames(trigram_list_tmp) & "Main_words" %in% colnames(trigram_list_tmp)){
                  trigram_list_tmp$Main_Prefix <- ifelse(trigram_list_tmp$Prefix==1 & trigram_list_tmp$Main_words==1,1,0)
                }
                if ("Suffix" %in% colnames(trigram_list_tmp) & "Main_words" %in% colnames(trigram_list_tmp)){
                  trigram_list_tmp$Main_Suffix <- ifelse(trigram_list_tmp$Suffix==1 & trigram_list_tmp$Main_words==1,1,0)
                }
                trigram_list_tmp <- trigram_list_tmp[trigram_list_tmp$Main_Prefix==1 | trigram_list_tmp$Main_Suffix==1,]
                if (nrow(trigram_list_tmp)>0){
                  trigram_list <- trigram_list[trigram_list$word %in% row.names(trigram_list_tmp),]
                  trigram_list_prfx <- trigram_list
                  trigram_list_sfx <- trigram_list
                  
                  
                  for (f in 1:length(Prefix_itx)){
                    tt <-  as.data.frame(stri_locate_first(pattern = Prefix_itx[f],
                                                           trigram_list_prfx$word, fixed = T)) %>% select(start)
                    if (nrow(tt)==0){
                      trigram_list_prfx[[Prefix_itx[f]]] <- NA
                    } else {
                      trigram_list_prfx[[Prefix_itx[f]]] <- tt$start
                    }
                  }
                  for (f in 1:length(Main_words)){
                    tt <-  as.data.frame(stri_locate_last(pattern = Main_words[f],
                                                          trigram_list_prfx$word, fixed = T)) %>% select(start)
                    if (nrow(tt)==0){
                      trigram_list_prfx[[Main_words[f]]] <- NA
                    } else {
                      trigram_list_prfx[[Main_words[f]]] <- tt$start
                    }
                  }
                  trigram_list_prfx <- trigram_list_prfx[, which(colnames(trigram_list_prfx) %in% c("word",Prefix_itx, Main_words))]
                  colnames(trigram_list_prfx) <- ifelse(colnames(trigram_list_prfx) %in% Main_words, "Main_words",
                                                        ifelse(colnames(trigram_list_prfx) %in% Prefix_itx, "Prefix",
                                                               ifelse(colnames(trigram_list_prfx) %in% Suffix_itx, "Suffix",
                                                                      ifelse(colnames(trigram_list_prfx) %in% exception_list, "Exception",
                                                                             ifelse(colnames(trigram_list_prfx) %in% Both_itx, "both",
                                                                                    ifelse(colnames(trigram_list_prfx) == "word", "word", NA))))))
                  # trigram_list_prfx_backup <- trigram_list_prfx
                  # trigram_list_prfx <- trigram_list_prfx_backup
                  
                  trigram_list_prfx_pfx <- trigram_list_prfx[, which(colnames(trigram_list_prfx)%in% c("word","Prefix"))]
                  row.names(trigram_list_prfx_pfx) <- trigram_list_prfx_pfx$word
                  trigram_list_prfx_pfx$word <- NULL
                  trigram_list_prfx_pfx$Prefix <- apply(trigram_list_prfx_pfx,1,FUN = min, na.rm = T)
                  trigram_list_prfx_pfx <- trigram_list_prfx_pfx %>% select(Prefix)
                  trigram_list_prfx_pfx$Prefix <- gsub(Inf, NA, trigram_list_prfx_pfx$Prefix)
                  trigram_list_prfx_pfx$word <- row.names(trigram_list_prfx_pfx)
                  row.names(trigram_list_prfx_pfx) <- NULL
                  
                  
                  trigram_list_prfx_main <- trigram_list_prfx[, which(colnames(trigram_list_prfx) %in% c("word", "Main_words"))]
                  row.names(trigram_list_prfx_main) <- trigram_list_prfx_main$word
                  trigram_list_prfx_main$word <- NULL
                  trigram_list_prfx_main$Main_words <- apply(trigram_list_prfx_main,1,FUN = min, na.rm = T)
                  trigram_list_prfx_main <- trigram_list_prfx_main %>% select(Main_words)
                  trigram_list_prfx_main$Main_words <- gsub(Inf, NA, trigram_list_prfx_main$Main_words)
                  trigram_list_prfx_main$word <- row.names(trigram_list_prfx_main)
                  row.names(trigram_list_prfx_main) <- NULL
                  
                  trigram_list_prfx <- merge(trigram_list_prfx_main, trigram_list_prfx_pfx, by = "word", all = F)
                  trigram_list_prfx <- na.omit(trigram_list_prfx)
                  trigram_list_prfx$output <- ifelse(as.numeric(trigram_list_prfx$Main_words) > as.numeric(trigram_list_prfx$Prefix),1,0)
                  
                  if (any(trigram_list_prfx$output==1)){
                    DataTable2[n,j] <- 1
                  } else {
                    for (f in 1:length(Suffix_itx)){
                      tt <-  as.data.frame(stri_locate_last(pattern = Suffix_itx[f],
                                                            trigram_list_sfx$word, fixed = T)) %>% select(start)
                      if (nrow(tt)==0){
                        trigram_list_sfx[[Suffix_itx[f]]] <- NA
                      } else {
                        trigram_list_sfx[[Suffix_itx[f]]] <- tt$start
                      }
                    }
                    for (f in 1:length(Main_words)){
                      trigram_list_sfx[[Main_words[f]]] <- NA
                      tt <-  as.data.frame(stri_locate_first(pattern = Main_words[f],
                                                             trigram_list_sfx$word, fixed = T)) %>% select(start)
                      if (nrow(tt)==0){
                        trigram_list_sfx[[Main_words[f]]] <- NA
                      } else {
                        trigram_list_sfx[[Main_words[f]]] <- tt$start
                      }
                    }
                    trigram_list_sfx <- trigram_list_sfx[, which(colnames(trigram_list_sfx) %in% c("word", Suffix_itx, Main_words))]
                    # colnames(trigram_list_sfx) <- gsub(".start", "", colnames(trigram_list_sfx), fixed = T)
                    colnames(trigram_list_sfx) <- ifelse(colnames(trigram_list_sfx) %in% Main_words, "Main_words",
                                                         ifelse(colnames(trigram_list_sfx) %in% Prefix_itx, "Prefix",
                                                                ifelse(colnames(trigram_list_sfx) %in% Suffix_itx, "Suffix",
                                                                       ifelse(colnames(trigram_list_sfx) %in% exception_list, "Exception",
                                                                              ifelse(colnames(trigram_list_sfx) %in% Both_itx, "both",
                                                                                     ifelse(colnames(trigram_list_sfx) == "word", "word", NA))))))
                    
                    trigram_list_sfx_sfx <- trigram_list_sfx[, which(colnames(trigram_list_sfx)%in% c("word", "Suffix"))]
                    row.names(trigram_list_sfx_sfx) <- trigram_list_sfx_sfx$word
                    trigram_list_sfx_sfx$word <- NULL
                    trigram_list_sfx_sfx$Suffix <- apply(trigram_list_sfx_sfx, 1, FUN = min, na.rm = T)
                    trigram_list_sfx_sfx <- trigram_list_sfx_sfx %>% select(Suffix)
                    trigram_list_sfx_sfx$Suffix <- gsub(Inf, NA, trigram_list_sfx_sfx$Suffix)
                    trigram_list_sfx_sfx$word <- row.names(trigram_list_sfx_sfx)
                    row.names(trigram_list_sfx_sfx) <- NULL
                    
                    
                    trigram_list_sfx_main <- trigram_list_sfx[, which(colnames(trigram_list_sfx) %in% c("word", "Main_words"))]
                    row.names(trigram_list_sfx_main) <- trigram_list_sfx_main$word
                    trigram_list_sfx_main$word <- NULL
                    trigram_list_sfx_main$Main_words <- apply(trigram_list_sfx_main,1,FUN = min, na.rm = T)
                    trigram_list_sfx_main <- trigram_list_sfx_main %>% select(Main_words)
                    trigram_list_sfx_main$Main_words <- gsub(Inf, NA, trigram_list_sfx_main$Main_words)
                    trigram_list_sfx_main$word <- row.names(trigram_list_sfx_main)
                    row.names(trigram_list_sfx_main) <- NULL
                    
                    trigram_list_sfx <- merge(trigram_list_sfx_main, trigram_list_sfx_sfx, by = "word", all = F)
                    trigram_list_sfx <- na.omit(trigram_list_sfx)
                    trigram_list_sfx$output <- ifelse(as.numeric(trigram_list_sfx$Main_words) < as.numeric(trigram_list_sfx$Suffix), 1, 0)
                    
                    if (any(trigram_list_sfx$output==1)){
                      DataTable2[n,j] <- 1
                    } else {
                      DataTable2[n,j] <- 0
                    }
                  }
                }
                else{DataTable2[n,j] <- 0}
                
                
              }
            } else {
              colnames(trigram_list) <- gsub("+","", colnames(trigram_list), fixed = T)
              colnames(trigram_list) <- gsub('”', '',gsub('“', '”',colnames(trigram_list), fixed = T), fixed = T)
              colnames(trigram_list) <- str_replace(gsub("\\s+", " ", str_trim(colnames(trigram_list))), "B", "b")
              
              for (g in 2:ncol(trigram_list)) {
                tmp10 <- gsub(paste0("\\b", colnames(trigram_list)[g], "\\b"),
                              paste0("trigram_list", "[[", g, "]]"), tmp10)
              }
              trigram_list$output <- eval(parse(text = tmp10))
              
              DataTable2[n,j] <- ifelse(any(trigram_list$output==T)==TRUE,1,0)
            }
          }
        }
        
      }
      
    } 
    
    if (all(map_use_1$output==0)){
      Keys_DataTable2[[str_glue("{colnames(DataTable2)[j]}_match_terms")]] <- NA
    }else{
      Keys_DataTable2[[str_glue("{colnames(DataTable2)[j]}_match_terms")]] <- lapply(lapply(apply(map_use_1[,3:ncol(map_use_2),with=FALSE], 1, function(x) which(x==1)), names),toString)
    }
  
    Keys_DataTable2$temp <- DataTable2[[j]]
    Keys_DataTable2[temp==0, str_glue("{colnames(DataTable2)[j]}_match_terms"):= NA]
    Keys_DataTable2[,temp:=NULL]
    backup_dt <- copy(DataTable2)
    toc()
  }
  
  # write.csv(DataTable2, paste0("C:/Emerging Claims/", FolderName2,
  #                              "/Enriched Taxonomy/", FolderName2, 
  #                              "_Taxonomy_Themes_test.csv"), row.names = F)
  
  # DataTable2 <- DataTable2 %>% group_by(UPC) %>% summarise_all(sum)
  DataTable2 <- DataTable2[, row_id := NULL]
  DataTable2_Overall <- copy(DataTable2[,c(ColumnNameinQuotes2,"raw_claim"):=NULL])
  # DataTable2_Overall <- DataTable2_Overall %>% group_by(UPC) %>% summarise_all(sum)
  # DataTable2_Overall[,2:ncol(DataTable2_Overall)] <- ifelse(DataTable2_Overall[,
  #                                       2:ncol(DataTable2_Overall)]>=1,1,0)
  DataTable2_Overall <- DataTable2_Overall[, lapply(.SD, max), by = UPC]
  # DataTable2_Overall <- DataTable2_Overall %>% group_by(UPC) %>% summarise_all(max)
  # DataTable2_Overall <- setDF(DataTable2_Overall)
  DataTable2_Overall <- as.data.frame(colSums(DataTable2_Overall[,UPC:=NULL],
                                              na.rm = T))
  colnames(DataTable2_Overall) <- "No.of.UPC"
  DataTable2_Overall$Themes <- row.names(DataTable2_Overall)
  row.names(DataTable2_Overall) <- NULL
  DataTable2_Overall <- DataTable2_Overall[,c("Themes", "No.of.UPC")]
  colnames(DataTable2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
  toc()
  return(list(DataTable2, DataTable2_Overall, Keys_DataTable2))
}

Summary_fn <- function(DataTable2_Overall, FolderName2){
  DataTable2_Overall <- DataTable2_Overall %>% group_by(UPC) %>% summarise_all(sum)
  DataTable2_Overall[,2:ncol(DataTable2_Overall)] <- ifelse(DataTable2_Overall[,
                                                                               2:ncol(DataTable2_Overall)]>=1,1,0)
  DataTable2_Overall <- as.data.frame(colSums(DataTable2_Overall[-1], na.rm = T))
  colnames(DataTable2_Overall) <- "No.of.UPC"
  DataTable2_Overall$Themes <- row.names(DataTable2_Overall)
  row.names(DataTable2_Overall) <- NULL
  DataTable2_Overall <- DataTable2_Overall %>% select(Themes, No.of.UPC)
  colnames(DataTable2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
  return(DataTable2_Overall)
}
