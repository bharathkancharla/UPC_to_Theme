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
    filter(!word1 %in% stopwords_2) %>%
    filter(!word2 %in% stopwords_2) %>%
    filter(!word3 %in% stopwords_2) %>%
    filter(!word4 %in% stopwords_2) %>%
    unite(word, word1, word2, word3, word4, sep = " ")
  tmp <- unique(tmp)
  return(tmp)
}



# DataFrame = OneSpace_Claims_Aggr
# ColumnNameinQuotes <- "OneSpace.Claim.Text"

lemmatize <- function(DataFrame, ColumnNameinQuotes){
  corpus <- VCorpus(VectorSource(DataFrame[[ColumnNameinQuotes]]))
  corpus <- tm_map(corpus, lemmatize_strings)
  corpus <- tm_map(corpus, PlainTextDocument)
  
  tmp2 <- data.frame(text=unlist(sapply(corpus, `[[`, "content")), 
                     stringsAsFactors=F)
  tmp2 <- as.data.frame(t(tmp2))
  DataFrame[ColumnNameinQuotes] <- tmp2$V1
  return(DataFrame)
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
Prefix <- tolower(as.character(Prefix$Prefix))

Suffix <- c("Free", "Diet", "intolerance", "formula", "not", "intolerant", "allergy")
Suffix <- data.frame(Suffix = as.character(Suffix))
Suffix <- lemmatize(Suffix,"Suffix")
Suffix <- tolower(as.character(Suffix$Suffix))

both_PS <- c("sensitivity", "added", "sensitive")
both_PS <- data.frame(both_PS = as.character(both_PS))
both_PS <- lemmatize(both_PS,"both_PS")
both_PS <- tolower(as.character(both_PS$both_PS))

All_negative <- unique(c(Prefix, Suffix, both_PS))




Back_up_file <- data.frame()
TaxoNomy_Mapping <- function(DataFrame2, ColumnNameinQuotes2, FolderName2){
  tic("TaxoNomy_Mapping function")
  for (k in unique(TaxoNomy$Theme)) {
    DataFrame2[k] <- NA
  }
  map_use <- DataFrame2[, which(colnames(DataFrame2) %in% c("UPC", ColumnNameinQuotes2))]
  colnames(map_use) <- c("UPC", "claim")
  map_use$claim <- as.character(map_use$claim)
  keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
  keep_theme <- c("'","+")
  colnames(map_use) <- c("UPC",   "OneSpaceClaim")
  
  for (j in 3:ncol(DataFrame2)) {
    Back_up_file <- DataFrame2
    # if (colnames(DataFrame2)[j] %in% c("Lifestage (ALL)", "Senior Pets", "Clean/Insect/Novel Protein (ALL)", "Hydration")){
    #   j=j+1
    # }
    print(paste0(j, "/", ncol(DataFrame2), " - ", colnames(DataFrame2)[j]))
    Theme.Boolean <- TaxoNomy$Theme.Boolean[TaxoNomy$Theme==colnames(DataFrame2)[j]]
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
    tmp9 <- gsub("+","", tmp9, fixed = T)
    tmp9 <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                        "|[^[:punct:]]).*?"), "\\1", tmp9)
    tmp9 <- str_replace(gsub("\\s+", " ", str_trim(tmp9)), "B", "b")
    tmp10 <- tmp9
    
    #### direct match for every theme
    tic("direct match")
    map_use_1 <- map_use
    print(paste0("Mapping taxonomy of ", colnames(DataFrame2)[j], " with ", "claims"))
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
    map_use_2 <- map_use_1
    map_use_1$output <- eval(parse(text = tmp9))
    DataFrame2[,j] <- ifelse(map_use_1$output==TRUE,1,0)
    toc()
    if (colnames(DataFrame2)[j] %in% NoIssueTheme){
      Main_words <- intersect(as.character(Theme.Boolean_split), All_negative)
      Main_words <- Theme.Boolean_split[!Theme.Boolean_split %in% Main_words]
      Prefix_itx <- intersect(as.character(Theme.Boolean_split), Prefix)
      Suffix_itx <- intersect(as.character(Theme.Boolean_split), Suffix)
      Both_itx <- intersect(as.character(Theme.Boolean_split), both_PS)
      
      exception_list <- unique(c(paste0(Main_words, "free"), paste0("no", Main_words)))
      exception_list <- Main_words[Main_words %in% exception_list]
      
      Main_words <- Main_words[!Main_words %in% exception_list]
    }
    
    
    tic("ngram after direct match")
    if (colnames(DataFrame2)[j] %in% free_form_theme){
      for (n in 1:nrow(DataFrame2)){
        if (DataFrame2[n,j]==1){
          # print(n)
          DtText <- data.frame(col1=DataFrame2[[ColumnNameinQuotes2]][n])
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
          if (colnames(DataFrame2)[j] %in% NoIssueTheme){
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
              DataFrame2[n,j] <- 1
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
                  DataFrame2[n,j] <- 1
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
                    DataFrame2[n,j] <- 1
                  } else {
                    DataFrame2[n,j] <- 0
                  }
                }
              }
                
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
            
            DataFrame2[n,j] <- ifelse(any(trigram_list$output==T)==TRUE,1,0)
          }
        }
      }
    }  
    toc()
  }
  
  # write.csv(DataFrame2, paste0("C:/Emerging Claims/", FolderName2,
  #                              "/Enriched Taxonomy/", FolderName2, 
  #                              "_Taxonomy_Themes_test.csv"), row.names = F)
  
  # DataFrame2 <- DataFrame2 %>% group_by(UPC) %>% summarise_all(sum)
  
  DataFrame2_Overall <- DataFrame2 
  DataFrame2_Overall[[ColumnNameinQuotes2]] <- NULL
  # DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(sum)
  # DataFrame2_Overall[,2:ncol(DataFrame2_Overall)] <- ifelse(DataFrame2_Overall[,
  #                                       2:ncol(DataFrame2_Overall)]>=1,1,0)
  DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(max)
  # DataFrame2_Overall <- setDF(DataFrame2_Overall)
  DataFrame2_Overall <- as.data.frame(colSums(DataFrame2_Overall[-1], na.rm = T))
  colnames(DataFrame2_Overall) <- "No.of.UPC"
  DataFrame2_Overall$Themes <- row.names(DataFrame2_Overall)
  row.names(DataFrame2_Overall) <- NULL
  DataFrame2_Overall <- DataFrame2_Overall %>% select(Themes, No.of.UPC)
  colnames(DataFrame2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
  toc()
  return(list(DataFrame2, DataFrame2_Overall))
}

# Summary_fn <- function(DataFrame2_Overall, FolderName2){
#   DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(sum)
#   DataFrame2_Overall[,2:ncol(DataFrame2_Overall)] <- ifelse(DataFrame2_Overall[,
#                                                      2:ncol(DataFrame2_Overall)]>=1,1,0)
#   DataFrame2_Overall <- as.data.frame(colSums(DataFrame2_Overall[-1], na.rm = T))
#   colnames(DataFrame2_Overall) <- "No.of.UPC"
#   DataFrame2_Overall$Themes <- row.names(DataFrame2_Overall)
#   row.names(DataFrame2_Overall) <- NULL
#   DataFrame2_Overall <- DataFrame2_Overall %>% select(Themes, No.of.UPC)
#   colnames(DataFrame2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
#   return(DataFrame2_Overall)
# }



# tmp <- Plain_themes_map(Nielsen_Dt, "Nielsen_column", "Nielsen")
# tmp <- TaxoNomy_Mapping(OneSpace_Claims_Aggr, "OneSpace.Claim.Text", "OneSpace Claims")
# 
# lemma_dictionary <- make_lemma_dictionary(OneSpace_Claims_Aggr_main$OneSpace.Claim.Text, engine = 'hunspell', lang = TaxDict)
# lemmas <- lexicon::hash_lemmas[token=="colorings",lemma:="color"]

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

Nestle_comaprision_summary <- function(Data2, Nestle_Data){
  common_UPC <- intersect(unique(Data2$UPC), unique(Nestle_Data$UPC))
  Data2 <- gather(Data2, theme, flag, colnames(Data2)[1:ncol(Data2)], -UPC)
  Data2$theme <- tolower(ifelse(Data2$theme=="Holisitc.Food",
                                "Holistic.Food", Data2$theme))
  comparison <- merge(Data2, Nestle_Data, by = c("UPC", "theme"), all = T)
  
  theme_list <- unique(comparison$theme)
  output_table <- data.frame()
  for (d in theme_list){
    tmp4 <- comparison[comparison$theme==d,]
    tmp5 <- tmp4[tmp4$UPC %in% common_UPC,]
    tmp5$common <- ifelse(tmp5$flag==1 & tmp5$Nestle.Mapping.flag==1, 1,0)
    
    tmp_table <- data.frame(Theme = d, UPC_tagged_Tiger = sum(tmp4$flag, na.rm = T),
                            UPC_tagged_Nestle = sum(tmp5$Nestle.Mapping.flag, na.rm = T),
                            UPC_tagged_Tiger_common_UPC = sum(tmp5$flag, na.rm = T),
                            Common = sum(tmp5$common, na.rm = T))
    tmp_table$`%_accuracy` <- tmp_table$Common/tmp_table$UPC_tagged_Nestle
    tmp_table$Tiger_additional <- tmp_table$UPC_tagged_Tiger - tmp_table$Common
    tmp_table$Missed_from_Nestle <- tmp_table$UPC_tagged_Nestle - tmp_table$Common
    if (nrow(output_table)==0){
      output_table <- tmp_table
    } else {
      output_table <- rbind(output_table, tmp_table)
    }
  }
  return(output_table)
}

# Functions for paragraph to sentence 
reorder_Tax <- function(Tax_conditions){
  cond_list <- list()
  for (cond in Tax_conditions){
    cond <- str_squish(cond)
    cond_split <- unique(stri_remove_empty_na(trimws(unlist(str_split(cond,"\\|")))))
    cond_split_ord <- cond_split[order(-nchar(cond_split))]
    # new_cond_Split <- character()
    # for (word in cond_split_ord){
    #   if (nchar(word)<4){
    #     word <- paste0("\\b",word,"\\b")
    #     new_cond_Split <- append(new_cond_Split, word)
    #   }else {new_cond_Split <- append(new_cond_Split, word)}
    #   
    # }
    cond_list <- append(cond_list,paste0("\\b",paste0(cond_split_ord,collapse = "\\b|\\b"),"\\b"))
  }
  return(unlist(cond_list))
}

Taxonomy_mapping_p2s<- function(DataFrame2, ColumnNameinQuotes2, FolderName2){
  
  tic("Taxonomy mapping paragraph to sentence function")
  df <- DataFrame2[, which(colnames(DataFrame2) %in% c("UPC", ColumnNameinQuotes2))]
  colnames(df) <- c("UPC", "claim")
  df$claim <- as.character(df$claim)
  keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
  keep_theme <- c("'","+")
  colnames(df) <- c("UPC",   "OneSpaceClaim")
  map_use <- df
  map_use$OneSpaceClaim <- str_squish(tolower(removePunctuation(map_use$OneSpaceClaim)))
  
  negative_word_list <- c("allergen", "allergic", 
                          "allergenic","made without",	"without","none","non", 
                          "never", "left out", "avoid", "unwanted", "Diet","dont", 
                          "intolerance", "formula", "not", "intolerant", "allergy",
                          "sensitivity", "added", "sensitive", "no", "does not contain",
                          "leave out", "does not", "No", "Low", "Zero","does not"
  )
  
  common_words <- c("free","health","food","certified","verified")
  common_words_exp = paste0(common_words,collapse = "|")
  negative_exp <- paste0(negative_word_list,collapse = "|")
  
  df_main <- data.frame(df)
  df <- unique(df)
  
  # df <- df %>% group_by(UPC) %>% 
    # summarise(OneSpaceClaim = paste(OneSpaceClaim, collapse = ". "))
  
  df$OneSpaceClaim <- str_squish(as.character(df$OneSpaceClaim))
  # df$OneSpace.Claim.Text <- gsub("+","",df$OneSpace.Claim.Text)
  df$updated <- lapply(df$OneSpaceClaim, 
                       function(x) str_squish(removePunctuation(tolower(sent_detect_nlp(x)))))
  
  df$OneSpaceClaim <- str_squish(tolower(removePunctuation(df$OneSpaceClaim)))
  
  `%notin%` <- Negate(`%in%`)
  and_not_themes <- TaxoNomy[str_detect(TaxoNomy$Category,"AND NOT"),]$Theme
  free_themes <- TaxoNomy[str_detect(tolower(TaxoNomy$Theme),"free"),]$Theme
  check_themes <- TaxoNomy[str_detect(TaxoNomy$Category,"CHECK"),]$Theme
  common_word_themes <- TaxoNomy[str_detect(tolower(TaxoNomy$Theme),common_words_exp,negate=T),]$Theme
  
  for (row in 1:nrow(TaxoNomy)){
    theme_upc <- c()
    Theme <- as.character((TaxoNomy$Theme[row]))
    Theme.Boolean <- as.character(TaxoNomy$Theme.Boolean[row])
    Theme.Boolean <- str_squish(tolower(Theme.Boolean))
    Theme.Boolean <- trimws(str_squish(gsub("'", "", Theme.Boolean)))
    
    cat <- as.character(TaxoNomy$Category[row])
    print(Theme)
    
    if (str_detect(tolower(Theme),"free",negate=T)){
      negative_exp <- paste0(negative_exp,"|free")
      negative_exp <- reorder_Tax(negative_exp)
      df$updated <- lapply(df$updated, function(x) unlist(x[str_detect(unlist(x),negative_exp,negate=T)]))
    }
    
    
    if(str_detect(Theme.Boolean,"and",negate=T)){
      print("Simple OR")
      mod_condition <- gsub("\\bor\\b", "|",removePunctuation(Theme.Boolean),ignore.case = T)
      mod_condition <- unlist(reorder_Tax(mod_condition))
      print(mod_condition)
      theme_upc <- df[str_detect(tolower(df$OneSpaceClaim),tolower(mod_condition)),]$UPC
      mod_df <- df[df$UPC %notin% unique(theme_upc),]
    }
    else if(trimws(cat)=="Single AND"){
      print("Single AND")
      #condition_split <- str_split(Theme.Boolean,"\\bAND\\b")
      conditons <- unlist(Map(removePunctuation,str_split(Theme.Boolean,"\\band\\b")))
      conditons <- gsub("\\bor\\b","|",trimws(unlist(conditons)),ignore.case = T)
      conditons <- tolower(reorder_Tax(conditons))
      print(conditons)
      
      conditon_1 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[1]))
      conditon_2 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[2]))
      
      result_exp <- Map('*',conditon_1,conditon_2)
      theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
      
      mod_df <- df[df$UPC %notin% unique(theme_upc),]
    }
    else if(trimws(cat)=="Single AND NOT"){
      print("Single AND NOT")
      conditons <- unlist(Map(removePunctuation,str_split(Theme.Boolean,"\\band not\\b")))
      conditons <- gsub("\\bor\\b","|", unlist(trimws(conditons)),ignore.case = T)
      conditons <- reorder_Tax(conditons)
      print(conditons)
      
      conditon_1 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[1]))
      conditon_2 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[2],negate=TRUE))
      
      result_exp <- Map('*',conditon_1,conditon_2)
      theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
      
      mod_df <- df[df$UPC %notin% unique(theme_upc),]
    }
    else if(trimws(cat)=="AND and AND NOT"){
      print("AND and AND NOT")
      conditons <- unlist(str_split(removePunctuation(Theme.Boolean),"\\band not\\b"))
      sub_condition <- unlist(str_split(conditons[1],"\\band\\b"))
      all_conditions <- removePunctuation(c(sub_condition[1],sub_condition[2],conditons[2]))
      conditons <- tolower(gsub("\\bor\\b","|", unlist(trimws(all_conditions)),ignore.case = T))
      conditons <- reorder_Tax(conditons)
      print(conditons)
      
      conditon_1 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[1]))
      conditon_2 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[2]))
      conditon_3 <- lapply(df$updated, function(x) str_detect(unlist(x),conditons[3], negate=TRUE))
      
      result_exp <- Map('*',conditon_1,conditon_2)
      result_exp <- Map('*',result_exp,conditon_3)
      theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
      
      mod_df <- df[df$UPC %notin% unique(theme_upc),]
    } 
    else if(str_detect(cat,"Double closing bracket")){
      print("Double closing bracket")
      if (startsWith(Theme.Boolean, "((", trim=TRUE)){
        brack_condititon <- regmatches(Theme.Boolean, gregexpr("\\({1,3}(.*)\\){2}",
                                                               Theme.Boolean, perl=T))[[1]]
      }else if (startsWith(Theme.Boolean, "( (", trim=TRUE)){
        brack_condititon <- regmatches(Theme.Boolean, gregexpr("\\(\\s\\((.*)\\)\\s\\)",
                                                               Theme.Boolean, perl=T))[[1]]
      }
      else if (startsWith(Theme.Boolean, "(", trim=TRUE)){
        brack_condititon <- regmatches(Theme.Boolean, gregexpr("\\([^\\)]*\\)",
                                                               Theme.Boolean, perl=T))[[1]]
      }
      print(paste0("bracket conditions: ",brack_condititon))
      temp_cond <- Theme.Boolean
      for (cond in brack_condititon){
        temp_cond <- gsub(removePunctuation(cond),"",
                                   removePunctuation(temp_cond))
      }
      left_out_condition <- trimws(removePunctuation(gsub("\\band\\b|\\band not\\b","",temp_cond)))
      print(paste0("left_out conditions: ",left_out_condition))
      left_out_condition <- reorder_Tax(gsub("\\bor\\b","|",trimws(left_out_condition),ignore.case = T))
      if (left_out_condition != ""){
        left_out_condition <- reorder_Tax(gsub("\\bor\\b","|",trimws(left_out_condition),ignore.case = T))
        theme_upc <- df[str_detect(tolower(df$OneSpaceClaim),tolower(mod_condition)),]$UPC
        mod_df_i <- df[df$UPC %notin% unique(theme_upc),]
      }else{mod_df_i <- df}
      
      for (cond in brack_condititon){
        new_conditions <- unlist(strsplit(cond,"\\band not\\b|\\band\\b",perl = T))
        new_conditions_upd <- trimws(removePunctuation(new_conditions))
        new_conditions_upd <- reorder_Tax(gsub("\\bor\\b","|",new_conditions_upd))
        print(paste0("New conditions: ",new_conditions_upd))
        sub_logic <- character()
        result_exp <- c(TRUE)
        for (j in 1:length(new_conditions)){
          if (j==length(new_conditions)){break}
          sub_logic <- trimws(unlist(ex_between(brack_condititon,
                                                new_conditions[j],new_conditions[j+1])))
          print(paste0("sub logic: ", sub_logic))
          conditon_1 <- lapply(mod_df_i$updated, function(x) str_detect(unlist(x),new_conditions_upd[j]))
          conditon_2 <- lapply(mod_df_i$updated, function(x) str_detect(unlist(x),new_conditions_upd[j+1]))
          if (trimws(removePunctuation(sub_logic)) == "and not"){conditon_2 <- Map("!",conditon_2)}
          interm_exp <- Map('*',conditon_1,conditon_2)
          result_exp <- Map('*',result_exp,interm_exp)
        }
        theme_upc <- append(theme_upc,mod_df_i[unlist(Map(any, result_exp)),]$UPC)
        
        mod_df <- mod_df_i[mod_df_i$UPC %notin% unique(theme_upc),]
        
      }
      # brack_condititon <- regmatches(Theme.Boolean, gregexpr("((?=\\().*?(?<=\\)))", 
      #                                                        Theme.Boolean, perl=T))[[1]]
      # print(brack_condititon)
      # new_brack_condition <- character()
      # for (i in brack_condititon){
      #   if (str_detect(brack_condititon,"\\bAND NOT\\b")|str_detect(brack_condititon,"\\bAND\\b")){
      #     new_brack_condition <- append(new_brack_condition,
      #                                   unlist(str_split(brack_condititon[i],
      #                                                    "\\bAND NOT\\b|\\bAND\\b")))
      #     
      #   }else{new_brack_condition <- append(new_brack_condition, brack_condititon[i])}
      # }
      # 
      # condition_list <- character()
      # for (i in length(new_brack_condition)-1){
      #   condition <- ex_between(Theme.Boolean, new_brack_condition[i], new_brack_condition[i+1])[[1]]
      #   condition <- trimws(removePunctuation(condition))
      #   condition_list <- append(condition_list,condition)
      # }
      # 
      # brack_condititon <- removePunctuation(brack_condititon)
      # Theme.Boolean <- removePunctuation(Theme.Boolean)
      # for (cond in brack_condititon){
      #   Theme.Boolean <- gsub(cond,"",Theme.Boolean)
      #   Theme.Boolean <- str_squish(gsub("\\bAND NOT\\b|\\bAND\\b","",Theme.Boolean))
      # }
      # left_out_cond <- str_squish(gsub("\\bOR\\b","|",
      #                                  trimws(gsub("^OR","",trimws(Theme.Boolean))),ignore.case = T))
      # left_out_cond <- reorder_Tax(left_out_cond)
      # print(left_out_cond)
      # 
      # if (left_out_cond !=""){
      #   theme_upc <- df[str_detect(tolower(df$OneSpace.Claim.Text),tolower(left_out_cond)),]$UPC
      #   mod_df_i <- df[df$UPC %notin% unique(theme_upc),]
      #   # theme_upc <- append(theme_upc,mod_df_i$UPC)
      # }
      # 
      # brack_condititons <- gsub("\\bOR\\b","|",trimws(unlist(new_brack_condition)),ignore.case = T)
      # brack_condititons <- reorder_Tax(brack_condititons)
      # 
      # for (j in length(brack_condititon)-1){
      #   print(brack_condititon[j])
      #   if (str_detect(condition_list[j],"AND NOT")) {
      #     conditon_1 <- lapply(mod_df_i$updated, 
      #                          function(x) str_detect(unlist(x),brack_condititons[j]))
      #     conditon_2 <- lapply(mod_df_i$updated, 
      #                          function(x) str_detect(unlist(x),brack_condititons[j+1],negate = T))
      #     interm_exp <- Map('*',conditon_1,conditon_2)
      #     result_exp <- Map('*',interm_exp,result_exp)
      #   }else if (str_detect(condition_list[j],"AND")){
      #     conditon_1 <- lapply(mod_df_i$updated, 
      #                          function(x) str_detect(unlist(x),brack_condititons[j]))
      #     conditon_2 <- lapply(mod_df_i$updated, 
      #                          function(x) str_detect(unlist(x),brack_condititons[j+1]))
      #     interm_exp <- Map('*',conditon_1,conditon_2)
      #     result_exp <- Map('*',interm_exp,result_exp)
      #   }
      # }
      # 
      # theme_upc <- append(theme_upc,df[unlist(Map(any, result_exp)),]$UPC)
      # 
      # mod_df <- df[df$UPC %notin% unique(theme_upc),]
    }
    else if(trimws(cat)=="Double AND"){mod_df <- df}
    else {
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
      tmp9 <- gsub("+","", tmp9, fixed = T)
      tmp9 <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                          "|[^[:punct:]]).*?"), "\\1", tmp9)
      tmp9 <- str_replace(gsub("\\s+", " ", str_trim(tmp9)), "B", "b")
      tmp10 <- tmp9
      
      #### direct match for every theme
      tic("direct match")
      map_use_1 <- map_use
      #print(paste0("Mapping taxonomy of ", colnames(DataFrame2)[j], " with ", "claims"))
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
      map_use_1$output <- eval(parse(text = tmp9))
      theme_upc <- append(theme_upc, map_use_1[map_use_1$output,]$UPC)
      # print(paste0("length of upc: ", length(theme_upc)))
      mod_df <- map_use_1[map_use_1$UPC %notin% unique(theme_upc),]
      #print(paste0("No of rows left after modification: ", nrow(mod_df)))
    }
    
    #check directly the theme name
    # theme_upc <- df[str_detect(tolower(df$OneSpace.Claim.Text),tolower(Theme)),]$UPC
    # mod_df <- df[df$UPC %notin% unique(theme_upc),]
    add_exc_themes <- c("Human Grade","Air Dried","High Protein","Humanely Raised",
                        "Plant Based Protein","Senior Pets", 
                        "Organ Protein and Organ Meat", "Allergy Management",
                        "Skin and Coat","Bone Health","Puppy/Kitten",
                        "Animal Welfare (ALL)","Artificial- Total")
    
    exclude_themes <- unique(c(and_not_themes,free_themes,check_themes,add_exc_themes,common_word_themes))
    theme <- tolower(gsub("-"," ",gsub(" \\(ALL\\)$|\\(|\\)","",as.character(Theme))))
    if (Theme %notin% exclude_themes){
      #split the words in theme name validate it 
      if (sapply(strsplit(theme," "),length)>1){
        temp_theme_list <- unique(unlist(str_split(str_squish(gsub("\\band\\b|\\bor\\b","",
                                                            tolower(gsub("/","|",theme))))," ")))
        print(paste0("temporary theme list: ", temp_theme_list))
        #Covers double AND
        evaluate <- c(TRUE)
        for (theme_word in temp_theme_list){
          # theme_exp <- eval(parse(text = paste0(temp_theme, collapse = "&")))
          interm_list <- lapply(mod_df$updated, 
                                function(x) (str_detect(unlist(x),paste0("\\b",theme_word,"\\b"))))
          if (length(interm_list)!=0){evaluate <- Map('*',interm_list, evaluate)}
        }
        
        theme_upc <- append(theme_upc,mod_df[unlist(Map(any, evaluate)),]$UPC)
        
        mod_df_1 <- mod_df[mod_df$UPC %notin% unique(theme_upc),]
      }else{mod_df_1<- mod_df}
    }else{mod_df_1<- mod_df}
    
    if (str_detect(theme,"free$") & (theme %notin% tolower(and_not_themes))){
      theme <- str_squish(gsub("/","|",theme))
      modified_theme <- trimws(gsub("free","",theme))
      negative_exp <- paste0(negative_exp,"|free")
      negative_exp <- reorder_Tax(negative_exp)
      
      if(str_detect(modified_theme,"\\band\\b")){
        split_theme <- trimws(unlist(strsplit(modified_theme, "\\band\\b")))
        split_1 <- lapply(mod_df_1$updated, 
                          function(x) str_detect(unlist(x),paste0("\\b",split_theme[1],"\\b")))
        split_2 <- lapply(mod_df_1$updated, 
                          function(x) str_detect(unlist(x),paste0("\\b",split_theme[2],"\\b")))
        
        interm_1 <- Map('*',split_1,split_2)
      }else{interm_1 <- lapply(mod_df_1$updated, 
                               function(x) str_detect(unlist(x), paste0("\\b",modified_theme,"\\b")))}
      
      interm_2 <- lapply(mod_df_1$updated, function(x) str_detect(unlist(x),negative_exp))
      
      result_exp <- Map('*',interm_1,interm_2)
      theme_upc <- append(theme_upc,mod_df_1[unlist(Map(any, result_exp)),]$UPC)
      
      mod_df_2 <- mod_df_1[mod_df_1$UPC %notin% unique(theme_upc),]
    }else{mod_df_2 <- mod_df_1}
    
    # sub_df <- mod_df_2[str_detect(tolower(mod_df_2$OneSpace.Claim.Text), Theme.Boolean),]
    # sub_df <- sub_df[unlist(lapply(sub_df$updated,
    #                                function(x) any(str_detect(unlist(x),Theme.Boolean)))),]
    df_main[Theme] <- ifelse(df_main$UPC %in% unique(theme_upc), 1, 0)
    # print(paste0("sum: ", sum(df_main[Theme])))
  }
  
  # write.csv(df_main, "D:/Nestle/Emerging/onespaceoutput_070520_1.csv",row.names = F)
  names(df_main)[names(df_main) == "OneSpaceClaim"] <- ColumnNameinQuotes2
  DataFrame2_Overall <- df_main 
  DataFrame2_Overall[[ColumnNameinQuotes2]] <- NULL
  # DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(sum)
  # DataFrame2_Overall[,2:ncol(DataFrame2_Overall)] <- ifelse(DataFrame2_Overall[,
  #                                       2:ncol(DataFrame2_Overall)]>=1,1,0)
  DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(max)
  # DataFrame2_Overall <- setDF(DataFrame2_Overall)
  DataFrame2_Overall <- as.data.frame(colSums(DataFrame2_Overall[-1], na.rm = T))
  colnames(DataFrame2_Overall) <- "No.of.UPC"
  DataFrame2_Overall$Themes <- row.names(DataFrame2_Overall)
  row.names(DataFrame2_Overall) <- NULL
  DataFrame2_Overall <- DataFrame2_Overall %>% select(Themes, No.of.UPC)
  colnames(DataFrame2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
  toc()
  return(list(df_main, DataFrame2_Overall))
}