

TaxoNomy_whole <- TaxoNomy %>% group_by() %>% summarise(Theme.Boolean = paste(Theme.Boolean, collapse = " "))
TaxoNomy_whole <- as.character(TaxoNomy_whole)
TaxoNomy_whole <- gsub(")","",gsub("(", ")", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub(",","",gsub(".", ",", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub("\r","",gsub("\n", "\r", TaxoNomy_whole, fixed = T), fixed = T)
TaxoNomy_whole <- gsub("[[:punct:]]", " ", TaxoNomy_whole)
TaxoNomy_whole <- tolower(TaxoNomy_whole)
TaxoNomy_whole <- unlist(strsplit(TaxoNomy_whole, " "))
TaxoNomy_whole <- unique(TaxoNomy_whole)

stopwords_1 <- stopwords()
stopwords_1 <- unlist(strsplit(stopwords_1, " "))
stopwords_1 <- unique(stopwords_1)
tmp <- intersect(TaxoNomy_whole, stopwords())
stopwords_2 <- setdiff(stopwords_1, tmp)

stopwords_2 <- stopwords_2[stopwords_2 != "of"]


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



DataFrame = OneSpace_Claims_Aggr
ColumnNameinQuotes <- "OneSpace.Claim.Text"

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
Plain_themes_map(Ingredients_Dt_Plain, "Ingredient.Name", "Ingredients")
Plain_themes_map <- function(DataFrame1, ColumnNameinQuotes1, FolderName){
  DataFrame1 <- DataFrame1[unique(DataFrame1$UPC) %in%
                             unique(OneSpace_Claims_Plain$UPC),]
  DataFrame1[[ColumnNameinQuotes1]] <- tolower(DataFrame1[[ColumnNameinQuotes1]])
  DataFrame1[[ColumnNameinQuotes1]] <- lapply(DataFrame1[[ColumnNameinQuotes1]], 
                                              function(y) gsub("[[:punct:]]", "", y))
  DataFrame1[[ColumnNameinQuotes1]] <- str_replace(gsub("\\s+", " ", str_trim(DataFrame1[[ColumnNameinQuotes1]])), "B", "b")
  
  for (k in unique(TaxoNomy_Theme$Theme)) {
    DataFrame1[k] <- NA
  }
  for (i in 3:ncol(DataFrame1)) {
    DataFrame1[,i] <- ifelse(grepl(paste0("\\b", 
                                          colnames(DataFrame1)[i], "\\b"), 
                                   DataFrame1[[ColumnNameinQuotes1]]) == TRUE,1,0)
  }
  DataFrame1$UPC <- as.character(DataFrame1$UPC)
  Mapped_Nestle_dcasted$UPC <- as.character(Mapped_Nestle_dcasted$UPC)
  Mapped_Nestle_dcasted_common <- 
    Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
                            unique(DataFrame1$UPC),]
  DataFrame1_with_Nestle <- merge(DataFrame1, Mapped_Nestle_dcasted_common,
                                  by = "UPC", all = T)
  write.csv(DataFrame1_with_Nestle, paste0("C:/Emerging Claims/",
           FolderName, "/Plain Themes/", FolderName, "_Plain_Themes_v2.csv"), 
           row.names = F)
  
  DataFrame1_Plain_Overall <- DataFrame1 %>% select(-ColumnNameinQuotes1)
  DataFrame1_Plain_Overall <- as.data.frame(colSums(DataFrame1_Plain_Overall[-1]))
  colnames(DataFrame1_Plain_Overall) <- "No.of.UPC"
  DataFrame1_Plain_Overall$Themes <- row.names(DataFrame1_Plain_Overall)
  row.names(DataFrame1_Plain_Overall) <- NULL
  DataFrame1_Plain_Overall <- DataFrame1_Plain_Overall %>% select(Themes, No.of.UPC)
  colnames(DataFrame1_Plain_Overall) <- c("Themes", paste0(FolderName,"_No.of.UPC"))
  
  return(list(DataFrame1_with_Nestle, DataFrame1_Plain_Overall))
}


##### Prefix/ Suffix list 
Prefix <- c("No", "Low", "Zero",	"Free of", "allergen", "allergic", "allergenic", "free from",
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


## Adding the Synonyms of Negative words
TaxoNomy$Theme.Boolean <- gsub(" no ", " no or never or not or none ", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub(" without ", " without or leave out ", TaxoNomy$Theme.Boolean)



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
    
    
    Main_words <- intersect(as.character(Theme.Boolean_split), All_negative)
    Main_words <- Theme.Boolean_split[!Theme.Boolean_split %in% Main_words]
    Prefix_itx <- intersect(as.character(Theme.Boolean_split), Prefix)
    Suffix_itx <- intersect(as.character(Theme.Boolean_split), Suffix)
    Both_itx <- intersect(as.character(Theme.Boolean_split), both_PS)
    
    exception_list <- unique(c(paste0(Main_words, "free"), paste0("no", Main_words)))
    exception_list <- Main_words[Main_words %in% exception_list]
    
    Main_words <- Main_words[!Main_words %in% exception_list]
    
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
      print(Theme.Boolean_split[f])
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
    
    tic("ngram after direct match")
    if (colnames(DataFrame2)[j] %in% free_form_theme){
      for (n in 1:nrow(DataFrame2)){
        if (DataFrame2[n,j]==1){
          print(n)
          if(n==2){print("n=1 - ran successfully")}
          # print(n)
          DtText <- data.frame(col1=DataFrame2[[ColumnNameinQuotes2]][n])
          DtText$col1 <- as.character(DtText$col1)
          words_tokens <- unlist(strsplit(DtText$col1, " "))
          words_tokens <- words_tokens[!words_tokens %in% stopwords_2]
          if (length(words_tokens)<= 4){
            trigram_list <- DtText
            colnames(trigram_list) <- "word"
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
            trigram_list_tmp <- trigram_list
            trigram_list_tmp_idx <- trigram_list
            trigram_list_tmp_idx <- trigram_list_tmp_idx[, colSums(trigram_list_tmp_idx != 0) > 0]
           
            trigram_list_tmp_idx$Main_words <- NA
            trigram_list_tmp_idx$Prefix <- NA
            trigram_list_tmp_idx$Suffix <- NA
            Main_words_idx <- trigram_list_tmp_idx
            Main_words_idx <- as.data.frame(Main_words_idx[,1])
            colnames(Main_words_idx) <- c("key")
            Prefix_words_idx <- Main_words_idx
            Suffix_words_idx <- Main_words_idx
            for (s in 1:4){
              Main_words_idx[[paste0("col",s)]]  <- ifelse((word(trigram_list_tmp_idx$word, s) %in% Main_words)==T,s,NA)
              Prefix_words_idx[[paste0("col",s)]]  <- ifelse((word(trigram_list_tmp_idx$word, s) %in% Prefix_itx)==T,s,NA)
              Suffix_words_idx[[paste0("col",s)]]  <- ifelse((word(trigram_list_tmp_idx$word, s) %in% Suffix_itx)==T,s,NA)
              
            }
            Main_words_idx <- transform(Main_words_idx, min = pmin(col1, col2, col3, col4, na.rm = T))
            Main_words_idx <- transform(Main_words_idx, max = pmax(col1, col2, col3, col4, na.rm = T))
            Prefix_words_idx <- transform(Prefix_words_idx, min = pmin(col1, col2, col3, col4, na.rm = T))
            Suffix_words_idx <- transform(Suffix_words_idx, max = pmax(col1, col2, col3, col4, na.rm = T))
            Main_words_idx <- Main_words_idx %>% select(key, min, max)
            colnames(Main_words_idx)[2:ncol(Main_words_idx)] <- paste0("Main_", colnames(Main_words_idx)[2:ncol(Main_words_idx)])
            Prefix_words_idx <- Prefix_words_idx %>% select(key, min)
            colnames(Prefix_words_idx)[2:ncol(Prefix_words_idx)] <- paste0("Prefix_", colnames(Prefix_words_idx)[2:ncol(Prefix_words_idx)])
            Suffix_words_idx <- Suffix_words_idx %>% select(key, max)
            colnames(Suffix_words_idx)[2:ncol(Suffix_words_idx)] <- paste0("Suffix_", colnames(Suffix_words_idx)[2:ncol(Suffix_words_idx)])
            All_min_max <- merge(Main_words_idx, Prefix_words_idx, by = "key", all = F)
            All_min_max <- merge(All_min_max, Suffix_words_idx, by = "key", all = F)
            All_min_max$output1 <- ifelse(All_min_max$Main_max>All_min_max$Prefix_min, 1,NA)
            All_min_max$output2 <- ifelse(All_min_max$Main_min<All_min_max$Suffix_max,1,NA)
            All_min_max <- transform(All_min_max, output = pmax(output1, output2, na.rm = T))
            
            
           
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
            if (any(trigram_list_tmp$Exception == 1)==T){
              DataFrame2[n,j] <- 1
            } else if(any(trigram_list_tmp$Main_both==1, na.rm = T)==T) {
              DataFrame2[n,j] <- 1
            } else if(any(All_min_max$output==1, na.rm = T)==T) {
              DataFrame2[n,j] <- 1
            } else {
              DataFrame2[n,j] <- 0
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
  DataFrame2_Overall <- DataFrame2 %>% select(-ColumnNameinQuotes2)
  DataFrame2_Overall <- DataFrame2_Overall %>% group_by(UPC) %>% summarise_all(sum)
  DataFrame2_Overall[,2:ncol(DataFrame2_Overall)] <- ifelse(DataFrame2_Overall[,
                                        2:ncol(DataFrame2_Overall)]>=1,1,0)
  DataFrame2_Overall <- as.data.frame(colSums(DataFrame2_Overall[-1], na.rm = T))
  colnames(DataFrame2_Overall) <- "No.of.UPC"
  DataFrame2_Overall$Themes <- row.names(DataFrame2_Overall)
  row.names(DataFrame2_Overall) <- NULL
  DataFrame2_Overall <- DataFrame2_Overall %>% select(Themes, No.of.UPC)
  colnames(DataFrame2_Overall) <- c("Themes", paste0(FolderName2, "_No.of.UPC"))
  toc()
  return(list(DataFrame2, DataFrame2_Overall))
}



tmp <- Plain_themes_map(Nielsen_Dt, "Nielsen_column", "Nielsen")
tmp <- TaxoNomy_Mapping(OneSpace_Claims_Aggr, "OneSpace.Claim.Text", "OneSpace Claims")


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

