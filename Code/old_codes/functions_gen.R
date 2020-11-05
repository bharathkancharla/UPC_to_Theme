packages_required <- c("readxl", "dplyr", "stringr", "tm", "textstem", "hunspell", "tictoc", "stringi",
                       "quanteda", "tidytext", "tidyr", "textclean", "openxlsx", "wordnet", "udpipe",
                       "data.table", "qdap")
new.packages <- packages_required[!(packages_required %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(packages_required, require, character.only = TRUE)
rm(packages_required)

taxonomy_validation<-function(str1,str2){
  a<-as.character(TaxoNomy[TaxoNomy$Theme==str1,"Theme.Boolean"])
  b<-a
  c<-(tolower(str2))
  c<-str_replace_all(c,pattern = "[[:punct:]]",replacement = " ")
  c<-str_squish(c)
  
  d<-as.character(TaxoNomy[TaxoNomy$Theme==str1,"d"])
  
  #d
  e<-str_squish(unlist(strsplit(d,split="\\$")))
  e<-rev(e[order(nchar(e), e)])
  #e
  #c
  l1<-c()
  for(i in e)
  {
    if(grepl(paste("\\b",i,"\\b",sep=""),c)){
      #print(i)
      l1<-c(l1,i)
      b<-str_replace_all(b,pattern =paste("\\b",i,"\\b",sep=""),replacement = "1")
    }else{
      # print("No")
      b<-str_replace_all(b,pattern =paste("\\b",i,"\\b",sep=""),replacement = "0")
    }
  }
  #b
  # print("Result:")
  l1<-c(eval(parse(text=b)),l1)
  return(paste(l1,collapse =";"))
}


ngram_validation_Os<-function(description,Theme_Name){
  #tic("ngram")
  
  d1<-unlist(tokenize_ngrams(description, n = 4L))
  help1<-c()
  for(i in d1)
  {
    i<-str_squish(i)
    # print(i)
    x<-taxonomy_validation(Theme_Name,i)
    if(unlist(strsplit(x,split=";"))[1]=="TRUE")
    {
      prev_element<-d1[max(0,which(d1==i)-4)]
      pres_element<-i
      next_element<-d1[min(length(d1),which(d1==i)+4)]
      sen<-paste(prev_element,pres_element,next_element)
      help1<-c(help1,sen)
    }
  }
  return(paste(help1,collapse=" $$ "))
  #toc()
}

stopwords_2 <- read.csv("stopwords_2.csv", stringsAsFactors = F, na.strings = c("", "NA"))

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

Validation_Function <- function(Os, ColumnNameinQuotes2){
  y=0
  for(j in paste("result", colnames(d8)[3:ncol(d8)], sep="_")){
    y = y+1
    print(paste0(y, "/", ncol(d8)-2, " - ", j))
    Theme <- unlist(strsplit(j,split="_"))[2]
    uncommon_UPCs <- new_only_upcs[new_only_upcs$Theme == Theme,]
    new_UPCs <- unique(trimws(unlist(strsplit(as.character(uncommon_UPCs$new_only[1]), split = ","))))
    old_UPCs <- unique(trimws(unlist(strsplit(as.character(uncommon_UPCs$old_only[1]), split = ","))))
    uncommon_UPCs <- unique(c(new_UPCs, old_UPCs))
    if (length(uncommon_UPCs) == 0){
      d8_tmp <- d8 %>% select(index, UPC, Theme)
      if (Theme %in% colnames(Os)){
        colnames(d8_tmp) <- c("index", "UPC", paste0(Theme, "_1"))
        name_conflict <- T
      } else{
        name_conflict <- F
      }
      Os_tmp <- merge(Os, d8_tmp, by = c("index", "UPC"), all = F)
      if (nrow(Os_tmp)==nrow(Os)){
        Os <- merge(Os, d8_tmp, by = c("index", "UPC"), all = F)
        rm(Os_tmp)
        if (name_conflict==T){
          Os[,j] <- ifelse(Os[,paste0(Theme, "_1")]==1, "Common Tags", "Not tagged")
          Os[, paste0(Theme, "_1")] <- NULL
        } else{
          Os[,j] <- ifelse(Os[,Theme]==1, "Common Tags", "Not tagged")
          Os[,Theme] <- NULL
        }
        
        Os <- Os[order(Os$index),]
        rm(d8_tmp)
      } else {
        stop("Os merged with d8 gone wrong!!!")
      }
      
    } else {
      Os_1 <- Os %>% filter(!UPC %in% uncommon_UPCs)
      d8_tmp <- d8 %>% select(index, UPC, Theme)
      if (Theme %in% colnames(Os)){
        colnames(d8_tmp) <- c("index", "UPC", paste0(Theme, "_1"))
        name_conflict <- T
      } else{
        name_conflict <- F
      }
      Os_1 <- merge(Os_1, d8_tmp, by = c("index", "UPC"), all = F)
      if (name_conflict==T){
        Os_1[,j] <- ifelse(Os_1[,paste0(Theme, "_1")]==1, "Common Tags", "Not tagged")
        Os_1[, paste0(Theme, "_1")] <- NULL
      } else{
        Os_1[,j] <- ifelse(Os_1[,Theme]==1, "Common Tags", "Not tagged")
        Os_1[,Theme] <- NULL
      }
      Os_2 <- Os %>% filter(UPC %in% uncommon_UPCs)
      Os_2 <- merge(Os_2, d8_tmp, by = c("index", "UPC"), all = F)
      
      for(i in 1:nrow(Os_2)){
        l1 <- taxonomy_validation(Theme, Os_2[i,ColumnNameinQuotes2])
        Os_2[i,j] <- l1
      }
      if (name_conflict==T){
        Os_2[, paste0(Theme, "_1")] <- NULL
      } else{
        Os_2[,Theme] <- NULL
      }
      Os <- rbind(Os_1, Os_2)
      rm(Os_1, Os_2)
      Os <- Os[order(Os$index),]
      #### N-gram search
      if (Theme %in% make.names(free_form_theme)){
        print("Direct match done, N-gram Starting....")
        Os$tmp <- sapply(strsplit(Os[[j]], ";"), "[", 1)
        Os_tmp <- Os %>% filter(tmp == "TRUE")
        Os_2 <- Os %>% filter(tmp != "TRUE")
        
        if (nrow(Os_tmp)>0){
          Os_tmp$tmp <- NULL
          Os_2$tmp <- NULL
          Os$tmp <- NULL
          for (k in 1:nrow(Os_tmp)){
            DtText <- data.frame(col1=Os_tmp[[ColumnNameinQuotes2]][k])
            DtText$col1 <- as.character(DtText$col1)
            words_tokens <- unlist(strsplit(DtText$col1, " "))
            words_tokens <- words_tokens[!words_tokens %in% stopwords_2]
            if (length(words_tokens)<= 4){
              trigram_list <- paste0(words_tokens, collapse = " ")
              trigram_list <- data.frame(word = trigram_list)
            } else{
              trigram_list <- four_gram(DtText)
            }
            d1 <- unique(trigram_list$word)
            rm(trigram_list)
            help1<-c()
            for(p in d1){
              i<-str_squish(p)
              # print(i)
              x<-taxonomy_validation(Theme,i)
              if(unlist(strsplit(x,split=";"))[1]=="TRUE"){
                prev_element<-d1[max(0,which(d1==p)-4)]
                pres_element<-p
                next_element<-d1[min(length(d1),which(d1==p)+4)]
                sen<-paste(prev_element,pres_element,next_element)
                help1<-c(help1,sen)
              }
            }
            if (length(help1)==0){
              l1 <- "FALSE FROM N-GRAM"
            } else {
              l1 <- paste("TRUE: N-gram", help1, collapse = ";")
            }
            Os_tmp[k,j] <- l1
          }
          Os <- rbind(Os_tmp, Os_2)
          rm(Os_tmp, Os_1, Os_2)
          Os <- Os[order(Os$index),]
        }
      }
    }
  }
  Os<-Os[,!names(Os) %in% l1_Os]
  Os_1 <- Os[, c(2,3, which(grepl("result_", colnames(Os))))] 
  return(Os)
  toc()
}
library(pryr)
Themes_116 <- read.csv("Themes_116.csv", stringsAsFactors = F, na.strings = c("", "NA"))
Themes_116 <- Themes_116$x
