

TaxoNomy<-read_excel("Boolean theme categorization.xlsx", sheet = "Sheet2")
colnames(TaxoNomy)<-make.names(colnames(TaxoNomy))
TaxoNomy <- TaxoNomy %>% select(Theme.Boolean, Theme)
keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
keep_theme <- c("'","+")

TaxoNomy$Theme.Boolean <- tolower(TaxoNomy$Theme.Boolean)
#remove all of punctuation from Theme.Boolean and storing in d so as to extract all unique words
TaxoNomy$d<-str_replace_all(str_replace_all(TaxoNomy$Theme.Boolean,pattern = "[[:punct:]]",replacement = " "),pattern = "\\band not\\b",replacement = "$")
TaxoNomy$d<-tolower(str_replace_all(TaxoNomy$d,pattern = "\\bor\\b|\\band\\b",replacement = "$"))
TaxoNomy$d<-str_replace_all(TaxoNomy$d,"\\+","")
TaxoNomy$d<-str_replace_all(TaxoNomy$d,"~"," ")

#replacing all punctuation except brackets
TaxoNomy$Theme.Boolean<- gsub("(?![()])[[:punct:]]", " ", TaxoNomy$Theme.Boolean, perl=TRUE)
TaxoNomy$Theme.Boolean<-str_replace_all(TaxoNomy$Theme.Boolean,"~"," ")

#now creating sth like for ex if we have "nick and (john or george)" then form "nick &(john|george)" that's why remove all punc except brackets and then replace
TaxoNomy$Theme.Boolean<-str_replace_all(TaxoNomy$Theme.Boolean,pattern = "\\band not\\b",replacement = "&!")
TaxoNomy$Theme.Boolean<-str_replace_all(TaxoNomy$Theme.Boolean,pattern = "\\band\\b",replacement = "&")
TaxoNomy$Theme.Boolean<-str_replace_all(TaxoNomy$Theme.Boolean,pattern = "\\bor\\b",replacement = "|")


TaxoNomy$Theme.Boolean <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                                      "|[^[:punct:]]).*?"), "\\1", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- str_squish(TaxoNomy$Theme.Boolean)
#TaxoNomy$Theme<-str_squish(TaxoNomy$Theme)
TaxoNomy$Theme<-make.names(TaxoNomy$Theme)
s<-paste(as.character(TaxoNomy$d),collapse=" $ ")
s<-str_replace_all(string = s,pattern = "\r",replacement = "")
s<-str_replace_all(string = s,pattern = "\n",replacement = "")
l1_Os<-unique(str_squish(unlist(strsplit(s,split="\\$"))))


#***********************************************************************************************************
############################################ OneSapce Claims ###############################################
#***********************************************************************************************************

OneSpace_Claims.Nielsen <- readRDS("OneSpace_Analytics03232020.Rda")
OneSpace_Claims.Nielsen <- OneSpace_Claims.Nielsen %>% select(-c(Category,
                                                                 Segment, `Brand Expanded`,
                                                                 CLAIM_NAME))
OneSpace_Claims <- OneSpace_Claims.Nielsen %>% select(c(UPC, CLAIM_TEXT))
OneSpace_Claims <- unique(OneSpace_Claims)
colnames(OneSpace_Claims) <- c("UPC", "OneSpace.Claim.Text")

Os<-OneSpace_Claims
rownames(Os)<-NULL
rm(OneSpace_Claims, OneSpace_Claims.Nielsen)

d8<-read.csv("OneSpace_Claims_Aggr_mapped_81k_07_05_2020_latest.csv")
drops <- c("OneSpace.Claim.Text")
d8<-d8[ , !(names(d8) %in% drops)]
sum(is.na(d8))
d8$index <- as.numeric(row.names(d8))
d8 <- d8 %>% select(index, everything())
d8 <- d8[, which(colnames(d8) %in% c("index", "UPC", Themes_116))]


Os[,l1_Os]<-NA

Os$OneSpace.Claim.Text<-str_squish(str_replace_all(tolower(Os$OneSpace.Claim.Text),
                                                   pattern = "[[:punct:]]",replacement = " "))
#making a lookup table for all words of taxonomy
for(i in 3:length(colnames(Os))){
  if(i%%100==0){print(i)}
  Os[[colnames(Os)[i]]] <- grepl(paste("\\b", colnames(Os)[i], "\\b", sep=""), Os$OneSpace.Claim.Text)
}

Os[,paste("result", colnames(d8)[3:ncol(d8)], sep="_")]<-NA
Os$index <- as.numeric(row.names(Os))
Os <- Os %>% select(index, everything())
# Os_back_up <- Os
Os <- Os_back_up
new_only_upcs <- readRDS("OS_comarision_upc.rds")
new_only_upcs <- new_only_upcs %>% select(Theme, new_only, old_only)
Os[is.na(Os)] <- F


free_form_theme <- c("Allergy Management","Antibiotic and Hormone Free","Antibiotic Free",
                     "Artificial Colors","Artificial Flavors","Artificial Preservatives",
                     "Artificial Sweeteners","Biodynamic","Biologics","Bone Health","Cage/Pen Free",
                     "Carb Free Diet","Chicken Free","Clean (Cell Cultured Meat)","Collagen","Corn Free",
                     "DairyFree","Dash Diet","Dental/Oral Health","Egg Free","Fair Trade Certified",
                     "Farm Raised (Seafood)","Fillers","Food Traceability","Free Foraging","Genomics",
                     "Gluten Free","Grain Free","Gum Free","Gut Health","Hairball","Hand Crafted Foods",
                     "Heart Health","Holistic Food","Hormone Free","Immune Health","Joint Health","Lectin Free",
                     "Legume Free","Lentil Free","Lifestage (ALL)","Mediterranean Diet","Milk Alternatives",
                     "MIND Diet","Natural","Novel Proteins","Organ Protein and Organ Meat","Pasture Raised",
                     "Pea Free","Pesticide Free","Plant Based Protein","Potato Free", "Pumpkin", "Puppy/Kitten","Raw",
                     "Raw Frozen","Regenerative Agriculture","Senior Pets","Skin and Coat","Slow Grown",
                     "Soy Free","Sprouted (Grain)","Starch Free","Sustainable Manufacturing Practices",
                     "Therapeutic Diet","Transitional Farming","UTH","Vision Health","Volumetric Diet", "Yak Chew")

# j = paste("result", colnames(d8)[33], sep="_")
Os_output <- Validation_Function(Os, "OneSpace.Claim.Text")

write.csv(Os_1, "Os_latest_data_validation.csv", row.names = F)
Os_1[,3:ncol(Os_1)] <- Os_1[,3:ncol(Os_1)]
Os_1 <- data.frame(lapply(Os_1, function(x) {gsub(":", ";", x, fixed = T)}))
Os_1[,3:ncol(Os_1)] = apply(Os_1[,3:ncol(Os_1)], 2, function(x) as.character(x))
for (f in 3:ncol(Os_1)){
  Os_1[,f] <- sapply(strsplit(Os_1[,f],";"), `[`, 1)
}
Summary_file <- data.frame()
All_cols <- c("True Positive", "False Positive", "Common Tags", "Not tagged")
y=0
for (g in colnames(Os_1)[3:ncol(Os_1)]){
  y = y+1
  print(paste(y, "/116", " - ", gsub("result_", "", g)))
  tmp_1 <- data.frame(UPC = Os_1$UPC, x = Os_1[[g]])
  tmp_1 <- tmp_1 %>% group_by(UPC) %>% summarise(x = paste(x, collapse=' $$$$ '))
  tmp_1$x <- ifelse(grepl("TRUE", tmp_1$x), "True Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("FALSE", tmp_1$x), "False Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Common Tags", tmp_1$x), "Common Tags", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Not tagged", tmp_1$x), "Not tagged", tmp_1$x)
  tmp_2 <- tmp_1 %>% group_by(x) %>% summarise(total = n_distinct(UPC))
  tmp_2 <- data.frame(t(tmp_2))
  
  names(tmp_2) <- lapply(tmp_2[1, ], as.character)
  names_save <- names(tmp_2)
  tmp_2 <- data.frame(tmp_2[-1,])
  names(tmp_2) <- names_save
  row.names(tmp_2) <- NULL
  NA_cols <- All_cols[!All_cols %in% names(tmp_2)]
  tmp_2[, NA_cols] <- NA
  tmp_2$Theme <- gsub("result_", "", g)
  tmp_2 <- tmp_2 %>% select("Theme", "Not tagged", "Common Tags", "True Positive", "False Positive")


  if (nrow(Summary_file)==0){
    Summary_file <- tmp_2
  } else {
    Summary_file <- rbind(Summary_file, tmp_2)
  }
}
write.csv(Summary_file, "Summary_file_OS_latest_Dt_15_05_2020.csv", row.names = F)
# Os_2[,45] <- (unlist(strsplit(Os_2[,45], split=";"))[1])
#***********************************************************************************************************
################################################# Nielsen ##################################################
#***********************************************************************************************************

OneSpace_Claims.Nielsen <- readRDS("OneSpace_Analytics03232020.Rda")
OneSpace_Claims.Nielsen <- OneSpace_Claims.Nielsen %>% select(-c(Category,
                                                                 Segment, `Brand Expanded`,
                                                                 CLAIM_NAME))

Nielsen_Dt <- OneSpace_Claims.Nielsen %>% select(c(UPC,NUTRITIONAL_HEALTH_CLAIM_B_NB, CLAIM_B_NB,
                                                   STRATEGIC_INGREDIENT_PRSNC_CLM_B_NB, VETERINARIAN_CLAIM_NB,
                                                   WHEAT_PRESENCE_CLAIM_B_NB, FLAVOR_DETAIL_B_NB, FORMULATION_NB,
                                                   FORMULATION_B_NB, FLAVOR_NB, USDA_ORGANIC_SEAL_B_NB, PROTEIN_PRESENCE_CLAIM_NB,
                                                   MANUFACTURING_PROCESS_B_NB, PRODUCT_STORAGE_AS_STATED_B_NB,
                                                   TARGET_GROUP_CONDITION_NB, BONE_MUSCLE_AND_TEETH_CLAIM_B_NB,
                                                   DIGESTION_CLAIM_B_NB, SKIN_AND_COAT_CLAIM_B_NB, 
                                                   CALORIE_CLAIM_B_NB, WEIGHT_AND_GROWTH_CLAIM_B_NB, `Product Description`))

Nielsen_Dt <- unique(Nielsen_Dt)
rownames(Nielsen_Dt)<-NULL
Nielsen_Dt[,2:ncol(Nielsen_Dt)] <- lapply(Nielsen_Dt[,2:ncol(Nielsen_Dt)], as.character)
Nielsen_Dt<-cbind(Nielsen_Dt[1], stack(Nielsen_Dt[2:21]))
Nielsen_Dt[,2] <- gsub("\\bNOT APPLICABLE\\b", NA, gsub("\\bNOT STATED\\b", "NOT APPLICABLE", Nielsen_Dt[,2]))
Nielsen_Dt[,2] <- gsub("\\NOT COLLECTED\\b", NA, Nielsen_Dt[,2])
Nielsen_Dt$ind <- NULL
sum(is.na(Nielsen_Dt$values))
Nielsen_Dt <- na.omit(Nielsen_Dt)
Nielsen_Dt <- unique(Nielsen_Dt)
row.names(Nielsen_Dt) <- NULL

colnames(Nielsen_Dt) <- c("UPC", "Nielsen_column")
Nielsen_Dt$Nielsen_column <- gsub("\\bFR\\b"," FREE ",gsub("\\b100 PCT\\b"," 100 PERCENT ",Nielsen_Dt$Nielsen_column))




Os<-Nielsen_Dt
rownames(Os)<-NULL

d8<-read.csv("Nielsen_Dt_mapped_txnm_latest.csv")
# d8 <- d8[,-which(grepl("Nestle",names(d8)))]
drops <- c("Nielsen_column")
d8<-d8[ , !(names(d8) %in% drops)]
sum(is.na(d8))
d8$index <- row.names(d8)
d8 <- d8 %>% select(index, everything())
d8 <- d8[ ,which(colnames(d8) %in% c("index", "UPC", Themes_116))]

Os[,l1_Os]<-NA
new_only_upcs <- readRDS("NLSN_comarision_upc.rds")
new_only_upcs <- new_only_upcs %>% select(Theme, new_only, old_only)

Os$Nielsen_column<-str_squish(str_replace_all(tolower(Os$Nielsen_column),
                                                   pattern = "[[:punct:]]",replacement = " "))
#making a lookup table for all words of taxonomy
for(i in 3:length(colnames(Os))){
  if(i%%100==0){print(i)}
  Os[[colnames(Os)[i]]]<-grepl(paste("\\b",colnames(Os)[i],"\\b",sep=""),Os$Nielsen_column)
}

Os[,paste("result", colnames(d8), sep="_")]<-NA
Os$index <- row.names(Os)
Os <- Os %>% select(index, everything())
# Os_back_up <- Os
Os <- Os_back_up
# new_only_upcs <- readRDS("comparative_UPCs.rds")
# new_only_upcs <- new_only_upcs %>% select(Theme, new_only, old_only)
Os[is.na(Os)] <- F

Os_output <- Validation_Function(Os, "Nielsen_column")

write.csv(Os_output, "Nlsn_Validation.csv", row.names = F)
# Os_1[,3:ncol(Os_1)] <- Os_1[,3:ncol(Os_1)]
Os_1 <- data.frame(lapply(Os_1, function(x) {gsub(":", ";", x, fixed = T)}))
Os_1[,3:ncol(Os_1)] = apply(Os_1[,3:ncol(Os_1)], 2, function(x) as.character(x))
for (f in 3:ncol(Os_1)){
  Os_1[,f] <- sapply(strsplit(Os_1[,f],";"), `[`, 1)
}
write.csv(Os_1, "Nielsen_tx_validated.csv", row.names = F)
Os_1_backup <- Os_1
Os_1 <- Os_1[, which(colnames(Os_1) %in% c("UPC", "Nielsen_column", Themes_116))]

Nielsen_abbr <- read.csv("Nielsen_Dt_pdt_abbr_mapped_latest.csv", stringsAsFactors = F, na.strings = c("", "NA"))
Nielsen_abbr[,3:ncol(Nielsen_abbr)] <- lapply(Nielsen_abbr[,3:ncol(Nielsen_abbr)], as.logical)
Nielsen_abbr <- Nielsen_abbr[, which(colnames(Nielsen_abbr) %in% c("UPC", "Nielsen_pdt_column", Themes_116))]


Nielsen_VR <- read.csv("Nielsen_Dt_pdt_vr_mapped_latest.csv", stringsAsFactors = F, na.strings = c("", "NA"))
Nielsen_VR[,3:ncol(Nielsen_VR)] <- lapply(Nielsen_VR[,3:ncol(Nielsen_VR)], as.logical)
Nielsen_VR <- Nielsen_VR[, which(colnames(Nielsen_VR) %in% c("UPC", "Nielsen_pdt_column", Themes_116))]


colnames(Os_1) <- gsub("result_", "", colnames(Os_1))
Nielsen_Main <- rbind(Os_1[,-2], Nielsen_abbr[,-2], Nielsen_VR[,-2]) 


y=0
Nielsen_output_validation = data.frame()
for (g in colnames(Nielsen_Main)[2:ncol(Nielsen_Main)]){
  y = y+1
  print(paste(y, "/116", " - ", gsub("result_", "", g)))
  tmp_1 <- data.frame(UPC = Nielsen_Main$UPC, x = Nielsen_Main[[g]])
  tmp_1 <- tmp_1 %>% group_by(UPC) %>% summarise(x = paste(x, collapse=' $$$$ '))
  tmp_1$x <- ifelse(grepl("TRUE", tmp_1$x), "True Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Common Tags", tmp_1$x), "Common Tags", tmp_1$x)
  tmp_1$x <- ifelse(grepl("FALSE", tmp_1$x), "False Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Not tagged", tmp_1$x), "Not tagged", tmp_1$x)
  colnames(tmp_1) <- c("UPC", g)
  if (nrow(Nielsen_output_validation) == 0){
    Nielsen_output_validation <- tmp_1
  } else {
    Nielsen_output_validation <- merge(Nielsen_output_validation, tmp_1, by = "UPC", all = F)
  }
}
write.csv(Nielsen_output_validation, "Nielsen_output_validation.csv", row.names = F)
Summary_file <- data.frame()
All_cols <- c("True Positive", "False Positive", "Common Tags", "Not tagged")

y=0

for (g in colnames(Nielsen_Main)[3:ncol(Nielsen_Main)]){
  y = y+1
  print(paste(y, "/116", " - ", gsub("result_", "", g)))
  tmp_1 <- data.frame(UPC = Nielsen_Main$UPC, x = Nielsen_Main[[g]])
  tmp_1 <- tmp_1 %>% group_by(UPC) %>% summarise(x = paste(x, collapse=' $$$$ '))
  tmp_1$x <- ifelse(grepl("TRUE", tmp_1$x), "True Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Common Tags", tmp_1$x), "Common Tags", tmp_1$x)
  tmp_1$x <- ifelse(grepl("FALSE", tmp_1$x), "False Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Not tagged", tmp_1$x), "Not tagged", tmp_1$x)
  tmp_2 <- tmp_1 %>% group_by(x) %>% summarise(total = n_distinct(UPC))
  tmp_2 <- data.frame(t(tmp_2))
  
  names(tmp_2) <- lapply(tmp_2[1, ], as.character)
  names_save <- names(tmp_2)
  tmp_2 <- data.frame(tmp_2[-1,])
  names(tmp_2) <- names_save
  row.names(tmp_2) <- NULL
  NA_cols <- All_cols[!All_cols %in% names(tmp_2)]
  tmp_2[, NA_cols] <- NA
  tmp_2$Theme <- gsub("result_", "", g)
  tmp_2 <- tmp_2 %>% select("Theme", "Not tagged", "Common Tags", "True Positive", "False Positive")
  
  
  if (nrow(Summary_file)==0){
    Summary_file <- tmp_2
  } else {
    Summary_file <- rbind(Summary_file, tmp_2)
  }
}
write.csv(Summary_file, "Summary_file_NLSN_17_05_2020.csv", row.names = F)




Os_validation_file <- read.csv("Os_latest_data_validation.csv", stringsAsFactors = F, na.strings = c("", "NA"))
colnames(Os_validation_file) <- gsub("result_", "", colnames(Os_validation_file))
Os_validation_file <- Os_validation_file[,-2]
y=0
Os_output_validation = data.frame()
for (g in colnames(Os_validation_file)[2:ncol(Os_validation_file)]){
  y = y+1
  print(paste(y, "/116", " - ", gsub("result_", "", g)))
  tmp_1 <- data.frame(UPC = Os_validation_file$UPC, x = Os_validation_file[[g]])
  tmp_1 <- tmp_1 %>% group_by(UPC) %>% summarise(x = paste(x, collapse=' $$$$ '))
  tmp_1$x <- ifelse(grepl("TRUE", tmp_1$x), "True Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Common Tags", tmp_1$x), "Common Tags", tmp_1$x)
  tmp_1$x <- ifelse(grepl("FALSE", tmp_1$x), "False Positive", tmp_1$x)
  tmp_1$x <- ifelse(grepl("Not tagged", tmp_1$x), "Not tagged", tmp_1$x)
  colnames(tmp_1) <- c("UPC", g)
  if (nrow(Os_output_validation) == 0){
    Os_output_validation <- tmp_1
  } else {
    Os_output_validation <- merge(Os_output_validation, tmp_1, by = "UPC", all = F)
  }
}
write.csv(Os_output_validation, "Os_output_validation.csv", row.names = F)
