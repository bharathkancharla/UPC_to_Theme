options(java.parameters = "- Xmx1024m")
packages_required <- c("readxl", "dplyr", "stringr", "tm", "textstem", "hunspell", "tictoc", "stringi",
                       "quanteda", "tidytext", "tidyr", "textclean", "openxlsx", "wordnet", "udpipe",
                       "data.table", "qdap","gdata")
new.packages <- packages_required[!(packages_required %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(packages_required, require, character.only = TRUE)
rm(packages_required)

Dataset = "new"
working_dir <- "D:/Nestle/Emerging/Final/Data/"
current_date <- format(Sys.Date(), "%d%m%Y")
results_dir <- str_glue("Results_{current_date}_{Dataset}")

setwd(working_dir)
#setwd("D:/Nestle/Emerging/")


#*****************************************************************************************
############################## input files & Data Cleaning ##############################
#*****************************************************************************************

## Taxonomy file
FileLocTaxoNomy <- paste0(getwd(),"/","Boolean theme categorization.xlsx")
TaxoNomy <- read_excel(FileLocTaxoNomy, sheet = "Sheet2")
rm(FileLocTaxoNomy)
colnames(TaxoNomy) <- make.names(colnames(TaxoNomy))



#### OneSpace Claims and Nielsen file

# OneSpace_Claims.Nielsen <- readRDS(paste0(getwd(),"/OneSpace_Analytics01292020.Rds"))
OneSpace_Claims.Nielsen <- readRDS(paste0(getwd(),"/OneSpace_Analytics03232020.Rda"))

OneSpace_Claims.Nielsen <- OneSpace_Claims.Nielsen %>% select(-c(Category,
                                                                 Segment, `Brand Expanded`
                                                                 )) #CLAIM_NAME

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

colnames(Nielsen_Dt) <- c("UPC", "Nielsen_column")
Nielsen_Dt$Nielsen_column <- gsub("\\bFR\\b"," FREE ",gsub("\\b100 PCT\\b"," 100 PERCENT ",Nielsen_Dt$Nielsen_column))

#Nielsen_Dt_pdt <- OneSpace_Claims.Nielsen %>% select("UPC", "Nielsen_column")
Nielsen_Dt_pdt <- Nielsen_Dt
colnames(Nielsen_Dt_pdt) <- c("UPC", "Nielsen_pdt_column")



OneSpace_Claims_Aggr <- OneSpace_Claims.Nielsen %>% select(c(UPC, CLAIM_NAME, CLAIM_TEXT))
OneSpace_Claims_Aggr <- unique(OneSpace_Claims_Aggr)
OneSpace_Claims_Aggr$CLAIM_NAME <- as.character(OneSpace_Claims_Aggr$CLAIM_NAME)
OneSpace_Claims_Aggr$CLAIM_TEXT <- as.character(OneSpace_Claims_Aggr$CLAIM_TEXT)

for (i in 1: nrow(OneSpace_Claims_Aggr)){
  if (OneSpace_Claims_Aggr$CLAIM_TEXT[i] =="Yes"){
    OneSpace_Claims_Aggr$CLAIM_TEXT[i] <- gsub("_"," ",OneSpace_Claims_Aggr$CLAIM_NAME[i])
  }
}

OneSpace_Claims_Aggr <- OneSpace_Claims_Aggr %>% select(c(UPC, CLAIM_TEXT))
colnames(OneSpace_Claims_Aggr) <- c("UPC", "OneSpace.Claim.Text")


#### Ingredients file
# Ingredients_Dt <- readRDS(paste0(getwd(),"/Ingridients_Analytics01292020.Rds"))
# Ingredients_Dt <- readRDS(paste0(getwd(),"/Ingredient_Analytics03252020.Rda"))
# Ingredients_Dt <- Ingredients_Dt %>% filter(`Ingredient Name` != "No Ingredient")
# Ingredients_Dt <- Ingredients_Dt %>% group_by(UPC, POSITION_N, `Ingredient Name`) %>% summarise()
# 
# tic("ingredient_collapse")
# 
# Ingredients_Dt <- Ingredients_Dt %>% group_by(UPC) %>% 
#   summarise(Ingredient.Name = paste(`Ingredient Name`, collapse = " "))
# toc()
# Ingredients_Dt <- Ingredients_Dt[unique(Ingredients_Dt$UPC) %in% 
#                                    unique(OneSpace_Claims_Aggr$UPC),]

## Nestle mapped 42 themes
if (Dataset == "old"){
  Mapped_data_from_Nestle <- read.csv(paste0(getwd(),"/UPC to theme mapping.csv"), stringsAsFactors = F, na.strings = c("", "NA"))
  Mapped_data_from_Nestle <- Mapped_data_from_Nestle %>% filter(theme != "no claim")
  Mapped_data_from_Nestle$Nestle.Mapping.flag <- 1
  
  Mapped_Nestle_dcasted <- reshape2::dcast(Mapped_data_from_Nestle, UPC ~ theme, value.var = "Nestle.Mapping.flag")
  colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)])
  Mapped_Nestle_dcasted <- Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
                                                   unique(OneSpace_Claims_Aggr$UPC),]
  Mapped_data_from_Nestle$theme <- make.names(Mapped_data_from_Nestle$theme)
} else {
  ## Nestle mapped 86 Themes
  CLAIMDIM <- readRDS(file = paste0(getwd(), "/CLAIMDIM_03102020.rda"))
  CLAIMFACTs <- readRDS(file = paste0(getwd(), "/CLAIMFACTs_03102020.rda"))
  PRODUCTDIM <- readRDS(file = paste0(getwd(), "/PRODUCTDIM_03102020.rda"))  #"/Emerging/",
  
  Claims_dim_facts <- merge(CLAIMDIM, CLAIMFACTs, by = "CLAIM_ID", all = F)
  Claim_PDT <- merge(PRODUCTDIM, Claims_dim_facts, by = "PRODUCT_ID", all = F)
  Claim_PDT <- unique(Claim_PDT)
  OneSpace_Claims_Aggr_unique_UPC <- unique(OneSpace_Claims_Aggr$UPC)
  
  Mapped_data_from_Nestle <- Claim_PDT %>% select(UPC, `Claim Name`)
  Mapped_data_from_Nestle <- unique(Mapped_data_from_Nestle)
  colnames(Mapped_data_from_Nestle) <- make.names(colnames(Mapped_data_from_Nestle))
  Mapped_data_from_Nestle <- Mapped_data_from_Nestle %>% filter(Claim.Name != "No Claim")
  
  
  old.2.new.themes <- read.csv(paste0(getwd(), "/", "old to new themes mapping.csv"), stringsAsFactors = F,
                               na.strings = c("", "NA"))
  old.2.new.themes <- unique(old.2.new.themes)
  Mapped_data_from_Nestle <- merge(Mapped_data_from_Nestle, old.2.new.themes, by.x = "Claim.Name", by.y = "New", all = F)
  Mapped_data_from_Nestle$Claim.Name <- NULL
  colnames(Mapped_data_from_Nestle) <- c("UPC", "theme")
  
  Mapped_data_from_Nestle$Nestle.Mapping.flag <- 1
  
  
  Mapped_Nestle_dcasted <- reshape2::dcast(Mapped_data_from_Nestle, UPC ~ theme, value.var = "Nestle.Mapping.flag")
  colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)])
  Mapped_Nestle_dcasted <- Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
                                                   unique(OneSpace_Claims_Aggr$UPC),]
  Mapped_data_from_Nestle$theme <- tolower(make.names(trimws(Mapped_data_from_Nestle$theme)))
  rm(Claim_PDT, CLAIMDIM, CLAIMFACTs, Claims_dim_facts)
  # tmp <- intersect(unique(OneSpace_Claims_Aggr$UPC), unique(Mapped_Nestle_dcasted$UPC))
}



##### Loading the functions from Function file 
source("D:/Nestle/Emerging/Final/code files/Emerging_functions_3_optimized_prefix.suffix_P2S.R")

#**************************************************************************************
################################# Spell Check ######################################### 
#**************************************************************************************

######## Loading words from Taxonomy to the dictionary

TaxoNomy_words_for_dict <- TaxoNomy$Theme.Boolean
TaxoNomy_words_for_dict <- removePunctuation(TaxoNomy_words_for_dict)
TaxoNomy_words_for_dict <- str_squish(TaxoNomy_words_for_dict)
# TaxoNomy_words_for_dict <- str_replace(gsub("\\s+", " ", 
#                                             str_trim(TaxoNomy_words_for_dict)), "B", "b")
TaxoNomy_words_for_dict <- unlist(strsplit(TaxoNomy_words_for_dict, " "))
TaxoNomy_words_for_dict <- tolower(unique(TaxoNomy_words_for_dict))
TaxDict <- hunspell::dictionary(lang = "en_US", affix = NULL, add_words = TaxoNomy_words_for_dict, cache = T)


#### OneSpace Claims
# OneSpace_Claims_Aggr$OneSpace.Claim.Text <- tolower(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
tic("spell check onespace")
OneSpace_Claims_Aggr$OneSpace.Claim.Text <- replace_misspelling(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
toc()

#### Nielsen
# Nielsen_Dt$Nielsen_column <- tolower(Nielsen_Dt$Nielsen_column)
tic("spell check Nielsen")
Nielsen_Dt$Nielsen_column <- replace_misspelling(Nielsen_Dt$Nielsen_column)
toc()

#### Ingredients
# Ingredients_Dt$Ingredient.Name <- tolower(Ingredients_Dt$Ingredient.Name)
# tic("spell check Ingredient")
# Ingredients_Dt$Ingredient.Name <- replace_misspelling(Ingredients_Dt$Ingredient.Name)
# toc()

#**************************************************************************************
#################################### Preprocessing ####################################
#**************************************************************************************

#### OneSpace Claims

# OneSpace_Claims_Aggr$OneSpace.Claim.Text <- lapply(OneSpace_Claims_Aggr$OneSpace.Claim.Text, 
#                                                    function(y) gsub("[[:punct:]]", "", y))
OneSpace_Claims_Aggr$OneSpace.Claim.Text <- gsub("\\bcolorings\\b", " coloring ", OneSpace_Claims_Aggr$OneSpace.Claim.Text)
OneSpace_Claims_Aggr$OneSpace.Claim.Text <- gsub("\\bbrushings\\b", " brushing ", OneSpace_Claims_Aggr$OneSpace.Claim.Text)

# OneSpace_Claims_Aggr$OneSpace.Claim.Text <- removePunctuation(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
OneSpace_Claims_Aggr$OneSpace.Claim.Text <- str_squish(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
# OneSpace_Claims_Aggr$OneSpace.Claim.Text <- str_replace(gsub("\\s+", " ", str_trim(OneSpace_Claims_Aggr$OneSpace.Claim.Text)), "B", "b")

#### Nielsen
Nielsen_Dt$Nielsen_column <- gsub("\\bcolorings\\b", " coloring ", Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- gsub("\\bbrushings\\b", " brushing ", Nielsen_Dt$Nielsen_column)

# Nielsen_Dt$Nielsen_column <- removePunctuation(Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- str_squish(Nielsen_Dt$Nielsen_column)

# Nielsen_Dt$Nielsen_column <- lapply(Nielsen_Dt$Nielsen_column, 
#                                     function(y) gsub("[[:punct:]]", "", y))
# Nielsen_Dt$Nielsen_column <- str_replace(gsub("\\s+", " ", str_trim(Nielsen_Dt$Nielsen_column)), "B", "b")


#### Nielsen Product Description
# Nielsen_Dt_pdt$Nielsen_pdt_column <- tolower(Nielsen_Dt_pdt$Nielsen_pdt_column)
# Nielsen_Dt_pdt$Nielsen_pdt_column <- removePunctuation(Nielsen_Dt_pdt$Nielsen_pdt_column)
Nielsen_Dt_pdt$Nielsen_pdt_column <- str_squish(Nielsen_Dt_pdt$Nielsen_pdt_column)
# Nielsen_Dt_pdt$Nielsen_pdt_column <- lapply(Nielsen_Dt_pdt$Nielsen_pdt_column, 
#                                     function(y) gsub("[[:punct:]]", "", y))
# Nielsen_Dt_pdt$Nielsen_pdt_column <- str_replace(gsub("\\s+", " ", str_trim(Nielsen_pdt_column$Nielsen_column)), "B", "b")

#### Ingredients
# Ingredients_Dt$Ingredient.Name <- gsub("\\bcolorings\\b", " coloring ", Ingredients_Dt$Ingredient.Name)
# Ingredients_Dt$Ingredient.Name <- gsub("\\bbrushings\\b", " brushing ", Ingredients_Dt$Ingredient.Name)
# 
# # Ingredients_Dt$Ingredient.Name <- removePunctuation(Ingredients_Dt$Ingredient.Name)
# Ingredients_Dt$Ingredient.Name <- str_squish(Ingredients_Dt$Ingredient.Name)

# Ingredients_Dt$Ingredient.Name <- lapply(Ingredients_Dt$Ingredient.Name, 
#                                          function(y) gsub("[[:punct:]]", "", y))
# Ingredients_Dt$Ingredient.Name <- str_replace(gsub("\\s+", " ", str_trim(Ingredients_Dt$Ingredient.Name)), "B", "b")

#### Taxonomy
TaxoNomy <- TaxoNomy %>% select(Theme.Boolean, Theme, Category)
keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
keep_theme <- c("'","+")
TaxoNomy$Theme.Boolean <- tolower(TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                                           "|[^[:punct:]]).*?"), "\\1", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- str_squish(TaxoNomy$Theme.Boolean)
# TaxoNomy$Theme.Boolean <- str_replace(gsub("\\s+", " ", str_trim(TaxoNomy$Theme.Boolean)), "B", "b")



#**************************************************************************************
################################## Lemmatization ######################################
#**************************************************************************************

tic("lemmatization onespace")
OneSpace_Claims_Aggr <- lemmatize(OneSpace_Claims_Aggr, "OneSpace.Claim.Text")
toc()

# saveRDS(OneSpace_Claims_Aggr, "OneSpace_Claims_Aggr_lemma.rds")

#### Nielsen 
tic("lemmatization Nielsen")
Nielsen_Dt <- lemmatize(Nielsen_Dt, "Nielsen_column")
toc()
# saveRDS(Nielsen_Dt, "Nielsen_Dt_lemma.RDS")

#### Ingredients
# tic("lemmatization Ingredients")
# Ingredients_Dt <- lemmatize(Ingredients_Dt, "Ingredient.Name")
# toc()
# saveRDS(Ingredients_Dt, "Ingredients_Dt_lemma.rds")

#### Taxonomy
tic("lemmatization Taxonomy")
TaxoNomy <- lemmatize(TaxoNomy, "Theme.Boolean")
toc()

# saveRDS(TaxoNomy_main, "TaxoNomy_main_lemma.rds")


########################## Number conversion ##################################
OneSpace_Claims_Aggr$OneSpace.Claim.Text <- as.character(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
Nielsen_Dt$Nielsen_column <- as.character(Nielsen_Dt$Nielsen_column)
Nielsen_Dt_pdt$Nielsen_pdt_column <- as.character(Nielsen_Dt_pdt$Nielsen_pdt_column)
# Ingredients_Dt$Ingredient.Name <- as.character(Ingredients_Dt$Ingredient.Name)
TaxoNomy$Theme.Boolean <- as.character(TaxoNomy$Theme.Boolean)

OneSpace_Claims_Aggr$OneSpace.Claim.Text <- textclean::replace_number(OneSpace_Claims_Aggr$OneSpace.Claim.Text)
Nielsen_Dt$Nielsen_column <- textclean::replace_number(Nielsen_Dt$Nielsen_column)
Nielsen_Dt_pdt$Nielsen_pdt_column <- textclean::replace_number(Nielsen_Dt_pdt$Nielsen_pdt_column)
# Ingredients_Dt$Ingredient.Name <- textclean::replace_number(Ingredients_Dt$Ingredient.Name)
TaxoNomy$Theme.Boolean <- textclean::replace_number(TaxoNomy$Theme.Boolean)

TaxoNomy_main <- TaxoNomy
OneSpace_Claims_Aggr_backup <- OneSpace_Claims_Aggr
OneSpace_Claims_Aggr <- OneSpace_Claims_Aggr_backup

#**************************************************************************************
################################## Mapping using Taxonomy #############################
#**************************************************************************************

#### For partial theme search
# Themes_12 <- c("Lifestage (ALL)", "Animal Welfare (ALL)", "Clean/Insect/Novel Protein (ALL)",
#                 "Heart Disease Total", "Instinctual/Ancestral-Total")
# 
# TaxoNomy <- TaxoNomy_main %>% filter(Theme %in% Themes_12)
# Mapped_data_from_Nestle_12 <- Mapped_data_from_Nestle %>%
#   filter(theme %in% tolower(make.names(Themes_12)))
# Mapped_data_from_Nestle_12$theme <- make.names(tolower(Mapped_data_from_Nestle_12$theme))
# Mapped_Nestle_dcasted_12 <- reshape2::dcast(Mapped_data_from_Nestle_12, UPC ~ theme, value.var = "Nestle.Mapping.flag")
# colnames(Mapped_Nestle_dcasted_12)[2:ncol(Mapped_Nestle_dcasted_12)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted_12)[2:ncol(Mapped_Nestle_dcasted_12)])
# Mapped_Nestle_dcasted_12 <- Mapped_Nestle_dcasted_12[unique(Mapped_Nestle_dcasted_12$UPC) %in%
#                                                        unique(OneSpace_Claims_Aggr$UPC),]
#### End

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

NoIssueTheme <- c("Antibiotic Free", "Carb Free Diet", "Chicken Free", "Corn Free", "DairyFree",
                  "Egg Free", "Gluten Free", "Grain Free", "Gum Free", "Hormone Free", "Lectin Free",
                  "Legume Free", "Lentil Free", "Pea Free", "Pesticide Free", "Potato Free", "Soy Free",
                  "Starch Free")
Non_direct_match <- unique(c(free_form_theme, NoIssueTheme))


dir.create(file.path(working_dir, results_dir), showWarnings = FALSE)
setwd(file.path(working_dir, results_dir))


# setwd("D:/Nestle/Emerging/Updated code run 07052020/")
colnames(Mapped_data_from_Nestle) <- c("UPC", "theme", "Nestle.Mapping.flag")

#### OneSpace Claims
tic("OneSpace tax mapping")
OneSpace_Claims_Aggr_Tax_list <- Taxonomy_mapping_p2s(OneSpace_Claims_Aggr, 
                                                  "OneSpace.Claim.Text", "OneSpace Claims") 
toc()

OneSpace_Claims_Aggr_mapped <- as.data.frame(OneSpace_Claims_Aggr_Tax_list[1])
write.csv(OneSpace_Claims_Aggr_mapped, str_glue("OneSpace_Claims_Aggr_mapped_81k_{current_date}.csv"),
          row.names = F)

OneSpace_Claims_Aggr_mapped_txt <- OneSpace_Claims_Aggr_mapped %>% group_by(UPC) %>% summarise(OneSpace.Claim.Text = paste(OneSpace.Claim.Text, collapse = " "))
OneSpace_Claims_Aggr_mapped <- OneSpace_Claims_Aggr_mapped[,-which(colnames(OneSpace_Claims_Aggr_mapped) == "OneSpace.Claim.Text")]
OneSpace_Claims_Aggr_mapped <- OneSpace_Claims_Aggr_mapped %>% group_by(UPC) %>% summarise_all(max)
OneSpace_Claims_Aggr_mapped <- merge(OneSpace_Claims_Aggr_mapped_txt, OneSpace_Claims_Aggr_mapped, 
                                     by = "UPC", all = F)
OneSpace_Claims_Aggr_mapped_summary <- as.data.frame(OneSpace_Claims_Aggr_Tax_list[2])


OneSpace_Claims_Aggr_mapped_with_Nestle <- merge(OneSpace_Claims_Aggr_mapped, 
                                  Mapped_Nestle_dcasted, by = "UPC", all = T)

OneSpace_Claims_Aggr_mapped_with_Nestle_summary <- Nestle_comaprision_summary(
  OneSpace_Claims_Aggr_mapped[,-which(colnames(OneSpace_Claims_Aggr_mapped) == "OneSpace.Claim.Text")],
                                                                              Mapped_data_from_Nestle)
write.csv(OneSpace_Claims_Aggr_mapped_with_Nestle, str_glue("OneSpace_Claims_Aggr_mapped_with_Nestle_{current_date}.csv"),
          row.names = F)
write.csv(OneSpace_Claims_Aggr_mapped_with_Nestle_summary, str_glue("OneSpace_Claims_Aggr_mapped_with_Nestle_summary_{current_date}.csv"), row.names = F)


#### Nielsen 
tic("Nielsen_Dt_Tax_list")
Nielsen_Dt_Tax_list <- Taxonomy_mapping_p2s(Nielsen_Dt, "Nielsen_column", "Nielsen") 
toc()
Nielsen_Dt_mapped_tax <- as.data.frame(Nielsen_Dt_Tax_list[1])
Nielsen_Dt_mapped_tax_summary <- as.data.frame(Nielsen_Dt_Tax_list[2])
write.csv(Nielsen_Dt_mapped_tax, str_glue("Nielsen_Dt_mapped_txnm_{current_date}.csv"), row.names = F)
write.csv(Nielsen_Dt_mapped_tax_summary, str_glue("Nielsen_Dt_mapped_txnm_summary_{current_date}.csv"), row.names = F)


#### Ingredients
# tic("Ingredients")
# Ingredients_Dt_Tax_list <- Taxonomy_mapping_p2s(Ingredients_Dt, "Ingredient.Name", "Ingredients") 
# toc()
# Ingredients_Dt_mapped <- as.data.frame(Ingredients_Dt_Tax_list[1])
# write.csv(Ingredients_Dt_mapped, str_glue("Ingredients_Dt_mapped_81k_{current_date}.csv"), row.names = F)
# 
# Ingredients_Dt_mapped_txt <- Ingredients_Dt_mapped %>% group_by(UPC) %>% summarise(Ingredient.Name = paste(Ingredient.Name, collapse = " "))
# Ingredients_Dt_mapped <- Ingredients_Dt_mapped[,-which(colnames(Ingredients_Dt_mapped) == "Ingredient.Name")]
# Ingredients_Dt_mapped <- Ingredients_Dt_mapped %>% group_by(UPC) %>% summarise_all(sum)
# Ingredients_Dt_mapped[,2:ncol(Ingredients_Dt_mapped)] <- ifelse(Ingredients_Dt_mapped[,
#                   2:ncol(Ingredients_Dt_mapped)]>=1,1,0)
# Ingredients_Dt_mapped <- merge(Ingredients_Dt_mapped_txt, Ingredients_Dt_mapped, 
#                                      by = "UPC", all = F)
# 
# Ingredients_Dt_mapped_summary <- as.data.frame(Ingredients_Dt_Tax_list[2])
# Ingredients_Dt_mapped_with_Nestle <- merge(Ingredients_Dt_mapped, 
#                                                  Mapped_Nestle_dcasted, by = "UPC", all = T)
# Ingredients_Dt_mapped <- Ingredients_Dt_mapped[,-which(colnames(Ingredients_Dt_mapped) == "Ingredient.Name")]
# 
# Ingredients_Dt_mapped_with_Nestle_summary <- Nestle_comaprision_summary(Ingredients_Dt_mapped,
#                                                                         Mapped_data_from_Nestle)
# write.csv(Ingredients_Dt_mapped_with_Nestle, str_glue("Ingredients_Dt_mapped_with_Nestle_{current_date}.csv"), row.names = F)
# write.csv(Ingredients_Dt_mapped_with_Nestle_summary, str_glue("Ingredients_Dt_mapped_with_Nestle_summary_{current_date}.csv"), row.names = F)


#### Nielsen Product Description abbr
# Taxonomy_main <- Taxonomy 
### 1) vowel removal
TaxoNomy$Theme.Boolean <- gsub(" not ", " ! ", TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub('([[:alpha:]])\\1+', '\\1', TaxoNomy$Theme.Boolean)
TaxoNomy$Theme.Boolean <- gsub("(?<=[a-z])[aeiou]", "", TaxoNomy$Theme.Boolean, perl=TRUE)
TaxoNomy$Theme.Boolean <- gsub(" ! ", " not ", TaxoNomy$Theme.Boolean)

# writeData(Taxonomy.Processed, sheet = "Processed 2 (Vowel rm)", TaxoNomy)

tic("vowel removal Nielsen")
Nielsen_Dt_pdt_vowel_rm_list <- Taxonomy_mapping_p2s(Nielsen_Dt_pdt, "Nielsen_pdt_column", "Vowel_rm")
toc()
Nielsen_Dt_pdt_vr_mapped <- as.data.frame(Nielsen_Dt_pdt_vowel_rm_list[1])
Nielsen_Dt_pdt_vr_mapped_summary <- as.data.frame(Nielsen_Dt_pdt_vowel_rm_list[2])

write.csv(Nielsen_Dt_pdt_vr_mapped, str_glue("Nielsen_Dt_pdt_vr_mapped_{current_date}.csv"), row.names = F)
write.csv(Nielsen_Dt_pdt_vr_mapped_summary, str_glue("Nielsen_Dt_pdt_vr_mapped_summary_{current_date}.csv"), row.names = F)



### 2) abbriviation
TaxoNomy <- TaxoNomy_main
# TaxoNomy <- TaxoNomy_main %>% filter(Theme %in% Themes_12)
# TaxoNomy$Theme.Boolean <- as.character(TaxoNomy$Theme.Boolean)
for (s in 1:nrow(TaxoNomy)){
  TaxoNomy_1 <- TaxoNomy$Theme.Boolean[s]
  Tax_dt <- TaxoNomy$Theme.Boolean[s]
  Tax_dt <- gsub("[[:punct:]]", "", Tax_dt)
  Tax_dt <-  str_replace(gsub("\\s+", " ", str_trim(Tax_dt)), "B", "b")
  Tax_dt <- unlist(strsplit(Tax_dt, " or ", fixed = T))
  Tax_dt <- unlist(strsplit(Tax_dt, " and not ", fixed = T))
  Tax_dt <- unlist(strsplit(Tax_dt, c(" and "), fixed = T))
  Tax_dt_2 <- Tax_dt
  for (j in 1:length(Tax_dt_2)){
    Tax_dt_2[j] <-  abbreviate(Tax_dt_2[j])
  }
  
  Tax_dt <- data.frame(original = Tax_dt, abr = abbreviate(Tax_dt_2))
  for (g in 1:nrow(Tax_dt)) {
    # print(paste0("g = ", g, ",  ", "column name = ", colnames(map_use_2)[g]))
    TaxoNomy_1 <- gsub(paste0("\\b", Tax_dt$original[g], "\\b"),
                       paste0(Tax_dt$abr[g]), TaxoNomy_1)
    # print(tmp9)
  }
  TaxoNomy$Theme.Boolean[s] <- TaxoNomy_1
}

# addWorksheet(Taxonomy.Processed, sheetName = "Processed 3 (Std Abbr)")

# writeData(Taxonomy.Processed, sheet = "Processed 3 (Std Abbr)", TaxoNomy)
# saveWorkbook(Taxonomy.Processed, "Taxonomy.Processed.xlsx", overwrite = T)

tic("abbriviation")
Nielsen_Dt_pdt_abbr_list <- Taxonomy_mapping_p2s(Nielsen_Dt_pdt, "Nielsen_pdt_column", "abbr")
toc()
Nielsen_Dt_pdt_abbr_mapped <- as.data.frame(Nielsen_Dt_pdt_abbr_list[1])
Nielsen_Dt_pdt_abbr_summary <- as.data.frame(Nielsen_Dt_pdt_abbr_list[2])
write.csv(Nielsen_Dt_pdt_abbr_mapped, "Nielsen_Dt_pdt_abbr_mapped.csv", row.names = F)
write.csv(Nielsen_Dt_pdt_abbr_summary, "Nielsen_Dt_pdt_abbr_summary.csv", row.names = F)


#### overall Nielsen
Nielsen_Dt_mapped <- rbind(Nielsen_Dt_mapped_tax[-2], Nielsen_Dt_pdt_vr_mapped[-2],
                           Nielsen_Dt_pdt_abbr_mapped[-2])
Nielsen_Dt_mapped <- Nielsen_Dt_mapped %>% group_by(UPC) %>% summarise_all(max)
Nielsen_Dt_mapped_with_Nestle <- merge(Nielsen_Dt_mapped, 
                                       Mapped_Nestle_dcasted, by = "UPC", all = T)
Nielsen_Dt_mapped_with_Nestle_summary <- Nestle_comaprision_summary(Nielsen_Dt_mapped,
                                                                    Mapped_data_from_Nestle)
Nielsen_Dt_mapped_summary <- Nielsen_Dt_mapped_with_Nestle_summary %>% 
  select(Theme,	UPC_tagged_Tiger) %>% rename("Nielsen_No.of.UPC" = UPC_tagged_Tiger)


write.csv(Nielsen_Dt_mapped_with_Nestle, "Nielsen_Dt_mapped_with_Nestle.csv", row.names = F)
write.csv(Nielsen_Dt_mapped_with_Nestle_summary, "Nielsen_Dt_mapped_with_Nestle_summary.csv", row.names = F)


#### OneSpace and Nielsen union
OneSpace_Nielsen_Nestle <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped)
OneSpace_Nielsen_Nestle <- OneSpace_Nielsen_Nestle %>% group_by(UPC) %>% summarise_all(max)
# OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)] <- ifelse(OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)]>=1,1,0)
# max(OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)], na.rm = T)
OneSpace_Nielsen_Nestle <- Nestle_comaprision_summary(OneSpace_Nielsen_Nestle, Mapped_data_from_Nestle)
write.csv(OneSpace_Nielsen_Nestle, "OneSpace_Nielsen_Nestle_main_table.csv", row.names = F)


#### Union
# Overall_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped,
#                      Ingredients_Dt_mapped)
Overall_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped)
Overall_Tax <- Overall_Tax %>% group_by(UPC) %>% summarise_all(max)
max(Overall_Tax[,2:ncol(Overall_Tax)], na.rm = T)


##### Common between Nielsen and OneSpace
#### Union Summary
Overall_Tax <- as.data.frame(colSums(Overall_Tax[-1]))
colnames(Overall_Tax) <- "Union_No.of.UPC"
Overall_Tax$Themes <- row.names(Overall_Tax)
row.names(Overall_Tax) <- NULL
Overall_Tax <- Overall_Tax %>% select(Themes, Union_No.of.UPC)
colnames(Overall_Tax) <- c("Themes", "Overall_No.of.UPC")


# OneSpace_Claims_Aggr_mapped_summary <- DataFrame2_Overall
Common_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped)
Common_Tax <- Common_Tax %>% group_by(UPC) %>% summarise_all(sum)
max(Common_Tax[,2:ncol(Common_Tax)], na.rm = T)
Common_Tax[ , 2:ncol(Common_Tax)][ Common_Tax[ , 2:ncol(Common_Tax)] == 1 ] <- 0
Common_Tax[ , 2:ncol(Common_Tax)][ Common_Tax[ , 2:ncol(Common_Tax)] == 2 ] <- 1
# Common_Tax[,2:ncol(Common_Tax)] <- ifelse(Common_Tax[,2:ncol(Common_Tax)]==2,1,0)
Common_Tax <- as.data.frame(colSums(Common_Tax[-1]))
colnames(Common_Tax) <- "Common_No.of.UPC"
Common_Tax$Themes <- row.names(Common_Tax)
row.names(Common_Tax) <- NULL
Common_Tax <- Common_Tax %>% select(Themes, Common_No.of.UPC)
colnames(Common_Tax) <- c("Themes", "Common_bw_Nlsn_&_OS")


OneSpace_Claims_Aggr_mapped_summary$Themes <- tolower(make.names(OneSpace_Claims_Aggr_mapped_summary$Themes))
# Ingredients_Dt_mapped_summary$Themes <- tolower(make.names(Ingredients_Dt_mapped_summary$Themes))
Overall_Tax$Themes <- tolower(Overall_Tax$Themes)
Common_Tax$Themes <- tolower(Common_Tax$Themes)
#### Overall Main Table
Main_Mapping_table_tax <- merge(OneSpace_Claims_Aggr_mapped_summary, Nielsen_Dt_mapped_summary,
                                by.x = "Themes", by.y = "Theme", all = F)
# Main_Mapping_table_tax <- merge(Main_Mapping_table_tax, Ingredients_Dt_mapped_summary,
#                                 by = "Themes", all = F)
Main_Mapping_table_tax <- merge(Main_Mapping_table_tax, Common_Tax,
                                by = "Themes", all = F)

Main_Mapping_table_tax <- merge(Main_Mapping_table_tax, Overall_Tax,
                                by = "Themes", all = F)
write.csv(Main_Mapping_table_tax, "Main_Mapping_table_tax.csv", row.names = F)