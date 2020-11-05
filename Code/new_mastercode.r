options(java.parameters = "- Xmx1024m")

# Install and Load the required Packages
packages_required <- c("readxl", "dplyr", "stringr", "tm", "textstem", "hunspell", 
                       "tictoc", "stringi","quanteda", "tidytext", "tidyr", "textclean", 
                       "openxlsx", "wordnet", "udpipe","data.table", "qdap","gdata","rlang")

new.packages <- packages_required[!(packages_required %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(packages_required, require, character.only = TRUE)
rm(packages_required)


# Set up the directories for the project
Dataset = "new"
working_dir <- "C:/Users/bharath.kancharla/Desktop/UPC_to_Theme_V2/"
current_date <- format(Sys.Date(), "%d%m%Y")
results_dir <- file.path(working_dir, str_glue("Results_{current_date}"))
dir.create(results_dir)
setwd(working_dir)


#*****************************************************************************************
############################## input files & Data Cleaning ##############################
#*****************************************************************************************

## Taxonomy file
FileLocTaxoNomy <- file.path(getwd(),"Boolean theme categorization_october.xlsx")
TaxoNomy <- read_excel(FileLocTaxoNomy, sheet = "Approved Themes Boolean")
colnames(TaxoNomy) <- make.names(colnames(TaxoNomy))
setDT(TaxoNomy)
# TaxoNomy <- TaxoNomy[Theme != "Dental/Oral Health",]

rm(FileLocTaxoNomy)

source("Code/add_functions.r")


#### OneSpace Claims and Nielsen file

OneSpace_Claims.Nielsen <- fread(file.path("Data/OneSpace_Analytics10152020.csv"))
OneSpace_Claims.Nielsen <- OneSpace_Claims.Nielsen[, UPC:=as.numeric(UPC)]
treat_upc <- unique(OneSpace_Claims.Nielsen[Segment=="Treats",]$UPC)

nieslsen_claim_Cols<- c("Product Description","NUTRITIONAL_HEALTH_CLAIM_B_NB", "CLAIM_B_NB",
                       "STRATEGIC_INGREDIENT_PRSNC_CLM_B_NB", "VETERINARIAN_CLAIM_NB",
                       "WHEAT_PRESENCE_CLAIM_B_NB", "FLAVOR_DETAIL_B_NB", "FORMULATION_NB",
                       "FORMULATION_B_NB", "FLAVOR_NB", "USDA_ORGANIC_SEAL_B_NB", 
                       "PROTEIN_PRESENCE_CLAIM_NB", "MANUFACTURING_PROCESS_B_NB",
                       "PRODUCT_STORAGE_AS_STATED_B_NB", "TARGET_GROUP_CONDITION_NB", 
                       "BONE_MUSCLE_AND_TEETH_CLAIM_B_NB","DIGESTION_CLAIM_B_NB",
                       "SKIN_AND_COAT_CLAIM_B_NB", "CALORIE_CLAIM_B_NB", "WEIGHT_AND_GROWTH_CLAIM_B_NB"
                       )

nieslsen_Cols <- c("UPC", "Brand Expanded", nieslsen_claim_Cols)

Nielsen_Dt <- OneSpace_Claims.Nielsen[,..nieslsen_Cols]
rownames(Nielsen_Dt)<-NULL
Nielsen_Dt <- unique(Nielsen_Dt)

# Identify length of the brand in brand column
Nielsen_Dt <- Nielsen_Dt[, brand_tokens:= lapply(`Brand Expanded`, 
                                                 function(x) length(strsplit(x,
                                                                             split = " ")[[1]]))]

# Removing the first few words in product description based on length of brand
Nielsen_Dt <- Nielsen_Dt[,`Product Description`:= 
                           mapply(function(x,y) word(x, y+1, -1, sep = fixed(" ")), 
                                                         x=`Product Description`, 
                                                         y=`brand_tokens`)]

Nielsen_Dt <- Nielsen_Dt[,lapply(.SD,tolower)]
Nielsen_Dt <- Nielsen_Dt[,c("brand_tokens","Brand Expanded"):=NULL]

Nielsen_Dt <- Nielsen_Dt[,(nieslsen_claim_Cols):= 
                           lapply(.SD,as.character), .SDcols=nieslsen_claim_Cols]

Nielsen_Dt <- cbind(Nielsen_Dt[,.(UPC)], stack(Nielsen_Dt[,..nieslsen_claim_Cols]))
Nielsen_Dt$values <- gsub("\\bnot applicable\\b|\\bnot stated\\b|\\bnot collected\\b",
                          NA, Nielsen_Dt$values)

Nielsen_Dt <- Nielsen_Dt[,ind:=NULL]
Nielsen_Dt <- Nielsen_Dt[!is.na(values),]
Nielsen_Dt <- unique(Nielsen_Dt)

setnames(Nielsen_Dt, "values","Nielsen_column")
Nielsen_Dt <- Nielsen_Dt[,Nielsen_column:=tolower(Nielsen_column)]
Nielsen_Dt <- Nielsen_Dt[,raw_claim:= lapply(Nielsen_column, as.character)]

Nielsen_Dt_pdt <- copy(Nielsen_Dt)
colnames(Nielsen_Dt_pdt) <- c("UPC", "Nielsen_pdt_column","raw_claim") #nielsen_pdt_raw_claim


OneSpace_Claims_Dt <- OneSpace_Claims.Nielsen[,.(UPC,CLAIM_NAME, CLAIM_TEXT)]
OneSpace_Claims_Dt$CLAIM_NAME <- gsub("_"," ", OneSpace_Claims_Dt$CLAIM_NAME)
OneSpace_Claims_Dt <- OneSpace_Claims_Dt[CLAIM_TEXT=="Yes", CLAIM_TEXT:= CLAIM_NAME]
OneSpace_Claims_Dt <- unique(OneSpace_Claims_Dt)
OneSpace_Claims_Dt <- OneSpace_Claims_Dt[,CLAIM_NAME:=NULL]

setnames(OneSpace_Claims_Dt, "CLAIM_TEXT", "OneSpace.Claim.Text")
OneSpace_Claims_Dt <- OneSpace_Claims_Dt[,OneSpace.Claim.Text:=tolower(OneSpace.Claim.Text)]
OneSpace_Claims_Dt <- OneSpace_Claims_Dt[,raw_claim:= lapply(OneSpace.Claim.Text, 
                                                                      as.character)]

#### Ingredients file
##### Processing of Ingredients file
# Ingredients_Dt <- readr::read_rds(file.path("Data/Ingredient_Analytics10152020.rds"))
# setDT(Ingredients_Dt)
# Ingredients_Dt <- Ingredients_Dt[`Ingredient Name` != "No Ingredient",]
# Ingredients_Dt <- unique(Ingredients_Dt)
# Ingredients_Dt <- Ingredients_Dt[, "Ingredient.Name" := paste(.SD, collapse = " "), 
#                                  by=.(UPC), .SDcols="Ingredient Name"]
# 
# 
# system.time(Ingredients_Dt <- Ingredients_Dt %>% group_by(UPC) %>% 
#               summarise(Ingredient.Name = paste(`Ingredient Name`, collapse = " ")))
# setDT(Ingredients_Dt)
# 
# Ingredients_Dt <- Ingredients_Dt[UPC %in% unique(OneSpace_Claims_Dt[,UPC]),]


## Nestle mapped 42 themes
if (Dataset == "old"){
  Mapped_data_from_Nestle <- fread(file.path(getwd(),"old to new themes mapping.csv"), stringsAsFactors = F, na.strings = c("", "NA"))
  Mapped_data_from_Nestle <- Mapped_data_from_Nestle[theme != "no claim",]
  Mapped_data_from_Nestle$Nestle.Mapping.flag <- 1
  
  Mapped_Nestle_dcasted <- reshape2::dcast(Mapped_data_from_Nestle, UPC ~ theme, value.var = "Nestle.Mapping.flag")
  colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)])
  Mapped_Nestle_dcasted <- Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
                                                   unique(OneSpace_Claims_Dt$UPC),]
  Mapped_data_from_Nestle$theme <- make.names(Mapped_data_from_Nestle$theme)
} else {
  ## Nestle mapped 93 Themes
  CLAIMDIM <- fread(file.path("Data/CLAIMDIM10152020.csv"))
  CLAIMFACTs <- fread(file.path("Data/CLAIMFACTS10152020.csv"))
  PRODUCTDIM <- fread(file.path("Data/PRODUCTDIM10152020.csv"))  #"/Emerging/",
  
  # CLAIMDIM <- setDT(CLAIMDIM)
  # CLAIMFACTs <- setDT(CLAIMFACTs)
  # PRODUCTDIM <- setDT(PRODUCTDIM)
  
  Claims_dim_facts <- merge(CLAIMDIM, CLAIMFACTs, by = "CLAIM_ID", all = F)
  Claim_PDT <- merge(PRODUCTDIM, Claims_dim_facts, by = "PRODUCT_ID", all = F)
  Claim_PDT <- unique(Claim_PDT)
  OneSpace_Claims_Dt_unique_UPC <- unique(OneSpace_Claims_Dt$UPC)
  
  Mapped_data_from_Nestle <- Claim_PDT[, .(UPC, `Claim Name`)]
  Mapped_data_from_Nestle <- unique(Mapped_data_from_Nestle)
  colnames(Mapped_data_from_Nestle) <- make.names(colnames(Mapped_data_from_Nestle))
  Mapped_data_from_Nestle <- Mapped_data_from_Nestle[Claim.Name != "No Claim",]
  
  
  old.2.new.themes <- fread(file.path(getwd(), "old to new themes mapping_october.csv"), stringsAsFactors = F,
                               na.strings = c("", "NA"),header = T)
  # old.2.new.themes[,V3:= NULL]
  old.2.new.themes <- unique(old.2.new.themes)
  Mapped_data_from_Nestle <- merge(Mapped_data_from_Nestle, old.2.new.themes, by.x = "Claim.Name", by.y = "New", all = F)
  Mapped_data_from_Nestle <- Mapped_data_from_Nestle[,Claim.Name:= NULL]
  colnames(Mapped_data_from_Nestle) <- c("UPC", "theme")
  
  Mapped_data_from_Nestle$Nestle.Mapping.flag <- 1
  
  
  Mapped_Nestle_dcasted <- reshape2::dcast(Mapped_data_from_Nestle, UPC ~ theme, value.var = "Nestle.Mapping.flag")
  colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted)[2:ncol(Mapped_Nestle_dcasted)])
  Mapped_Nestle_dcasted <- Mapped_Nestle_dcasted[unique(Mapped_Nestle_dcasted$UPC) %in% 
                                                   unique(OneSpace_Claims_Dt$UPC),]
  Mapped_data_from_Nestle$theme <- tolower(make.names(trimws(Mapped_data_from_Nestle$theme)))
  colnames(Mapped_data_from_Nestle) <- c("UPC", "theme", "Nestle.Mapping.flag")
  rm(Claim_PDT, CLAIMDIM, CLAIMFACTs, Claims_dim_facts)
  # tmp <- intersect(unique(OneSpace_Claims_Dt$UPC), unique(Mapped_Nestle_dcasted$UPC))
}



##### Loading the functions from Function file 
# source("Code/old/Emerging_functions_3_optimized_prefix.suffix.R")
source("Code/modified_emerging_functions - Copy.r")


### Manual Modification of the data
OneSpace_Claims_Dt$OneSpace.Claim.Text <- gsub("\\bcolorings\\b", " coloring ", OneSpace_Claims_Dt$OneSpace.Claim.Text)
OneSpace_Claims_Dt$OneSpace.Claim.Text <- gsub("\\bbrushings\\b", " brushing ", OneSpace_Claims_Dt$OneSpace.Claim.Text)

Nielsen_Dt$Nielsen_column <- gsub("\\bcolorings\\b", " coloring ", Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- gsub("\\bbrushings\\b", " brushing ", Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- gsub("\\btrn dog\\b|\\bdog trn\\b", " dog ", Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- gsub("\\bpet trn\\b", " pet ", Nielsen_Dt$Nielsen_column)
Nielsen_Dt$Nielsen_column <- gsub("\\badlt trn\\b", " adlt ", Nielsen_Dt$Nielsen_column)
# Based on client ask - strc fr is removed from nielsen product description 
# to avoid false for potato free claim
Nielsen_Dt$Nielsen_column <- gsub("\\b strc fr \\b", " ", Nielsen_Dt$Nielsen_column)

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
                     "Therapeutic Diet","Transitional Farming","UTH","Vision Health","Volumetric Diet", "Yak Chew",
                     "Insect Proteins")

NoIssueTheme <- c("Antibiotic Free", "Carb Free Diet", "Chicken Free", "Corn Free", "DairyFree",
                  "Egg Free", "Gluten Free", "Grain Free", "Gum Free", "Hormone Free", "Lectin Free",
                  "Legume Free", "Lentil Free", "Pea Free", "Pesticide Free", "Potato Free", "Soy Free",
                  "Starch Free")

Non_direct_match <- unique(c(free_form_theme, NoIssueTheme))

FREE_THEMES <- TaxoNomy[str_detect(tolower(TaxoNomy$Theme),"free$"),]$Theme
FREE_THEMES <- FREE_THEMES[!(FREE_THEMES %in% "Cage/Pen Free")]

# Themes that required additional validate to avoid capturing negative scenario's
ADD_CHECK_THEMES <- c("Artificial Colors","Artificial Flavors",
                      "Artificial Preservatives","Artificial Sweeteners",
                      "Artificial- Total","Fillers","By-Products",
                      "Propylene Glycol","Glyphosate", "Ethoxyquin", 
                      "Carrageenan")

FREE_THEMES <- append(FREE_THEMES, ADD_CHECK_THEMES)


TaxoNomy_main <- TaxoNomy
# TaxoNomy <- TaxoNomy_main

#### For partial theme search
# Themes_12 <- c("Aging Pets-Vitality", "Air Dried","Algae","Allergy Management","Ancient Grains",
#                "Animal Welfare (ALL)", "Antibiotic and Hormone Free","Antibiotic Free",
#                "Cage/Pen Free", "Chicken Free", "Corn Free", "Egg Free", "Gluten Free", 
#                "GMO Free/Non-GMO", "Grain Free" , "Gum Free" ,"Hormone Free",
#                "Legume Free","Lectin Free","Lentil Free","Pea Free","Pesticide Free","Potato Free","Soy Free",
#                "Starch Free") 

Themes_12 <- c("Antibiotic Free")
TaxoNomy <- TaxoNomy_main[Theme %in% Themes_12,]
Mapped_data_from_Nestle_12 <- Mapped_data_from_Nestle[theme %in% tolower(make.names(Themes_12)),]
Mapped_data_from_Nestle_12$theme <- make.names(tolower(Mapped_data_from_Nestle_12$theme))
Mapped_Nestle_dcasted_12 <- reshape2::dcast(Mapped_data_from_Nestle_12, UPC ~ theme, value.var = "Nestle.Mapping.flag")
colnames(Mapped_Nestle_dcasted_12)[2:ncol(Mapped_Nestle_dcasted_12)] <- paste0("Nestle_", colnames(Mapped_Nestle_dcasted_12)[2:ncol(Mapped_Nestle_dcasted_12)])
Mapped_Nestle_dcasted_12 <- Mapped_Nestle_dcasted_12[unique(Mapped_Nestle_dcasted_12$UPC) %in%
                                                        unique(OneSpace_Claims_Dt$UPC),]
#### End


#**************************************************************************************
##################################### RAW MAPPING #####################################
#************************************************************************************** 
# setDF(OneSpace_Claims_Dt)
OneSpace_Claims_Raw_Aggr_Tax_list <- TaxoNomy_Mapping(copy(OneSpace_Claims_Dt),
                                                      "OneSpace.Claim.Text", "OneSpace Claims")

OneSpace_Claims_Raw_Aggr_Tax <- as.data.table(OneSpace_Claims_Raw_Aggr_Tax_list[1])
OneSpace_Claims_Raw_Aggr_Tax_summary <- as.data.table(OneSpace_Claims_Raw_Aggr_Tax_list[2])
OneSpace_Claims_Raw_Matches <- as.data.table(OneSpace_Claims_Raw_Aggr_Tax_list[3])

fwrite(OneSpace_Claims_Raw_Aggr_Tax,
       file.path(results_dir,str_glue("OneSpace_Claims_Raw_Aggr_Tax_{current_date}.csv")),
       row.names = F)

fwrite(OneSpace_Claims_Raw_Aggr_Tax_summary,
       file.path(results_dir,
                 str_glue("OneSpace_Claims_Raw_Aggr_Tax_summary_{current_date}.csv")),
                 row.names = F)

fwrite(OneSpace_Claims_Raw_Matches,
       file.path(results_dir, str_glue("OneSpace_Claims_Raw_Matches_{current_date}.csv")),
                 row.names = F)


#### Nielsen Claims mapping
Nielsen_Dt_Raw__Tax_list <- TaxoNomy_Mapping(copy(Nielsen_Dt), "Nielsen_column", "Nielsen")

Nielsen_Dt_raw_mapped_tax <- as.data.table(Nielsen_Dt_Raw__Tax_list[1])
Nielsen_Dt_raw_mapped_tax_summary <- as.data.table(Nielsen_Dt_Raw__Tax_list[2])
Nielsen_Dt_raw_mapped_Strings <- as.data.table(Nielsen_Dt_Raw__Tax_list[3])


fwrite(Nielsen_Dt_raw_mapped_tax,
       file.path(results_dir,str_glue("Nielsen_Dt_raw_mapped_tax_{current_date}.csv")),
       row.names = F)

fwrite(Nielsen_Dt_raw_mapped_tax_summary,
       file.path(results_dir,str_glue("Nielsen_Dt_raw_mapped_tax_summary_{current_date}.csv")),
                 row.names = F)

fwrite(Nielsen_Dt_raw_mapped_Strings,
       file.path(results_dir,str_glue("Nielsen_Dt_raw_mapped_Strings_{current_date}.csv")),
                 row.names = F)

#**************************************************************************************
################################### Pre Processing ####################################
#**************************************************************************************

#### Spell Check
######## Loading words from Taxonomy to the dictionary

TaxoNomy_words_for_dict <- TaxoNomy[,Theme.Boolean]
TaxoNomy_words_for_dict <- removePunctuation(TaxoNomy_words_for_dict)
TaxoNomy_words_for_dict <- str_squish(TaxoNomy_words_for_dict)
TaxoNomy_words_for_dict <- unlist(strsplit(TaxoNomy_words_for_dict, " "))
TaxoNomy_words_for_dict <- tolower(unique(TaxoNomy_words_for_dict))
TaxDict <- hunspell::dictionary(lang = "en_US", affix = NULL, add_words = TaxoNomy_words_for_dict, cache = T)


#### replace misspellings
system.time(OneSpace_Claims_Dt <- 
              OneSpace_Claims_Dt[,OneSpace.Claim.Text := replace_misspelling(OneSpace.Claim.Text)])

system.time(Nielsen_Dt <- Nielsen_Dt[,Nielsen_column := replace_misspelling(Nielsen_column)])
# system.time(Ingredients_Dt <- Ingredients_Dt[,Ingredient.Name := replace_misspelling(Ingredient.Name)])


#### Punctuation removal
OneSpace_Claims_Dt <- 
  OneSpace_Claims_Dt[,OneSpace.Claim.Text := removePunctuation(OneSpace.Claim.Text)]

OneSpace_Claims_Dt <- 
  OneSpace_Claims_Dt[,OneSpace.Claim.Text := str_squish(OneSpace.Claim.Text)]


Nielsen_Dt_pdt <- Nielsen_Dt_pdt[,Nielsen_pdt_column := removePunctuation(Nielsen_pdt_column)]
Nielsen_Dt_pdt <- Nielsen_Dt_pdt[,Nielsen_pdt_column := str_squish(Nielsen_pdt_column)]

# Ingredients_Dt <- Ingredients_Dt[,Ingredient.Name:=removePunctuation(Ingredient.Name)]
# Ingredients_Dt <- Ingredients_Dt[,Ingredient.Name:=str_squish(Ingredient.Name)]

TaxoNomy <- TaxoNomy[,.(Theme.Boolean, Theme, Category)]
keeps <- c("_", "|", "&", "!", "[", "]", "(", ")", "+")
keep_theme <- c("'","+")
TaxoNomy <- TaxoNomy[,Theme.Boolean:= tolower(Theme.Boolean)]
TaxoNomy$Theme.Boolean <- gsub(paste0(".*?($|'|", paste(paste0("\\", keeps), collapse = "|"),
                                      "|[^[:punct:]]).*?"), "\\1", TaxoNomy$Theme.Boolean)
TaxoNomy <- TaxoNomy[,Theme.Boolean:= str_squish(Theme.Boolean)]

#### Lemmatization
# system.time(OneSpace_Claims_Dt <- lemmatize(copy(OneSpace_Claims_Dt), 
#                                             "OneSpace.Claim.Text"))
# system.time(Nielsen_Dt <- lemmatize(copy(Nielsen_Dt), "Nielsen_column"))
# # system.time(Ingredients_Dt <- lemmatize(copy(Ingredients_Dt), "Ingredient.Name"))
# system.time(TaxoNomy <- lemmatize(copy(TaxoNomy), "Theme.Boolean"))

#### Number conversion
OneSpace_Claims_Dt <- OneSpace_Claims_Dt[,OneSpace.Claim.Text:= as.character(OneSpace.Claim.Text)]
Nielsen_Dt <- Nielsen_Dt[,Nielsen_column:=as.character(Nielsen_column)]
Nielsen_Dt_pdt <- Nielsen_Dt_pdt[,Nielsen_pdt_column:= as.character(Nielsen_pdt_column)]
# Ingredients_Dt <- Ingredients_Dt[Ingredient.Name:=as.character(Ingredient.Name)]
TaxoNomy <- TaxoNomy[,Theme.Boolean:= as.character(Theme.Boolean)]


OneSpace_Claims_Dt <- OneSpace_Claims_Dt[,OneSpace.Claim.Text:= textclean::replace_number(OneSpace.Claim.Text)]
Nielsen_Dt <- Nielsen_Dt[,Nielsen_column:= textclean::replace_number(Nielsen_column)]
Nielsen_Dt_pdt <- Nielsen_Dt_pdt[,Nielsen_pdt_column:=textclean::replace_number(Nielsen_pdt_column)]
# Ingredients_Dt <- Ingredients_Dt[Ingredient.Name:=textclean::replace_number(Ingredient.Name)]
TaxoNomy <- TaxoNomy[,Theme.Boolean:=textclean::replace_number(Theme.Boolean)]

TaxoNomy_main <- TaxoNomy
OneSpace_Claims_Dt_backup <- OneSpace_Claims_Dt
OneSpace_Claims_Dt <- OneSpace_Claims_Dt_backup


setwd(results_dir)
#**************************************************************************************
################################## Mapping using Taxonomy #############################
#**************************************************************************************

# setDF(OneSpace_Claims_Dt)
OneSpace_Claims_Aggr_Tax_list <- TaxoNomy_Mapping(copy(OneSpace_Claims_Dt), 
                                                      "OneSpace.Claim.Text", "OneSpace Claims")


OneSpace_Claims_Aggr_Tax <- as.data.table(OneSpace_Claims_Aggr_Tax_list[1])
OneSpace_Claims_Aggr_Tax_summary <- as.data.table(OneSpace_Claims_Aggr_Tax_list[2])
OneSpace_Claims_Aggr_Mapped_Strings <- as.data.table(OneSpace_Claims_Aggr_Tax_list[3])

fwrite(OneSpace_Claims_Aggr_Tax, str_glue("OneSpace_Claims_Aggr_Tax_{current_date}.csv"), 
       row.names = F)

fwrite(OneSpace_Claims_Aggr_Mapped_Strings, 
       str_glue("OneSpace_Claims_Aggr_Mapped_Strings_{current_date}.csv"), row.names = F)

fwrite(OneSpace_Claims_Raw_Matches, 
       str_glue("OneSpace_Claims_Raw_Matches_{current_date}.csv"), row.names = F)


Nielsen_Dt_Tax_list <- TaxoNomy_Mapping(copy(Nielsen_Dt), "Nielsen_column", "Nielsen")

Nielsen_Dt_mapped_tax <- as.data.table(Nielsen_Dt_Tax_list[1])
Nielsen_Dt_mapped_tax_summary <- as.data.table(Nielsen_Dt_Tax_list[2])
Nielsen_Dt_mapped_Strings <- as.data.table(Nielsen_Dt_Tax_list[3])

fwrite(Nielsen_Dt_mapped_tax, str_glue("Nielsen_Dt_mapped_tax_{current_date}.csv"), row.names = F)
fwrite(Nielsen_Dt_mapped_tax_summary, 
       str_glue("Nielsen_Dt_mapped_tax_summary_{current_date}.csv"), row.names = F)
fwrite(Nielsen_Dt_raw_mapped_Strings, 
       str_glue("Nielsen_Dt_raw_mapped_Strings_{current_date}.csv"), row.names = F)


# TaxoNomy Abbreviations
TaxoNomy <- TaxoNomy[,Abbr.Theme.Boolean := custom_abbreviate(Theme.Boolean)]

NO_ABBR_THEMES <- c("Seafood Sourcing","DairyFree","Sprouted (Grain)","Digestive (ALL)",
                    "Insect Proteins","Taurine")

Nielsen_abbr_Dt_Tax_list <- TaxoNomy_Mapping(copy(Nielsen_Dt_pdt), "Nielsen_pdt_column", "Nielsen")

Nielsen_Dt_pdt_abbr_mapped_tax <- as.data.table(Nielsen_abbr_Dt_Tax_list[1])

# Remove the abbreviation effects on specific themes
Nielsen_Dt_pdt_abbr_mapped_tax <- Nielsen_Dt_pdt_abbr_mapped_tax[,c(NO_ABBR_THEMES):=0]
Nielsen_Dt_pdt_abbr_mapped_summary <- as.data.table(Nielsen_abbr_Dt_Tax_list[2])
Nielsen_Dt_pdt_abbr_mapped_Strings <- as.data.table(Nielsen_Dt_Tax_list[3])

fwrite(Nielsen_Dt_pdt_abbr_mapped_tax, str_glue("Nielsen_Dt_pdt_abbr_mapped_{current_date}.csv"), row.names = F)
fwrite(Nielsen_Dt_pdt_abbr_mapped_summary, 
       str_glue("Nielsen_Dt_pdt_abbr_mapped_summary_{current_date}.csv"), row.names = F)
fwrite(Nielsen_Dt_pdt_abbr_mapped_Strings, 
       str_glue("Nielsen_Dt_pdt_abbr_mapped_Strings_{current_date}.csv"), row.names = F)

#### overall Onespace
OneSpace_Dt_mapped <- rbind(OneSpace_Claims_Raw_Aggr_Tax[-C(2,3)], OneSpace_Claims_Aggr_Tax[-C(2,3)])
OneSpace_Dt_mapped <- OneSpace_Dt_mapped %>% group_by(UPC) %>% summarise_all(max)
# Remove whole muscle taggings for non treat categories
OneSpace_Dt_mapped <- OneSpace_Dt_mapped[!(UPC %in% treat_upc),Whole.Muscle.Treat:=0]

# OneSpace_Dt_mapped_with_Nestle <- merge(OneSpace_Dt_mapped, 
#                                       Mapped_Nestle_dcasted, by = "UPC", all = T)
# OneSpace_Dt_mapped_with_Nestle_summary <- Nestle_comaprision_summary(OneSpace_Dt_mapped,
#                                                                    Mapped_data_from_Nestle)
# OneSpace_Dt_mapped_summary <- OneSpace_Dt_mapped_with_Nestle_summary %>% 
#   select(Theme,	UPC_tagged_Tiger) %>% rename("Nielsen_No.of.UPC" = UPC_tagged_Tiger)

# write.csv(OneSpace_Dt_mapped_with_Nestle, "OneSpace_Dt_mapped_with_Nestle.csv", row.names = F)
# write.csv(OneSpace_Dt_mapped_with_Nestle_summary, "OneSpace_Dt_mapped_with_Nestle_summary.csv", row.names = F)


#### overall Nielsen
Nielsen_Dt_mapped <- rbind(Nielsen_Dt_raw_mapped_tax[-C(2,3)], Nielsen_Dt_mapped_tax[-C(2,3)],
                           Nielsen_Dt_pdt_abbr_mapped_tax[-C(2,3)])
Nielsen_Dt_mapped <- Nielsen_Dt_mapped %>% group_by(UPC) %>% summarise_all(max)
# Remove whole muscle taggings for non treat categories
Nielsen_Dt_mapped <- Nielsen_Dt_mapped[UPC!=treat_upc,Whole.Muscle.Treat=0]
# Nielsen_Dt_mapped_with_Nestle <- merge(Nielsen_Dt_mapped, 
#                                       Mapped_Nestle_dcasted, by = "UPC", all = T)
# Nielsen_Dt_mapped_with_Nestle_summary <- Nestle_comaprision_summary(Nielsen_Dt_mapped,
#                                                                    Mapped_data_from_Nestle)
# Nielsen_Dt_mapped_summary <- Nielsen_Dt_mapped_with_Nestle_summary %>% 
#  select(Theme,	UPC_tagged_Tiger) %>% rename("Nielsen_No.of.UPC" = UPC_tagged_Tiger)

# write.csv(Nielsen_Dt_mapped_with_Nestle, "Nielsen_Dt_mapped_with_Nestle.csv", row.names = F)
# write.csv(Nielsen_Dt_mapped_with_Nestle_summary, "Nielsen_Dt_mapped_with_Nestle_summary.csv", row.names = F)

---------------------------------------------------------------------------------

#### OneSpace and Nielsen union
# OneSpace_Nielsen_Nestle <- rbind(OneSpace_Dt_mapped, Nielsen_Dt_mapped)
# OneSpace_Nielsen_Nestle <- OneSpace_Nielsen_Nestle %>% group_by(UPC) %>% summarise_all(max)
# OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)] <- ifelse(OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)]>=1,1,0)
# max(OneSpace_Nielsen_Nestle[,2:ncol(OneSpace_Nielsen_Nestle)], na.rm = T)
# OneSpace_Nielsen_Nestle <- Nestle_comaprision_summary(OneSpace_Nielsen_Nestle, Mapped_data_from_Nestle)
# write.csv(OneSpace_Nielsen_Nestle, "OneSpace_Nielsen_Nestle_main_table.csv", row.names = F)


#### Union
# Overall_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped,
#                      Ingredients_Dt_mapped)
Overall_Tax <- rbind(OneSpace_Dt_mapped, Nielsen_Dt_mapped)
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