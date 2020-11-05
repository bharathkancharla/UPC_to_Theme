library(dplyr)

DATA <- c("data")

CLAIMFACTS <- read.csv(file.path(DATA,"CLAIMFACTS.csv"))
CLAIMDIM <- read.csv(file.path(DATA,"CLAIMDIM10152020.csv"))
SALESFACTS <- read.csv(file.path(DATA,"SALESFACTS10152020.csv"))

df_claim<-left_join(CLAIMFACTS, CLAIMDIM)
df<-full_join(SALESFACTS, df_claim)

saveRDS(df, file.path(DATA,"JoinData.rds"))

old <- readr::read_rds("C:/Users/bharath.kancharla/Desktop/Project/Latest_Update/OneSpace_Analytics03232020.Rda")
new <- readr::read_rds("data/OneSpace_Analytics10152020.rds")

new$UPC <- as.numeric(new$UPC)
setDT(new)
setDT(old)
common_upc <- intersect(new$UPC, old$UPC)

new <- new[UPC %in% common_upc,]

dt <- intersect(new, old)
