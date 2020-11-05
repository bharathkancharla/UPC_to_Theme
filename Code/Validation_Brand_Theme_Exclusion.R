## Read the onespace-nielsen data
OneSpace_Claims.Nielsen <- fread(file.path("Data/OneSpace_Analytics10152020.csv"))
OneSpace_Claims.Nielsen <- OneSpace_Claims.Nielsen[, UPC:=as.numeric(UPC)]

## Create a Brand-UPC mapping data table
brand_dt <- OneSpace_Claims.Nielsen[,.(UPC,`Brand Expanded`)]

## Brand Exclusion List
brand_exclude_dt <- read_excel("Brand Theme Exclusion List.xlsx")
brand_exclude_dt <- merge(brand_exclude_dt, old.2.new.themes, by.x = "Exclude Themes", by.y = "New", all = T)
setDT(brand_exclude_dt)
brand_exclude_dt <- brand_exclude_dt[is.na(Old), Old :=`Exclude Themes`]
brand_exclude_dt <- unique(na.omit(brand_exclude_dt))
brand_exclude_dt <- brand_exclude_dt[, `Exclude Themes`:= NULL]
brand_exclude_dt <- brand_exclude_dt[,Old:= make.names(Old)]
brand_exclude_dt <- brand_exclude_dt[, Tag:=0]
brand_exclude_dt <- spread(brand_exclude_dt, Old, Tag)

brand_exclude_dt <- merge(brand_dt,
                          brand_exclude_dt, 
                          by.x = "Brand Expanded",by.y = "Brand", 
                          all.y = T)
brand_exclude_dt <- subset(brand_exclude_dt, select = -c(`Brand Expanded`) )
brand_exclude_dt <- unique(brand_exclude_dt)

brand_exclude_dt <- gather(brand_exclude_dt, 2:ncol(brand_exclude_dt), 
                           key = "Theme", value = "Tagging")
brand_exclude_dt <- unique(na.omit(brand_exclude_dt))
brand_exclude_dt <- subset(brand_exclude_dt, select = -c(Tagging) )



## Overall
Overall_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped)
Overall_Tax <- Overall_Tax %>% group_by(UPC) %>% summarise_all(max)

require(tidyr)
tmp <- gather(Overall_Tax, "Theme", "flag", 2:ncol(Overall_Tax))
View(tmp[1:1000,])
tmp <- tmp %>% filter(flag == 1)

tmp <- subset(tmp, select = -c(flag) )
tmp$Theme <- lapply(tmp$Theme, make.names)

validation_df <- intersect(brand_exclude_dt,tmp)
validation_df <- validation_df[order(UPC),]
write.csv(validation_df, "Brand_Theme_validations.csv")