Overall_Tax <- rbind(OneSpace_Claims_Aggr_mapped[,-2], Nielsen_Dt_mapped)
Overall_Tax <- Overall_Tax %>% group_by(UPC) %>% summarise_all(max)

require(tidyr)
tmp <- gather(Overall_Tax, "Theme", "flag", 2:ncol(Overall_Tax))
View(tmp[1:1000,])
tmp <- tmp %>% filter(flag == 1)
write.csv(tmp, "UPC_to_Theme_Mapping.csv", row.names = F)

Sales_Prod <- readRDS("AOC_PetRetail_TotalUS_SalesFacts.rda")
Sales_Prod <- Sales_Prod %>% select(PRODUCT_ID, `Sales Dollars Value`)
names(Sales_Prod) <- make.names(names(Sales_Prod))

View(head(Sales_Prod))
Prod_UPC <- readRDS("PRODUCTDIM.rda")
Prod_UPC <- Prod_UPC %>% select("PRODUCT_ID", "UPC" )
Sales_UPC <- merge(Prod_UPC, Sales_Prod, by = "PRODUCT_ID", all = F)
Sales_UPC <- Sales_UPC %>% group_by(UPC) %>% summarise(Dollar_Sales = sum(Sales.Dollars.Value))
n_distinct(Sales_UPC$UPC)

UPC_theme_tagging$UPC <- as.character(UPC_theme_tagging$UPC)
UPC_theme <- UPC_theme_tagging %>% group_by(Theme) %>% summarise(Number.of.UPC = n_distinct(UPC))
Sales_UPC$UPC <- as.character(Sales_UPC$UPC)

UPC_theme_sales <- merge(UPC_theme_tagging, Sales_UPC, by = "UPC", all = F)
Theme_Sales <- UPC_theme_sales %>% group_by(Theme) %>% summarise(Number.of.UPC.tagged = n_distinct(UPC),
                                                                 Total_Sales = sum(Dollar_Sales))
Theme_Sales_UPC <- merge(UPC_theme, Theme_Sales, by = "Theme", all = F)
Theme_Sales_UPC$Number.of.UPC.tagged <- NULL
write.csv(Theme_Sales_UPC, "Theme_Sales_UPC.csv", row.names = F)
tmp <- unique(Sales_UPC$UPC)[!unique(Sales_UPC$UPC) %in% unique(UPC_theme_tagging$UPC)]
tmp <- unique(UPC_theme_tagging$UPC)[!unique(UPC_theme_tagging$UPC) %in% unique(Sales_UPC$UPC)]
