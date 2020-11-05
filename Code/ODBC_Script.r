library(RODBC)
library(dplyr)



#This will establish a connection to the data mart
myconnect <- odbcDriverConnect("Driver={SQL Server};Server=USSTLD0006\\PSAPP01,40011;Database=CMI iCube;Trusted_Connection=Yes")
View(RODBC::sqlTables(myconnect))

OneSpace <- sqlFetch(myconnect, "datascience.ONESPACE_ANALYTICS")

Ingridients <- sqlFetch(myconnect, "datascience.INGREDIENT_DECK")

SALESFACTS<-sqlFetch(myconnect, "datascience.MARKET_SALES_FACT_TOTAL_AOC_AND_PET_RETAIL")

CLAIMDIM<-sqlFetch(myconnect, "datascience.CLAIM_DIM")

CLAIMFACTS<-sqlFetch(myconnect, "datascience.CLAIM_FACT")

PERIODDIM<-sqlFetch(myconnect, "datascience.NIELSEN_CALENDAR_DIM")

PRODUCTDIM<-sqlFetch(myconnect, "datascience.PRODUCT_DIM_REPORTING_ATTRIBUTES")


df_claim<-left_join(CLAIMFACTS, CLAIMDIM)

df<-full_join(SALESFACTS, df_claim)

write.csv(OneSpace,"OneSpace_Analytics10152020.csv",row.names = F)
write.csv(Ingridients,"Ingridients10152020.csv",row.names = F)
write.csv(SALESFACTS,"SALESFACTS10152020.csv",row.names = F)
write.csv(CLAIMDIM,"CLAIMDIM10152020.csv",row.names = F)
write.csv(PERIODDIM, "PERIODDIM10152020.csv",row.names = F)
write.csv(PRODUCTDIM, "PRODUCTDIM10152020.csv",row.names = F)
write.csv(df, file.path(DATA,"JoinData.csv"),row.names = F)
