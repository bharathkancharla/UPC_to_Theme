"%ni%" <- Negate("%in%")
OneSpace_Claims_Raw_Aggr_Tax <- fread("Results/OneSpace_Claims_Raw_Aggr_Tax_03112020.csv")
Nielsen_Dt_raw_mapped_tax <- fread("Results/Nielsen_Dt_raw_mapped_tax_03112020.csv")

OneSpace_Claims_Aggr_Tax <- fread("Results/OneSpace_Claims_Aggr_Tax_03112020.csv")
Nielsen_Dt_mapped_tax <- fread("Results/Nielsen_Dt_mapped_tax_03112020.csv")
Nielsen_Dt_pdt_abbr_mapped_tax <- fread("Results/Nielsen_Dt_pdt_abbr_mapped_03112020.csv")

#### overall Onespace
OneSpace_Dt_mapped <- rbind(OneSpace_Claims_Raw_Aggr_Tax, OneSpace_Claims_Aggr_Tax)
OneSpace_Dt_mapped <- OneSpace_Dt_mapped[, lapply(.SD, max), by=.(UPC)]


#### overall Nielsen
Nielsen_Dt_mapped <- rbind(Nielsen_Dt_raw_mapped_tax, Nielsen_Dt_mapped_tax,
                           Nielsen_Dt_pdt_abbr_mapped_tax)

Nielsen_Dt_mapped <- Nielsen_Dt_mapped[, lapply(.SD, max), by=.(UPC)]


Overall_Tax <- rbind(OneSpace_Dt_mapped, Nielsen_Dt_mapped)
Overall_Tax <- Overall_Tax[, lapply(.SD, max), by=.(UPC)]

# Remove whole muscle taggings for non treat categories
Overall_Tax <- Overall_Tax[UPC!=treat_upc,Whole.Muscle.Treat=0]