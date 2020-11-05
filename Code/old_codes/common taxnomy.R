setwd("D:/Nestle/Emerging/Results_12052020")
OneSpace_Claims_Aggr_mapped <- read.csv("OneSpace_Claims_Aggr_mapped_81k_12052020.csv")
OneSpace_Claims_Aggr_mapped_txt <- OneSpace_Claims_Aggr_mapped %>% group_by(UPC) %>% summarise(OneSpace.Claim.Text = paste(OneSpace.Claim.Text, collapse = " "))
OneSpace_Claims_Aggr_mapped <- OneSpace_Claims_Aggr_mapped[,-which(colnames(OneSpace_Claims_Aggr_mapped) == "OneSpace.Claim.Text")]
OneSpace_Claims_Aggr_mapped <- OneSpace_Claims_Aggr_mapped %>% group_by(UPC) %>% summarise_all(max)
OneSpace_Claims_Aggr_mapped <- merge(OneSpace_Claims_Aggr_mapped_txt, OneSpace_Claims_Aggr_mapped, 
                                     by = "UPC", all = F)
Nielsen_Dt_mapped_tax <- read.csv("Nielsen_Dt_mapped_txnm_12052020.csv")
Nielsen_Dt_pdt_vr_mapped <- read.csv("Nielsen_Dt_pdt_vr_mapped_12052020.csv")
Nielsen_Dt_pdt_abbr_mapped <- read.csv("Nielsen_Dt_pdt_abbr_mapped.csv")
Nielsen_Dt_mapped <- rbind(Nielsen_Dt_mapped_tax[-2], Nielsen_Dt_pdt_vr_mapped[-2],
                           Nielsen_Dt_pdt_abbr_mapped[-2])

Nielsen_Dt_mapped <- Nielsen_Dt_mapped %>% group_by(UPC) %>% summarise_all(max)

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
Common_Tax$Themes <- tolower(Common_Tax$Themes)
Main_Mapping_table_tax <- read.csv("Main_Mapping_table_tax.csv")
overall_summary <- merge(Main_Mapping_table_tax, Common_Tax, by = "Themes", all = T)
write.csv(overall_summary, "overall_summary.csv")
