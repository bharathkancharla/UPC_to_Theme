setwd("D:/Nestle/Emerging/Results_13052020")
#### Comparision between old and updated code run
OldFilePath <- "C:/Users/bharath.kancharla/Downloads/"
OS_old_file <- read.csv("C:/Users/bharath.kancharla/Downloads/OneSpace_Claims_Aggr_mapped_81k_07_05_2020 (1).csv", stringsAsFactors = F,
                        na.strings = c("", "NA"))

Nlsn_old_file <- read.csv(paste0(OldFilePath, "Nielsen_Dt_mapped_with_Nestle.csv"), stringsAsFactors = F,
                         na.strings = c("", "NA"))

NewFilePath <- "D:/Nestle/Emerging/Results_13052020/"
OS_new_file <- read.csv("D:/Nestle/Emerging/Results_13052020/OneSpace_Claims_Aggr_mapped_81k_13052020.csv", stringsAsFactors = F,
                        na.strings = c("", "NA"))
Nlsn_new_file <- read.csv(paste0(NewFilePath, "Nielsen_Dt_mapped_with_Nestle.csv"), stringsAsFactors = F,
                         na.strings = c("", "NA"))

#### I used the "_mapped_with_Nestle" files so I am removing the tags done by Nestle, and (-2) in line 16 is 
# I am removing the OneSPace Text column from the dataframe
#OS_old_file <- OS_old_file[,-which(grepl("Nestle",names(OS_old_file)))]
OS_old_file <- OS_old_file[,-2]
Nlsn_old_file <- Nlsn_old_file[,-which(grepl("Nestle",names(Nlsn_old_file)))]

#OS_new_file <- OS_new_file[,-which(grepl("Nestle",names(OS_new_file)))]
OS_new_file <- OS_new_file[,-2]
Nlsn_new_file <- Nlsn_new_file[,-which(grepl("Nestle",names(Nlsn_new_file)))]
k <- "OS"

for (k in c("OS", "NLSN")){
  if (k == "OS"){
    new1 <- OS_new_file
    old1 <- OS_old_file
  } else {
    new1 <- Nlsn_new_file
    old1 <- Nlsn_old_file
  }
  Dt2 <- data.frame()
  for (j in colnames(old1)[2:ncol(old1)]){
    old_upc <- old1 %>% filter(old1[,j] == 1) %>% select("UPC")
    new_upc <- new1 %>% filter(new1[,j] == 1) %>% select("UPC")
    common_upc <- intersect(old_upc$UPC, new_upc$UPC)
    common_numbers <- as.integer(length(common_upc))
    old_only <- paste0(setdiff(old_upc$UPC, common_upc), collapse = ", ")
    new_only <- paste0(setdiff(new_upc$UPC, common_upc), collapse = ", ")
    old_only_number <- length(setdiff(old_upc$UPC, common_upc))
    new_only_number <- length(setdiff(new_upc$UPC, common_upc))
    
    if (nrow(Dt2)==0){
      Dt2 <- data.frame(Theme = j, old_only = old_only,new_only=new_only,
                        common = common_numbers, old_only_numbers = old_only_number, new_only_number = new_only_number)
    } else {
      Dt2 <- rbind(Dt2, data.frame(Theme = j, old_only = old_only,new_only=new_only,
                                   common = common_numbers, old_only_numbers = old_only_number, new_only_number = new_only_number))
    }
  }
  if (k == "OS"){
    OS <- Dt2
  } else {
    NLSN <- Dt2
  }
  write.csv(Dt2, paste0(k, "_comarision_upc_JAN.csv"), row.names = F)
}
