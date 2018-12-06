rm(list = ls())

# List of packages for session
package_list <- c("ggplot2", "dplyr", "data.table")

# Install CRAN packages (if not already installed)
new_packages <- package_list[!(package_list %in% installed.packages()[, "Package"])]
if(length(new_packages) > 0){
  install.packages(new_packages)
}

# Load packages into session 
lapply(package_list, require, character.only = TRUE)

# Load the data
subscription <- fread("subscription_nonull.csv", sep = "|")

# Remove the two years old data

subscription <- subscription[, c(2:50)] 
colnames(subscription)[which(names(subscription)== "SubTermSum")] <- "SubIssueSum"

setkey(subscription, KDB_ID, SubPubCode, IntroStartIssueIssueDate)
subscription<- subscription[(IntroStartIssueIssueDate >= (Sys.Date() - 365*2)) | (SubEndIssueIssueDate >= (Sys.Date() - 365*2))]

View(head(subscription, 10))

# Create Features

subscription[, `:=` (
  is_active = ifelse(!CancellationDate == "", 0L , 1L), # 0= passive, 1 = active
  passive_days = as.Date(shift(CancellationDate, n= 1), format = "%Y-%m-%d") - max(as.Date(IntroStartIssueIssueDate, format = "%Y-%m-%d"))), 
by = .(KDB_ID, SubPubCode)]

subscription[, ShipCustomerCount_hist := uniqueN(SubShipToCustomer), by = .(KDB_ID, SubPubCode)]
subscription[, LastTermSubType_hist := .(list(LastTermSubType)), by = .(KDB_ID, SubPubCode)]
subscription[, CountSubType_hist := lapply(LastTermSubType_hist, function (x) length (x)), by = .(KDB_ID, SubPubCode)]
subscription[, CountSubTypeUnique_hist := lapply(LastTermSubType_hist, function (x) length(unique(x))), by = .(KDB_ID, SubPubCode)]
subscription[, other_LastTermSubType_hist := mapply(setdiff, x=LastTermSubType_hist, y=LastTermSubType)]
# setdiff function will keep the diffrence values only from "x" column, not "y"

#dt[, gearsL11 := lapply(gearsL, `[`, 1)]
#dt[, gearsL2 := sapply(gearsL,function(x) x[length(x)])]
 


