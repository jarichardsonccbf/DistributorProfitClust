library(tidyverse)

mm.sysco <- read.csv("data/MMdataSysco.csv") %>% 
  filter(Customer != "DFLT_") %>%  
  droplevels()

mm2 <- read.csv("data/MMdata2.csv") %>% 
  filter(Customer != "DFLT_") %>% 
  droplevels()

mm.data <- read.csv("data/MMdata.csv") %>% 
  filter(DistributorName != "Reyes Beverage Group",
         DistributorName != "Amazon Direct Liberty",
         DistributorName != "Amazon Direct Swire",
         DistributorName != "Coca-Cola Beverages Florida",
         DistributorName != "Coca-Cola Beverages Northeast, Inc",
         DistributorName != "Chesterman Company",
         DistributorName != "Coca-Cola Bottling Company United",
         DistributorName != "Coca-Cola Consolidated, Inc",
         DistributorName != "Coca-Cola Southwest Beverages",
         DistributorName != "Atlantic Bottling Company",
         Customer != "DFLT_") %>%  
  droplevels()

mm.data <- rbind(mm.data, mm.sysco, mm2) %>% 
  mutate(Secondary.Volume = as.numeric(Secondary.Volume)) %>% 
  filter(Secondary.Volume > 0) %>% 
  mutate(Customer.host.code = as.numeric(Customer.host.code)) %>% 
  select(Customer.host.code) %>% 
  unique()

rm(mm.sysco, mm2)


equip.list <- read.csv("data/equip_list.csv") %>% 
  rename(Customer.host.code = Customer) %>% 
  #mutate(Customer.host.code = as.character(Customer.host.code)) %>% 
  select(Customer.host.code, Equipment.in.Market) %>% 
  unique() 


equip.perc <- mm.data %>% 
  left_join(equip.list, "Customer.host.code")

total.outlets <- equip.perc %>%
  count()

equip.outlets <- equip.perc %>%
  filter(Equipment.in.Market == 1) %>% 
  count()

equip <- cbind(equip.outlets, total.outlets)

colnames(equip) <- c("equip", "total")

equip %>% 
  mutate(`Equipment Percentage` = equip / total)

# Option 2 involves using only outlets that have been retained post cleanup:
# 
# source("05_oi_oiratio.R")
# 
# 
# rev.opex %>% head()
# 
# total.outlets <- rev.opex %>%
#   ungroup() %>% 
#   select(`Customer Name`, `Equipment Total OPEX`) %>%
#   unique() %>% 
#   count()
# 
# equip.outlets <- rev.opex %>%
#   ungroup() %>% 
#   select(`Customer Name`, `Equipment Total OPEX`) %>%
#   unique() %>% 
#   filter(`Equipment Total OPEX` > 1) %>% 
#   count()
# 
# equip <- cbind(equip.outlets, total.outlets)
# 
# colnames(equip) <- c("equip", "total")
#   
# equip %>% 
#   mutate(`Equipment Percentage` = equip / total)
