# OI = DNGP - OPEX
# DNGP = DNR - COGS
# OIRATIO = OI / REVENUE

options(scipen=999)

library(tidyverse)

# read MM files and combine them ----

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
  mutate(Customer.host.code = as.factor(Customer.host.code)) 

rm(mm.sysco, mm2)

price.cogs <- read.csv("data/cogs_price.csv")


# Get outlet LMP rev ----

# lmp overall vol by package type

lmp.vol <- mm.data %>%
  filter(TransactionType == "LMP") %>% 
  group_by(PackageCategory) %>%
  mutate(Secondary.Volume = as.numeric(Secondary.Volume)) %>% 
  summarise(sum = sum(as.numeric(Secondary.Volume), na.rm = TRUE))


lmp.rev <- mm.data %>%
  filter(TransactionType == "LMP") %>% 
  filter(PackageCategory == "1 GALLON 1-LS 2" |
           PackageCategory == "	2.5 GALLON 1-LS" |
           PackageCategory == "2.5 GALLON FROZEN 1-LS" |
           PackageCategory == "5 GALLON FROZEN 1-LS" |
           PackageCategory == "5 GALLON 1-LS") %>% 
  group_by(DistributorName, Customer, Customer.host.code) %>% 
  summarise(lmp.volume = sum(as.numeric(Secondary.Volume))) %>% 
  mutate(lmp.rev = lmp.volume * 5.25)


# Get outlet RTM rev ----

# first vol by pack and brand to see what's going on with those with no cogs

rtm.vol <- mm.data %>% 
  filter(TransactionType == "RTM") %>%
  left_join(price.cogs, by = c("PackageCategory", "BrandCategory")) %>% 
  select(PackageCategory, BrandCategory, COGs.Case, price, Secondary.Volume) %>% 
  group_by(PackageCategory, BrandCategory, COGs.Case, price) %>% 
  summarise(sum = sum(as.numeric(Secondary.Volume)))
  
# write.csv(rtm.vol, "outputs/missingprice.csv", row.names = FALSE)
  
rtm.rev <- mm.data %>% 
  filter(TransactionType == "RTM") %>%
  left_join(price.cogs, by = c("PackageCategory", "BrandCategory")) %>% 
  mutate(rtm.rev = as.numeric(Secondary.Volume) * price,
         rtm.cogs = as.numeric(Secondary.Volume) * COGs.Case,
         rtm.dngp = rtm.rev - rtm.cogs) %>% 
  group_by(DistributorName, Customer, Customer.host.code)

rtm.rev$Secondary.Volume[is.na(rtm.rev$Secondary.Volume)] <- 0
rtm.rev$rtm.rev[is.na(rtm.rev$rtm.rev)] <- 0
rtm.rev$rtm.cogs[is.na(rtm.rev$rtm.cogs)] <- 0
rtm.rev$COGs.Case[is.na(rtm.rev$COGs.Case)] <- 0
rtm.rev$price[is.na(rtm.rev$price)] <- 0
rtm.rev$rtm.dngp[is.na(rtm.rev$rtm.dngp)] <- 0

rtm.rev <- rtm.rev %>% 
  group_by(DistributorName, Customer, Customer.host.code) %>% 
  summarise(rtm.vol = sum(as.numeric(Secondary.Volume)),
            rtm.cogs = sum(rtm.cogs),
            rtm.rev = sum(rtm.rev),
            rtm.dngp = sum(rtm.dngp))

rm(lmp.vol, mm.data, price.cogs, rtm.vol)


# sum revenues ----

rev <- lmp.rev %>% 
  full_join(rtm.rev, by = "Customer.host.code")

rev$lmp.rev[is.na(rev$lmp.rev)] <- 0
rev$rtm.dngp[is.na(rev$rtm.dngp)] <- 0
rev$rtm.rev[is.na(rev$rtm.rev)] <- 0
rev$rtm.vol[is.na(rev$rtm.vol)] <- 0

rev <- rev %>% 
  mutate(total.rev = rtm.rev + lmp.rev,
         total.dngp = rtm.dngp + lmp.rev)

rm(lmp.rev, rtm.rev)
