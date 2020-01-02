# Use this file for cts data for distirbutor

source("01_calculate_rev_dngp.R")

library(readxl)

cts <- read.csv("data/cts.csv") %>% 
  select(DistributorName, PB.Total.Opex) %>% 
  mutate(PB.Total.Opex = parse_number(as.character(PB.Total.Opex))) %>% 
  group_by(DistributorName) %>% 
  summarise(OPEX.dist = sum(PB.Total.Opex))


# calculate rev per dist

rev$DistributorName.x[is.na(rev$DistributorName.x)] <- as.character(rev$DistributorName.y[is.na(rev$DistributorName.x)])

rev$Customer.x[is.na(rev$Customer.x)] <- as.character(rev$Customer.y[is.na(rev$Customer.x)])

dist.rev <- rev %>% 
  group_by(DistributorName.x) %>% 
  summarise(dist.rev = sum(total.rev, na.rm = T))

# divide individual outlet rev by these values

rev <- rev %>% 
  select(-c(Customer.y, DistributorName.y)) %>% 
  left_join(dist.rev, by = "DistributorName.x") %>% 
  mutate(outlet.rev.perc = total.rev / dist.rev)

# tie in cts opex

rev.opex <- rev %>% 
  rename(DistributorName = DistributorName.x) %>% 
  inner_join(cts, by = "DistributorName")

# and calculate % of opex to give to each outlet

rev.opex <- rev.opex %>% 
  mutate(outlets.opex.burden = outlet.rev.perc * OPEX.dist)  
  

# sanity check

rev.opex %>% 
  group_by(DistributorName, OPEX.dist) %>% 
  summarise(sum = sum(outlets.opex.burden))

# outlet burdens sum to distirbutor opex totals

# filter natl acts ----

# cust.master <- read.csv("data/master.customer.csv") %>% 
#   rename(Customer.host.code = Customer) %>% 
#   mutate(Customer.host.code = as.character(Customer.host.code),
#          Customer.host.code = str_sub(Customer.host.code, end = 9)) %>% 
#   select(-c(Name, Customer.1))
# 
# rev.opex <- rev.opex %>% 
#   left_join(cust.master, by = "Customer.host.code")

rm(cts, cust.master, dist.rev, rev)
