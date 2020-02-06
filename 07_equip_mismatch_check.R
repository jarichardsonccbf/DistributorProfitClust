source("05_oi_oiratio.R")

mm.check <- rev.opex %>% 
  ungroup() %>% 
  select(`Distributor Name`, `Customer Name`, `Account Number`, `LMP Volume`, `RTM Volume`, `Relevant Equipment Count`, `Postmix Equip Count`, `Cooler Count`, `Vending Equip Count`)

rm(rev.opex)

mm.check <- mm.check %>% 
  filter(`Relevant Equipment Count` != 0 |
           `Postmix Equip Count` != 0 |
           `Cooler Count` != 0 |
           `Vending Equip Count` != 0)

mm.check.lmp <- mm.check %>% 
  filter(`RTM Volume` == 0 &
           `LMP Volume` > 0) %>% 
  filter(`Cooler Count` > 0 |
           `Vending Equip Count` > 0)

mm.check.bc <- mm.check %>% 
  filter(`RTM Volume` > 0 &
           `LMP Volume` == 0) %>% 
  filter(`Postmix Equip Count` > 0)

mm.red.truck <- read.csv("data/mm_red_truck.csv") %>% 
  rename(`Account Number` = Customer) %>% 
  semi_join(mm.check.lmp, "Account Number")

mm.check.bc <- mm.check.bc %>% 
  left_join(mm.red.truck, )


library(xlsx)

# write.xlsx(as.data.frame(mm.check.lmp), file="deliverables/equipment_checks.xlsx", sheetName="sheet1", row.names=FALSE)

# write.xlsx(as.data.frame(mm.check.bc), file="deliverables/equipment_checks.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

# semijoin with mm for mm.check.bc