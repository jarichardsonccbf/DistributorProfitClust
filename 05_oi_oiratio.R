# OI = DNGP - OPEX
# DNGP = DNR - COGS
# OIRATIO = OI / REVENUE

source("04_equipment.R")

rev.opex <- rev.opex %>% 
  mutate(total.equipment.opex = equip.depr.opex + equip.serv.opex,
         total.outlet.opex = equip.depr.opex + equip.serv.opex + sam.opex + outlets.opex.burden,
         OI = total.dngp - total.outlet.opex,
         OIratio = OI / total.rev)

colnames(rev.opex) <- c("Distributor Name", 
                 "Customer Name", 
                 "Account Number", 
                 "LMP Volume", 
                 "LMP Revenue", 
                 "RTM Volume", 
                 "RTM COGS", 
                 "RTM Revenue", 
                 "RTM DNGP", 
                 "Total Revenue", 
                 "Total DNGP", 
                 "Distributor Revenue", 
                 "Distributor RTM Revenue",
                 "Distributor LMP Revenue",
                 "Outlet RTM Revenue Percentage", 
                 "Distributor OPEX", 
                 "Outlet OPEX Burden", 
                 "SubChannel",
                 "Channel",
                 "KeyAccount",
                 "TradeName",
                 "Total Hours", 
                 "SAM OPEX", 
                 "Equipment Service OPEX", 
                 "Equipment Depreciation OPEX", 
                 "Equipment Count",
                 "Equipment Total OPEX",
                 "Outlet Total OPEX", 
                 "OI",
                 "OI Ratio")

rev.opex %>% 
  group_by(`Distributor Name`) %>% 
  summarise(mean.oi.ratio = mean(as.numeric(`OI Ratio`)))

write.csv(rev.opex, "deliverables/artm_profit_raw.csv", row.names = FALSE)
