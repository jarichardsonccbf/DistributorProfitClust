# OI = DNGP - OPEX
# DNGP = DNR - COGS
# OIRATIO = OI / REVENUE

rev.opex <- rev.opex %>% 
  mutate(total.outlet.opex = equip.depr.opex + equip.serv.opex, sam.opex, outlets.opex.burden,
         OI = total.dngp - total.outlet.opex,
         OIratio = OI / total.rev)

