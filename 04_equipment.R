source("03_spring_pull.R")

lmp.only <- rev.opex %>% 
  filter(lmp.volume > 0 &
           rtm.vol < 1)

rtm.only <- rev.opex %>% 
  filter(lmp.volume < 1 &
           rtm.vol > 0)

blend <- rev.opex %>% 
  filter(lmp.volume > 0 &
           rtm.vol > 0)

# equipment in outlet from Jignesh

# show only ARTM outlets by equip type

equip.list <- read.csv("data/equip_list.csv") %>% 
  rename(Customer.host.code = Customer) %>%
  mutate(Customer.host.code = as.character(Customer.host.code)) %>% 
  semi_join(rev.opex, "Customer.host.code")

fountain.equip.ref <- equip.list %>% 
  filter(Equipment.Type == "POST MIX")

fountain.equip <- fountain.equip.ref %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n())

coolers.ref <- equip.list %>% 
  filter(Equipment.Type == "COOLER") 

coolers <- coolers.ref %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n())

vending.ref <- equip.list %>% 
  filter(Equipment.Type == "VENDING MACHINE") 

vending <- vending.ref %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n())

rm(rev.opex)

lmp.only <- lmp.only %>% 
  left_join(fountain.equip, by = "Customer.host.code") %>% 
  rename(relevent.equip.count = equip.count)

rtm.only <- rtm.only %>% 
  left_join(rbind(coolers, vending) %>% 
              group_by(Customer.host.code) %>% 
              summarise(equip.count = sum(equip.count)), by = "Customer.host.code") %>% 
  rename(relevent.equip.count = equip.count)

blend <- blend %>% 
  left_join(rbind(coolers, vending, fountain.equip) %>% 
              group_by(Customer.host.code) %>% 
              summarise(equip.count = sum(equip.count)), by = "Customer.host.code") %>% 
  rename(relevent.equip.count = equip.count)

equip.serv <- read.csv("data/equip_serv.csv")

equip.serv <- equip.serv %>% 
  select(-c(Actual.Finish.Date., Actual.Finish.Date, Actual.Release.Date, Actual.Shipment.Date, Actual.Start.Date, Actual.Work.in.Minutes, Asset.Subnumber, Basic.Finish.Time, Basic.Start.Date, Basic.Start.Date.Time, Basic.Start.Month, Basic.Start.Year, Basic.Start.Time, Calendar.Month, Calendar.Week, Cancel.Indicator, Changed.On.Date, Close.Date, Code.Cause.Text, Code.Cause, Code.Group.Cause, Code.Group.Cause.Text, Company.Code.Text, Company.Code,Completed.Service.Order, Completion.Time.in.Hours, Confirmation.Counter, Controlling.Area.Text, Controlling.Area, Created.On, Date.of.Notification, Delivery.Without.Service.Call, Distribution.Channel.Text, Distribution.Channel, Division, Equip.PLN.PLT, Equipment.Price.list, Equipment.Category, Equip.PLN.PLT, Equipment.Price.list, Equipment.Category, Equipment.Fulfillment.Date, Equipment.System.Status, EquipPriceList.Text, Fix.It.Right, Last.Order.Number, Main.Work.Center, Maintenance.Item, Maintenance.Plan, Maintenance.Planner.Group.Text, Maintenance.Planner.Group, Maintenance.Processing.Phase, Malfunction.Start.Date, Malfunction.Start.Time, Met.Request.Date, Net.Placements, Notification.Type, Number.of.Records, Number.Of.Orders, Order.System.Status, Order.User.Status, Open.Service.Orders, Order.Category, Order.Main.Work.Center, Order.Number.IHPA, Order.PLN.PLT, Order.Type, Phase.Order.Closed, Phase.Order.Completed, Phase.Order.Created, Phase.Order.Released, Placements, Plant.Name, Plant.Section, Plant.Text, Plant, Primary.Group.Text, Primary.Group, Priority.Text, Priority.Type.Text, Priority.Type, Priority, Reference.Date, Reference.Time, Release.Date, Required.End.Date, Required.End.Time, Responsible.Planner.Group.Department, Responsible.Planner.Group.Department.Text, Room, Sales.Group, Sales.Group.Text, Sales.Office.Text, Sales.Office, Sales.Organization.Text, Sales.Organization, Sales.Person.Text, Sales.Person, Scheduled.Finish.Date, Scheduled.Start.Time, Scheduled.Release.Date, Scheduled.Start.Date, Scheduled.Start.Time, Scheduled.Finish.Time, Secondary.Group.Text, Secondary.Group, Sort.Field, Sub.Trade.Channel, Sub.Trade.Channel.Text, Suppression.Reason, Suppression.Reason.Text, Technical.Completion.Date, Total.Work.Center, Total.Worked.Operations, Trade.Channel, Trade.Channel.Text, Valid.To.Date, Valid.From.Date, Warranty.End.Date, Warranty.Start.Date, Work.Center.Category.Text, Work.Center.Category, Work.Center.Text, Work.Center.Valid.From, Work.Center.Valid.To))

vending.serv.opex <- equip.serv %>% 
  semi_join(vending.ref, "Equipment") %>% 
  mutate(hourage = Average.Activity.Time / 60,
         Parts.Cost = Parts.Cost * -1,
         service.cost = hourage * 75 + Parts.Cost) %>%
  filter(hourage < 10) %>% 
  group_by(Sold.To) %>% 
  summarise(sum = sum(service.cost, na.rm = TRUE)) %>% 
  rename(Customer.host.code = Sold.To) %>% 
  mutate(Customer.host.code = as.factor(Customer.host.code))

coolers.serv.opex <- equip.serv %>% 
  semi_join(coolers.ref, "Equipment") %>% 
  mutate(hourage = Average.Activity.Time / 60,
         Parts.Cost = Parts.Cost * -1,
         service.cost = hourage * 75 + Parts.Cost) %>%
  filter(hourage < 10) %>% 
  group_by(Sold.To) %>% 
  summarise(sum = sum(service.cost, na.rm = TRUE)) %>% 
  rename(Customer.host.code = Sold.To) %>% 
  mutate(Customer.host.code = as.factor(Customer.host.code))

lmp.equip.serv.opex <- equip.serv %>% 
  semi_join(fountain.equip.ref, "Equipment") %>% 
  mutate(hourage = Average.Activity.Time / 60,
         Parts.Cost = Parts.Cost * -1,
         service.cost = hourage * 75 + Parts.Cost) %>%
  filter(hourage < 10) %>% 
  group_by(Sold.To) %>% 
  summarise(sum = sum(service.cost, na.rm = TRUE)) %>% 
  rename(Customer.host.code = Sold.To) %>% 
  mutate(Customer.host.code = as.factor(Customer.host.code))

lmp.only <- lmp.only %>% 
  left_join(lmp.equip.serv.opex, by = "Customer.host.code") %>% 
  rename(equip.serv.opex = sum)

rtm.only <- rtm.only %>% 
  left_join(rbind(coolers.serv.opex, vending.serv.opex) %>% 
              group_by(Customer.host.code) %>% 
              summarise(sum = sum(sum)), by = "Customer.host.code") %>% 
  rename(equip.serv.opex = sum)

blend <- blend %>% 
  left_join(rbind(coolers.serv.opex, vending.serv.opex, lmp.equip.serv.opex) %>% 
              group_by(Customer.host.code) %>% 
              summarise(sum = sum(sum)), by = "Customer.host.code") %>% 
  rename(equip.serv.opex = sum)

lmp.only$equip.serv.opex[is.na(lmp.only$equip.serv.opex)] <- 0
rtm.only$equip.serv.opex[is.na(rtm.only$equip.serv.opex)] <- 0
blend$equip.serv.opex[is.na(blend$equip.serv.opex)] <- 0

rm(equip.serv, fountain.equip, coolers, vending, vending.serv.opex, lmp.equip.serv.opex, coolers.serv.opex, equip.list)

# equip depr

equip.depr <- read.csv("data/equip_depr.csv", stringsAsFactors = FALSE)

fountain.equip <- fountain.equip.ref %>% 
  left_join(equip.depr, by = "Model.Group") %>% 
  mutate(Model.Group = as.character(Model.Group)) %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.depr.opex = sum(depr))

coolers <- coolers.ref %>% 
  left_join(equip.depr, by = "Model.Group") %>% 
  mutate(Model.Group = as.character(Model.Group)) %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.depr.opex = sum(depr))

vending <- vending.ref %>% 
  left_join(equip.depr, by = "Model.Group") %>% 
  mutate(Model.Group = as.character(Model.Group)) %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.depr.opex = sum(depr))

lmp.only <- lmp.only %>% 
  left_join(fountain.equip, by = "Customer.host.code")

  rtm.only <- rtm.only %>% 
  left_join(rbind(coolers, vending) %>% 
              group_by(Customer.host.code) %>% 
              summarise(equip.depr.opex = sum(equip.depr.opex)), by = "Customer.host.code")

blend <- blend %>% 
  left_join(rbind(coolers, vending, fountain.equip) %>% 
              group_by(Customer.host.code) %>% 
              summarise(equip.depr.opex = sum(equip.depr.opex)), by = "Customer.host.code")

lmp.only$equip.depr.opex[is.na(lmp.only$equip.depr.opex)] <- 0
rtm.only$equip.depr.opex[is.na(rtm.only$equip.depr.opex)] <- 0
blend$equip.depr.opex[is.na(blend$equip.depr.opex)] <- 0

rev.opex <- rbind(lmp.only,
                  rtm.only,
                  blend)

equip.list <- read.csv("data/equip_list.csv") %>% 
  rename(Customer.host.code = Customer) %>%
  mutate(Customer.host.code = as.character(Customer.host.code)) %>% 
  semi_join(rev.opex, "Customer.host.code")

fountain.equip <- equip.list %>% 
  filter(Equipment.Type == "POST MIX") %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n()) %>% 
  rename(fount.count = equip.count)

coolers <- equip.list %>% 
  filter(Equipment.Type == "COOLER")  %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n()) %>% 
  rename(cooler.count = equip.count)

vending <- equip.list %>% 
  filter(Equipment.Type == "VENDING MACHINE") %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.count = n()) %>% 
  rename(vending.count = equip.count)

rev.opex <- rev.opex %>% 
  left_join(fountain.equip, by = "Customer.host.code") %>% 
  left_join(coolers, by = "Customer.host.code") %>%
  left_join(vending, by = "Customer.host.code")

rev.opex$relevent.equip.count[is.na(rev.opex$relevent.equip.count)] <- 0
rev.opex$cooler.count[is.na(rev.opex$cooler.count)] <- 0
rev.opex$fount.count[is.na(rev.opex$fount.count)] <- 0
rev.opex$vending.count[is.na(rev.opex$vending.count)] <- 0

rm(equip.depr, blend, coolers, coolers.ref, equip.list, fountain.equip, fountain.equip.ref, lmp.only, rtm.only, vending, vending.ref)
