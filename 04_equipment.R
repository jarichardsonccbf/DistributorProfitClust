library(tidyverse)

source("03_spring_pull.R")

# equipment in outlet from Jignesh

# show only ARTM outlets

equip.list <- read.csv("data/equip_list.csv") %>% 
  rename(Customer.host.code = Customer) %>% 
  mutate(Customer.host.code = as.character(Customer.host.code)) %>% 
  semi_join(rev.opex, "Customer.host.code")
  

equip.serv <- read.csv("data/equip_serv.csv")
  
equip.serv <- equip.serv %>% 
  select(-c(Actual.Finish.Date., Actual.Finish.Date, Actual.Release.Date, Actual.Shipment.Date, Actual.Start.Date, Actual.Work.in.Minutes, Asset.Subnumber, Basic.Finish.Time, Basic.Start.Date, Basic.Start.Date.Time, Basic.Start.Month, Basic.Start.Year, Basic.Start.Time, Calendar.Month, Calendar.Week, Cancel.Indicator, Changed.On.Date, Close.Date, Code.Cause.Text, Code.Cause, Code.Group.Cause, Code.Group.Cause.Text, Company.Code.Text, Company.Code,Completed.Service.Order, Completion.Time.in.Hours, Confirmation.Counter, Controlling.Area.Text, Controlling.Area, Created.On, Date.of.Notification, Delivery.Without.Service.Call, Distribution.Channel.Text, Distribution.Channel, Division, Equip.PLN.PLT, Equipment.Price.list, Equipment.Category, Equip.PLN.PLT, Equipment.Price.list, Equipment.Category, Equipment.Fulfillment.Date, Equipment.System.Status, EquipPriceList.Text, Fix.It.Right, Last.Order.Number, Main.Work.Center, Maintenance.Item, Maintenance.Plan, Maintenance.Planner.Group.Text, Maintenance.Planner.Group, Maintenance.Processing.Phase, Malfunction.Start.Date, Malfunction.Start.Time, Met.Request.Date, Net.Placements, Notification.Type, Number.of.Records, Number.Of.Orders, Order.System.Status, Order.User.Status, Open.Service.Orders, Order.Category, Order.Main.Work.Center, Order.Number.IHPA, Order.PLN.PLT, Order.Type, Phase.Order.Closed, Phase.Order.Completed, Phase.Order.Created, Phase.Order.Released, Placements, Plant.Name, Plant.Section, Plant.Text, Plant, Primary.Group.Text, Primary.Group, Priority.Text, Priority.Type.Text, Priority.Type, Priority, Reference.Date, Reference.Time, Release.Date, Required.End.Date, Required.End.Time, Responsible.Planner.Group.Department, Responsible.Planner.Group.Department.Text, Room, Sales.Group, Sales.Group.Text, Sales.Office.Text, Sales.Office, Sales.Organization.Text, Sales.Organization, Sales.Person.Text, Sales.Person, Scheduled.Finish.Date, Scheduled.Start.Time, Scheduled.Release.Date, Scheduled.Start.Date, Scheduled.Start.Time, Scheduled.Finish.Time, Secondary.Group.Text, Secondary.Group, Sort.Field, Sub.Trade.Channel, Sub.Trade.Channel.Text, Suppression.Reason, Suppression.Reason.Text, Technical.Completion.Date, Total.Work.Center, Total.Worked.Operations, Trade.Channel, Trade.Channel.Text, Valid.To.Date, Valid.From.Date, Warranty.End.Date, Warranty.Start.Date, Work.Center.Category.Text, Work.Center.Category, Work.Center.Text, Work.Center.Valid.From, Work.Center.Valid.To))

 # filter down to artm equip

equip.serv <- equip.serv %>% 
  mutate(hourage = Average.Activity.Time / 60,
         Parts.Cost = Parts.Cost * -1,
         service.cost = hourage * 75 + Parts.Cost) %>%
  filter(hourage < 10) %>% 
  semi_join(equip.list, "Equipment") %>% 
  group_by(Sold.To) %>% 
  summarise(sum = sum(service.cost, na.rm = TRUE)) %>% 
  rename(Customer.host.code = Sold.To) %>% 
  mutate(Customer.host.code = as.factor(Customer.host.code))

rev.opex <- rev.opex %>% 
  left_join(equip.serv, "Customer.host.code") %>% 
  rename(equip.serv.opex = sum)

rev.opex$equip.serv.opex[is.na(rev.opex$equip.serv.opex)] <- 0

rm(equip.serv)

# equip depr

equip.depr <- read.csv("data/equip_depr.csv", stringsAsFactors = FALSE)

equip.list <- equip.list %>% 
  mutate(Model.Group = as.character(Model.Group),
         Model.Group = gsub(" ", "_", equip.list$Model.Group)) %>% 
  left_join(equip.depr, by = "Model.Group") %>% 
  group_by(Customer.host.code) %>% 
  summarise(equip.depr.opex = sum(depr))

rev.opex <- rev.opex %>% 
  left_join(equip.list, "Customer.host.code")

rev.opex$equip.depr.opex[is.na(rev.opex$equip.depr.opex)] <- 0

rm(equip.serv, equip.depr, equip.list)

# write.csv(rev.opex, "deliverables/artm_profit.csv")
