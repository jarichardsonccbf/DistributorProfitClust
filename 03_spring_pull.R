library(RJDBC)
library(keyring)
library(lubridate)

source("02_cts.R")

# import and establish hana connection ----

options(java.parameters = "-Xmx8048m")
# memory.limit(size=10000000000024)

# classPath="C:/Program Files/sap/hdbclient/ngdbc.jar"
# For ngdbc.jar use        # jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
# For HANA Studio jar use  # jdbcDriver <- JDBC(driverClass="com.sap.ndb.studio.jdbc.JDBCConnection",

# jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",
#                    classPath="C:/Program Files/sap/hdbclient/ngdbc.jar")
# 
# jdbcConnection <- dbConnect(jdbcDriver,
#                             "jdbc:sap://vlpbid001.cokeonena.com:30015/_SYS_BIC",
#                             "fl014036",
#                             "jasoN3#w")
# 
# # Fetch all results
# 
# sql <- "SELECT \"VISITTYPE\",
#                       \"ACCOUNT\",
#                       \"PLANNEDSTART\",
#                       \"EXECUTIONSTART\",
#                       \"EXECUTIONEND\",
#                       \"STATUS\",
#                       \"SOURCE\",
#                       \"ACCOUNT_NAME\",
#                       \"User_Display\"
#          FROM \"cona-reporting.field-sales::Q_CA_R_SpringVisit\"
#          WHERE
#          \"VISITTYPE\" = ? and
#          \"STATUS\" = ? and
#          \"SOURCE\" = ? and
#          \"PLANNEDSTART\" >= ? and
#          \"PLANNEDEND\" <= ?"
# 
# param1 <- 'ZA'
# param2 <- 'FINAL'
# param3 <- 'INT'
# param4 <- '2018-11-01 00:00:00'
# param5 <- '2019-10-31 00:00:00'
# 
# spring.visit <- dbGetQuery(jdbcConnection, sql,
#                             param1,
#                             param2,
#                             param3,
#                             param4,
#                             param5)

rm(jdbcConnection, jdbcDriver, param1, param2, param3, param4, param5, sql, jdbcDriver)

# saveRDS(spring.visit, "data/spring.RDS")

spring.visit <- readRDS("data/spring.RDS") %>% 
  rename(Customer.host.code = ACCOUNT) %>% 
  mutate(Customer.host.code = substr(Customer.host.code, 2, 10)) %>% 
  semi_join(rev.opex, by = "Customer.host.code")

# get visit count per outlet

sam.count <- spring.visit %>% 
  group_by(Customer.host.code) %>% 
  count()

sam.hours <- spring.visit %>%  
  mutate(hours = ((ymd_hms(EXECUTIONEND) - ymd_hms(EXECUTIONSTART)) / 60) / 60) %>%
  filter(hours > 0.01666667) %>% 
  filter(hours < 5) %>% 
  group_by(Customer.host.code) %>% 
  summarise(total.hours = sum(hours))


# Tie in to artm data

rev.opex <- rev.opex %>% 
  left_join(sam.hours, "Customer.host.code") %>% 
  mutate(sam.opex = total.hours * 27)

rev.opex$sam.opex[is.na(rev.opex$sam.opex)] <- 0

rm(sam.count, sam.hours, spring.visit)
