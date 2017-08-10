# Data Analysis

#library(openxlsx)

#set working directory (modify path as needed)
setwd("D:/Automation/Use Cases/Shipment Delays")
rm(list=ls())


# Load Data
X = read.csv(file='ship_Data.csv')

# Remove Comma from a numric field
X$ORDER_NUMBER = as.numeric(gsub(",", "", X$ORDER_NUMBER))
# Remove Column from Data frame
X <- subset( X, select = -SET_ID1)

# Variable transformations as required
X$SHIPMENT_AMOUNT = as.numeric(X$SHIPMENT_AMOUNT)
X$ORDER_NUMBER = as.numeric(X$ORDER_NUMBER)
X$BOOKED_DATE =  as.POSIXct(X$BOOKED_DATE, format = "%m/%d/%Y %H:%M:%S") 
# -AG- For All the date fields we should consider time stamp too. As there would be some milestones happening within hours timestamps will help
X$ROUTE_DERIVED =  as.POSIXct(X$ROUTE_DERIVED, format = "%m/%d/%Y %H:%M:%S")
X$MFG_DATE = as.POSIXct(X$MFG_DATE, format = "%m/%d/%Y %H:%M:%S")
X$CM_SSD  = as.POSIXct(X$CM_SSD, format = "%m/%d/%Y %H:%M:%S")
X$PACKOUT_DATE  = as.POSIXct(X$PACKOUT_DATE, format = "%m/%d/%Y %H:%M:%S")
X$DELIVERY_CREATED  = as.POSIXct(X$DELIVERY_CREATED, format = "%m/%d/%Y %H:%M:%S")
X$PICK_RELEASED  = as.POSIXct(X$PICK_RELEASED, format = "%m/%d/%Y %H:%M:%S")
X$EXPECTED_SLC_SD  = as.POSIXct(X$EXPECTED_SLC_SD, format = "%m/%d/%Y %H:%M:%S")
X$ACTUAL_SLC_SD  = as.POSIXct(X$ACTUAL_SLC_SD, format = "%m/%d/%Y %H:%M:%S")
X$EXPECTED_DELIVERY_DATE  = as.POSIXct(X$EXPECTED_DELIVERY_DATE, format = "%m/%d/%Y %H:%M:%S")
X$DELIVERED_DATE  = as.POSIXct(X$DELIVERED_DATE, format = "%m/%d/%Y %H:%M:%S")

# Overall Summary of Data
names(X)
summary(X)

# COLUMNS 1-5
# -AG- Order Number Concatenated with Set Name makes a Unique shipment/Order# e.g. ORDER_NUMBER- 13815672	SET_NAME - 29	SHIPMENT_NUM - 13815672-29

#summary(X$ORDER_NUMBER)
summary(X$SHIPMENT_NUM)
#ord_num = sort(table(X$ORDER_NUMBER),decreasing=T) # % unique orders
ship_num = sort(table(X$SHIPMENT_NUM),decreasing=T)
#ord_num[1]
ship_num[1]
#summary(X$SET_NAME) # Any relevant meaning?
#sort(table(X$ORDER_TYPE),decreasing=T) # % order types #which order types are included

# COLUMNS 6-10
summary(X$FLOW_STATUS_CODE) # 56220 closed lines 
summary(X$SHIP_FROM_ORG_ID) # Has only one value - 383
summary(X$OPT_IN_OUT) # 2 values C and S. Meaning?
# -AG- C means Cisco Owned Shipment S means Self or Customer Owned Shipment
ship_metcode = sort(table(X$SHIPPING_METHOD_CODE),decreasing=T) 
ship_metcode[1:10] # 100 codes exist. any meaning?
# -AG- Field used to identifies mode of shipment : Ocean / Air / Road

# COLUMNS 11-15
names(X[,11:15])
summary(X$SHIPMENT_AMOUNT) # from 1 to 15030
summary(X$SHIP_FROM) # Has only one value - INFYJMX
# -AG- Field means Inventory Organization/Contract Manufacturer
ship_to = sort(table(X$SHIP_TO),decreasing=T)
ship_to[1:5] # 48k out of 56k belong to US, CA
summary(X$OTM_IB_SHIPPING_ROUTE_CODE) # Mostly COC-SLCGUADSH

# COLUMNS 16-20
names(X[,16:20])
summary(X$SHIPMENT_PRIORITY_CODE) # 45k are PR7
bkd_dt=sort(X$BOOKED_DATE)
summary(bkd_dt)
bkd_dt[1:20] # 1+7 outliers
summary(X$ROUTE_DERIVED) # 3 NA values
class(X$MFG_DATE)
summary(X$MFG_DATE)
mfg_dt = sort(X$MFG_DATE)
mfg_dt[1:20] # 2+2 outliers
summary(X$CM_SSD)
ssd_dt = sort(X$CM_SSD)
ssd_dt[1:20] # Looks clean

# -AG- Mostly all Scheduled/Planned Dates should not have any outliers as it is given by Oracle Planning Engine.
# Actual Dates will have outliers as milestone event/data could be delayed due to to multiple reasons like Physical Delay/ System Delay etc

# COLUMNS 21-25
names(X[,21:25])

summary(X$PACKOUT_DATE) # 14 NAs
pkout_dt = sort(X$PACKOUT_DATE)
pkout_dt[1:20] # 2+2 outliers

summary(X$DELIVERY_CREATED) # 42 NAs
delcr_dt = sort(X$DELIVERY_CREATED)
delcr_dt[1:20] # 2+2 outliers

summary(X$PICK_RELEASED) # 105 NAs
pkrel_dt = sort(X$PICK_RELEASED)
pkrel_dt[1:20] # 2+2 outliers

summary(X$EXPECTED_SLC_SD) # 3 NAs
exp_slcsd = sort(X$EXPECTED_SLC_SD)
exp_slcsd[1:20] # looks ok

summary(X$ACTUAL_SLC_SD) # 230 NAs
act_slcsd = sort(X$ACTUAL_SLC_SD)
act_slcsd[1:20] # 2+2 outliers

# COLUMNS 26-30
names(X[,26:30])
summary(X$EXPECTED_DELIVERY_DATE) # no NAs
ed_dt = sort(X$EXPECTED_DELIVERY_DATE)
ed_dt[1:20] # looks ok

summary(X$DELIVERED_DATE) # 2384 NAs
del_dt = sort(X$DELIVERED_DATE)
del_dt[1:20] # 2 + 2 outliers

summary(X$LAST_MILESTONE) # 51k are ship confirmed
summary(X$FISCAL_MONTH) # may,jun,jul 2017
summary(X$FISCAL_QUARTER) # only Q4FY17


# delivery date delays
del_mismatch = as.numeric(X$EXPECTED_DELIVERY_DATE - X$DELIVERED_DATE)
summary(del_mismatch)
delayeddel <- which(del_mismatch<0)
delayeddel
X[52982,]
count(delayeddel)

X$slcsd_mismatch = as.numeric(X$EXPECTED_SLC_SD - X$ACTUAL_SLC_SD)
summary(slcsd_mismatch)
delayed_slcsd = which(slcsd_mismatch<0)
delayed_slcsd
X[40655,]
sum(delayed_slcsd)

summary(X$SHIPMENT_AMOUNT)# Hardly any variance based on shipment amount
summary(X$SHIPMENT_AMOUNT[delayed_slcsd])
summary(X$SHIPMENT_AMOUNT[-delayed_slcsd])


X$SLCSDSLA <- ifelse(X$slcsd_mismatch < 0, 'SLA_MISS', 'SLA_MET')
table(X$SLCSDSLA)

##Sampling the Data

set.seed(123)
samp <- sample(nrow(X), 0.6 * nrow(X))
train <- X[samp, ]
test <- X[-samp, ]

# Unique value sin Column

df <- unique(train_us$SHIPPING_METHOD_CODE)

train_us = filter(train, SHIP_TO == "US", OPT_IN_OUT=="C")

write.csv(train_us, file = "train_us.csv")

# Random Forest


output.forest <- randomForest(SLCSDSLA ~ SHIP_FROM + OTM_IB_SHIPPING_ROUTE_CODE + OPT_IN_OUT ,data = train)

output.forest <- randomForest(SHIPMENT_AMOUNT ~ OTM_IB_SHIPPING_ROUTE_CODE + OPT_IN_OUT + SHIPPING_METHOD_CODE ,data = s)

output.forest <- randomForest(SLCSDSLA ~ SHIP_FROM + OTM_IB_SHIPPING_ROUTE_CODE + SHIPPING_METHOD_CODE  ,data = train_us)

train <- train %>% mutate(SLCSDSLA = ifelse(is.na(SLCSDSLA),'NA',SLCSDSLA))

train$SLCSDSLA=as.factor(train$SLCSDSLA)
      
      
otype <- group_by(X, SHIPMENT_AMOUNT, ORDER_TYPE)

X %>%group_by(ORDER_TYPE, sum(SHIPMENT_AMOUNT)) %>% summarise_each(funs(sum))

aggdata <-aggregate(X, by=list(X$ORDER_TYPE,X$SHIPMENT_AMOUNT), FUN=mean, na.rm=TRUE)


library(dplyr)

select(X,  ORDER_TYPE, SHIPMENT_AMOUNT, FLOW_STATUS_CODE)

filter(X, FLOW_STATUS_CODE == "SHIPPED")

ship_data <- X %>% filter(FLOW_STATUS_CODE == "SHIPPED") %>% select(ORDER_TYPE, SHIPMENT_AMOUNT, FLOW_STATUS_CODE)


shipdata_m <- X %>%  group_by(ORDER_TYPE,SHIP_FROM_ORG_ID,OPT_IN_OUT,SHIPPING_METHOD_CODE,SHIP_TO) %>%  summarize(mean_size = mean(SHIPMENT_AMOUNT, na.rm = TRUE),min_generation = min(SHIPMENT_AMOUNT),max_generation = max(SHIPMENT_AMOUNT),count = n())


as.data.frame(summary(X$ROUTE_DERIVED))--,X$MFG_DATE,X$DELIVERY_CREATED)



X$ACT_PICKTOSLC= as.numeric(X$ACTUAL_SLC_SD - X$EXPECTED_SLC_SD)
X$ACT_PICKTOSLCSLA <- ifelse(X$ACT_PICKTOSLC > 0, 'SLA_MISS', 'SLA_MET')

X$ACT_PICKTOSLCSLA = as.factor(X$ACT_PICKTOSLCSLA)

s <- x
s$DELIVERED_DATE[is.na(s$DELIVERED_DATE) ] <- Sys.time()
s$ACT_DELDATE= as.numeric(s$DELIVERED_DATE - s$EXPECTED_DELIVERY_DATE)
s$ACT_ACT_DELDATESLA <- ifelse(s$ACT_DELDATE > 0, 'SLA_MISS', 'SLA_MET')
s$ACT_ACT_DELDATESLA = as.factor(s$ACT_ACT_DELDATESLA)
summary(s$ACT_ACT_DELDATESLA)
## SLA_MET SLA_MISS 
## 47138     9459
mean(s$ACT_DELDATE/60)

----

s$ROUTE_DERIVED[is.na(s$ROUTE_DERIVED) ] <- Sys.time()
s$ACT_ROUTEDATE= as.numeric(s$CM_SSD - s$ROUTE_DERIVED)
s$ACT_ROUTEDATESLA <- ifelse(s$ACT_ROUTEDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_ROUTEDATESLA = as.factor(s$ACT_ROUTEDATESLA)
summary(s$ACT_ROUTEDATESLA)
---

s$MFG_DATE[is.na(s$MFG_DATE) ] <- Sys.time()
s$ACT_MFGDATE= as.numeric(s$CM_SSD - s$MFG_DATE)
s$ACT_MFGDATESLA <- ifelse(s$ACT_MFGDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_MFGDATESLA = as.factor(s$ACT_MFGDATESLA)
summary(s$ACT_MFGDATESLA)

---

s$PACKOUT_DATE[is.na(s$PACKOUT_DATE) ] <- Sys.time()
s$ACT_PKOUTDATE= as.numeric(s$CM_SSD - s$PACKOUT_DATE)
s$ACT_PKOUTDATESLA <- ifelse(s$ACT_PKOUTDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_PKOUTDATESLA = as.factor(s$ACT_PKOUTDATESLA)
summary(s$ACT_PKOUTDATESLA)

----


sa <- X %>% mutate(DELIVERED_DATE = ifelse(is.na(DELIVERED_DATE),Sys.time(),DELIVERED_DATE))

sa <- filter(train, SHIP_TO == "US", OPT_IN_OUT=="C")


s$DELIVERED_DATE = ifelse(s$DELIVERED_DATE = 'NA', Sys.time(), s$DELIVERED_DATE)

s$DELIVERED_DATE[is.na(s$DELIVERED_DATE) ] <- Sys.time()


output.forest <- randomForest(ACT_ACT_DELDATESLA ~ OTM_IB_SHIPPING_ROUTE_CODE + OPT_IN_OUT + SHIPMENT_PRIORITY_CODE, data = s)

# CLustering

wss <- (nrow(X)-1)*sum(apply(X,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(X, 
  	centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")
  
  
  
  
  # Production Support Categorization & Fixing Automation
  # On Time Shipment Delivery Analytics
  # Engineering Use Case - Troubleshooting Incidents - Router/ etc
  # Diff between NIA and standalone paltform
  # Cisco use cases done by Product Team
  # Check with Siva on approach followed