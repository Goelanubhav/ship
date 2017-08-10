

s$ROUTE_DERIVED[is.na(s$ROUTE_DERIVED) ] <- Sys.time()
s$ACT_ROUTEDATE= round(as.numeric(s$CM_SSD - s$ROUTE_DERIVED),digits = 0)
s$ACT_ROUTEDATESLA <- ifelse(s$ACT_ROUTEDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_ROUTEDATESLA = as.factor(s$ACT_ROUTEDATESLA)
summary(s$ACT_ROUTEDATESLA)


s$MFG_DATE[is.na(s$MFG_DATE) ] <- Sys.time()
s$ACT_MFGDATE= round(as.numeric(s$CM_SSD - s$MFG_DATE),digits = 0)
s$ACT_MFGDATESLA <- ifelse(s$ACT_MFGDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_MFGDATESLA = as.factor(s$ACT_MFGDATESLA)
summary(s$ACT_MFGDATESLA)


s$PACKOUT_DATE[is.na(s$PACKOUT_DATE) ] <- Sys.time()
s$ACT_PKOUTDATE= round(as.numeric(s$CM_SSD - s$PACKOUT_DATE),digits = 0)
s$ACT_PKOUTDATESLA <- ifelse(s$ACT_PKOUTDATE > 0, 'SLA_MET', 'SLA_MISS')
s$ACT_PKOUTDATESLA = as.factor(s$ACT_PKOUTDATESLA)
summary(s$ACT_PKOUTDATESLA)


s$DELIVERY_CREATED[is.na(s$DELIVERY_CREATED) ] <- Sys.time()
s$ACT_DLDATE= round(as.numeric(s$EXPECTED_SLC_SD - s$DELIVERY_CREATED),digits = 0)
s$ACT_DLDATESLA <- ifelse(s$ACT_DLDATE < 0, 'SLA_MISS', 'SLA_MET')
s$ACT_DLDATESLA = as.factor(s$ACT_DLDATESLA)
summary(s$ACT_DLDATESLA)


s$PICK_RELEASED[is.na(s$PICK_RELEASED) ] <- Sys.time()
s$EXPECTED_SLC_SD[is.na(s$EXPECTED_SLC_SD) ] <- Sys.time()
s$ACT_PRDATE= round(as.numeric(s$EXPECTED_SLC_SD - s$PICK_RELEASED),digits = 0)
s$ACT_PRDATESLA <- ifelse(s$ACT_PRDATE < 0, 'SLA_MISS', 'SLA_MET')
s$ACT_PRDATESLA = as.factor(s$ACT_PRDATESLA)
summary(s$ACT_PRDATESLA)



s$ACTUAL_SLC_SD[is.na(s$ACTUAL_SLC_SD) ] <- Sys.time()
s$ACT_SDDATE= round(as.numeric(s$EXPECTED_SLC_SD - s$ACTUAL_SLC_SD),digits = 0)
s$ACT_ACT_SSDATESLA <- ifelse(s$ACT_SDDATE < 0, 'SLA_MISS', 'SLA_MET')
s$ACT_ACT_SSDATESLA = as.factor(s$ACT_ACT_SSDATESLA)
summary(s$ACT_ACT_SSDATESLA)


s$DELIVERED_DATE[is.na(s$DELIVERED_DATE) ] <- Sys.time()
s$ACT_DELDATE= round(as.numeric(s$DELIVERED_DATE - s$EXPECTED_DELIVERY_DATE),digits = 0)
s$ACT_ACT_DELDATESLA <- ifelse(s$ACT_DELDATE > 0, 'SLA_MISS', 'SLA_MET')
s$ACT_ACT_DELDATESLA = as.factor(s$ACT_ACT_DELDATESLA)
summary(s$ACT_ACT_DELDATESLA)


s <- x

summary(s$SHIPPING_METHOD_CODE)

X_sub = X_sub[!(as.numeric(X_sub$SHIPPING_METHOD_CODE) %in% which(table(X_sub$SHIPPING_METHOD_CODE)<5)),]