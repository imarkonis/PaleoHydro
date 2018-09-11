
test <- mhm[year(DTM) == 1921 & PT_ID == 1073]
test[DTM <= start + months(4), sum(value), variable]

event_2018 <- mhm[year(DTM) == 2018 & PT_ID == 1073]
event_2018[DTM <= start + months(4), sum(value), variable]

test <- mhm[year(DTM) == 1921 & PT_ID == 1072]
test[DTM <= start + months(4), sum(value), variable]

event_2018 <- mhm[year(DTM) == 2018 & PT_ID == 1072]
event_2018[DTM <= start + months(4), sum(value), variable]
