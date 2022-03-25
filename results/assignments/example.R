runoff <- 130 #m3/s 
runoff

runoff_ts <- data.frame(time = as.Date(1:90, origin = '2020/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
head(runoff_ts)
str(runoff_ts)


#data.table
library(data.table)

runoff_dt <- data.table(runoff_ts)
str(runoff_ts)

summary(runoff_dt)

runoff_dt[value > 130]

runoff_dt[value > 130, mean(value)]

runoff_dt[value > 130, mean(value), by = month(time)]



#new column
runoff_dt[, mon := month(time)]
runoff_dt

runoff_dt[, mon_mean := mean(value), by = mon]
runoff_dt


runoff_month <- runoff_dt[, .(mon, mon_mean)] 

runoff_month

unique(runoff_month)



saveRDS(runoff_dt, file = './data/dt_example.rds')














