#3 assignment
Temperatures = c(3, 6, 10, 14)
Weights = c(1, 0.8, 1.2, 1)

library(data.table)

aa = function(x, y) {
  x * y
  }

results <- aa(Temperatures, Weights)




#4 assignment
runoff_pc <- data.frame (time = as.Date(1:90, origin = '2021/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
library(data.table)


runoff_dt <- data.table(runoff_pc)
select(runoff_pc, c(1:3))
runoff_pc

runoff_dt[, month := month(time)]

runoff_dt[, month_mean := mean(value), by = month]

runoff_per_change <- runoff_dt[, .(month, month_mean)] 
runoff_per_change
unique_change <- unique(runoff_per_change)


percentage_change <- function(V1, V2){
  result_percentage <- ((V2-V1)/V1)*100
}

jan_percentage <- percentage_change(unique_change[1,month_mean], unique_change [2, month_mean])
jan_percentage

feb_percentage <- percentage_change(unique_change[2,month_mean], unique_change [3, month_mean])
feb_percentage



unique_change[, percentage_change := c(0, jan_percentage, feb_percentage)]
unique_change

