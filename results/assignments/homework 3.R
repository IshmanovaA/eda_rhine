library(data.table)
library(ggplot2)

#1
runoff_day <- readRDS('data/runoff_day.rds')

runoff_stats <- runoff_day[, .(mean_day = round(mean(value), 0),
                                  sd_day = round(sd(value), 0),
                                  min_day = round(min(value), 0),
                                  max_day = round(max(value), 0)), by = sname]

runoff_stats_tidy <- melt(runoff_stats, id.vars = 'sname', variable.name = 'stats', value.name = 'runoff')

ggplot(runoff_stats_tidy, aes(sname, runoff, shape = stats, col = stats)) + 
  geom_point(aes(col = stats, shape = stats))

#2
runoff_stats[, variation_coefficient := sd_day / mean_day, by = sname]
dt_coeff_var_skewness <- runoff_stats[, .(sname, skewness, variation_coefficient)]


#3
runoff_summary <- readRDS('./data/runoff_summary.rds')
runoff_month <- readRDS('./data/runoff_month.rds')

runoff_class_1 <- runoff_summary[, .(sname, runoff_class)]
runoff_month_1 <- runoff_month[runoff_class_1, on = 'sname']

ggplot(runoff_month_1, aes(x = factor(month), y = value, fill = runoff_class)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free')


#4
ggplot(runoff_day, aes(x = sname, y = value)) +
  geom_boxplot()


#5
col <-  c("coral3", "burlywood3", "cadetblue3")

runoff_summary[, area_class := factor('small')]
runoff_summary[area >= 10000 & area < 110000, area_class := factor('medium')]
runoff_summary[area >= 110000, area_class := factor('large')]

runoff_summary[, alt_class := factor('low')]
runoff_summary[altitude >= 50 & altitude < 350, alt_class := factor('medium')]
runoff_summary[altitude >= 350, alt_class := factor('high')]
runoff_summary

dt <- runoff_summary[, .(sname, area, alt_class)]
altitude_area <- runoff_stats[dt, on = 'sname']

ggplot(altitude_area, aes(x = mean_day, y = area, col = sname, cex = alt_class)) +
  geom_point() +
  scale_color_manual(values = colorRampPalette(col)(17)) +
  theme_bw() 










