#1 In retrospect, DOMA is not a representative station. It differs from other stations in its location (for ex.: altitude)

#2 
library(data.table)
library(ggplot2)

precipitation_day <- readRDS('data/raw/precip_day.rds')
colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

precipitation_day[, month := month(date)]
precipitation_day[month == 12 | month == 1 | month == 2, season := 'winter']
precipitation_day[month == 3 | month == 4 | month == 5, season := 'spring']
precipipitation_day[month == 6 | month == 7 | month == 8, season := 'summer']
precipitation_day[month == 9 | month == 10 | month == 11, season := 'autumn']
precipitation_day[, season := factor(season, levels = c('winter', 'spring', 'summer', 'autumn'))]

precipitation_day[, year := year(date)]
precipitation_winter <- precipitation_day[season == 'winter', 
                            .(value = sum(value)), by = year]
precipitation_summer <- precipitation_day[season == 'summer', 
                            .(value = sum(value)), by = year]

year_thres <- 1997
to_plot <- rbind(cbind(precipitation_winter, season = factor('winter')), 
                 cbind(precipitation_summer, season = factor('summer'))) 

to_plot[year < year_thres, period := factor('pre_1997')]
to_plot[year >= year_thres, period := factor('aft_1997')]
to_plot[year < year_thres, period := factor('pre_1997')]
to_plot[year >= year_thres, period := factor('aft_1997')]

ggplot(to_plot[year >= 1979], aes(season, value, fill = period)) +
  geom_boxplot() +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Precipitation") +
  theme_bw()

ggplot(to_plot[season == 'summer' & year >= 1979], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation on Summer") +
  theme_bw()

ggplot(to_plot[season == 'winter' & year >= 1979], aes(x = year, y = value)) +
  geom_line(col = colset_4[3])+
  geom_point(col = colset_4[3])+
  geom_smooth(method = 'lm', formula = y~x, se = 0, col = colset_4[1]) +
  geom_smooth(method = 'loess', formula = y~x, se = 0, col = colset_4[4]) +
  scale_color_manual(values = colset_4[c(1, 2, 4)]) +
  xlab(label = "Year") +
  ylab(label = "Precipitation on Winter") +
  theme_bw()


#3 yes, there are some changes in the runoff, and I guess that's because of changes of climate (for ex. global warming)

#4 we need more data about human activity in that area (for ex. factories, dams, polltuion) and explore more meteorological, climatological data


