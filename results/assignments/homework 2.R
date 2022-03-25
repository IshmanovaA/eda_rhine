#1
library(data.table)
library(dplyr)

runoff_stations <- readRDS('./data/runoff_stations.rds')
runoff_stations <- runoff_stations[, c(1,7,8)]
runoff_st <- melt(runoff_stations, id.vars = 'sname')


runoff_stations

#2
library(data.table)
library(ggplot2)

ggplot(data = runoff_stations) +
  geom_point(aes(x = area, y = altitude))


#3
library(data.table)
library(ggplot2)

runoff_stati <- readRDS('data/runoff_stations.rds')

ggplot(data = runoff_stati, aes(x = area, y = altitude, col = size)) +
  geom_point() +
  geom_text(label = runoff_stati$sname) +
  theme_bw()

ggplot(data = runoff_stati, aes(x = lon, y = lat, col = altitude)) +
  geom_point() +
  geom_text(label = runoff_stati$sname) +
  scale_color_gradient(low = 'dark green', high = 'brown') +
  theme_bw()


#4
runoff_st <- readRDS('data/runoff_stations.rds')


ggplot(data=runoff_st, aes(x=sname, y=size)) +
  geom_bar(stat="identity", fill = "black")








