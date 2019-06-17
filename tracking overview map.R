###Set working directory
setwd("~/Google Drive/Research Projects/Egyptian Vulture/Frontiers in Ecology & Evolution Paper/Latest/Data/EVmigration_data/")
library(ggplot2)
library(adehabitatLT)
library(raster)
library(zoom)
library(cowplot)
library(ggpubr)
library(grid)
library(ggplotify)
library(gridExtra)

#data
d = read.csv("EV-CompletedMigrations.csv")
summary(d)
d$population <- rep(NA, nrow(d))
summary(d$study)
d[d$study == "buechley-mideast", ][, "population"] <- "Caucasus"
d[d$study == "douro-spain", ][, "population"] <- "Western Europe"
d[d$study == "efrat-israel", ][, "population"] <- "Middle East"
d[d$study == "grefa-spain", ][, "population"] <- "Western Europe"
d[d$study == "karyakin-russia", ][, "population"] <- "Caucasus"
d[d$study == "kobierzycki-france", ][, "population"] <- "Western Europe"
d[d$study == "life.rupis-spain", ][, "population"] <- "Western Europe"
d[d$study == "migra-spain", ][, "population"] <- "Western Europe"
d[d$study == "oppel-balkans", ][, "population"] <- "Balkans"
d[d$study == "terra.natura-spain", ][, "population"] <- "Western Europe"
head(d)
d$population.season = paste(d$population,d$season)
head(d)


###########
#ggplot map
###########
summary(d$population)
d$population <- factor(d$population, levels=c("Balkans", "Caucasus", "Middle East", "Western Europe"))
d$id = factor(d$id, levels=unique(d$id))
#
map.plot = ggplot(d, aes(long, lat)) + annotation_map(map_data("world"), fill = 'grey') + coord_quickmap()  + theme_bw() 
map.plot = map.plot + geom_path(aes(long, lat, group = id, color = population)) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + scale_color_manual(values=c( "green4","blue4",  "magenta4", "red2" ))
map.plot
#
map.plot = ggplot(d, aes(long, lat)) + annotation_map(map_data("world"), fill = 'grey') + coord_quickmap()  + theme_bw() 
map.plot = map.plot + geom_path(aes(long, lat, group = id)) + labs(x = "longitude", y = "latitude")
map.plot
#
tiff("ev.tracks.tiff", units="cm", width=20, height=15, res=300)
map.plot = ggplot(d, aes(long, lat)) + annotation_map(map_data("world"), fill = 'grey') + coord_quickmap()  + theme_bw() 
map.plot = map.plot + geom_path(aes(long, lat, group = id, color = season)) + labs(x = "longitude", y = "latitude")
map.plot = map.plot + scale_color_manual(values=c( "green4","blue4"))
map.plot
dev.off()
