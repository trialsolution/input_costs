#read CoCo series from the original gdx file
library(ggplot2)
library(reshape2)
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")


gdxName <- "data/coco2_output"
symName <- "DATA2"
coco_data2 <- rgdx.param(gdxName,symName,names=c("country","stage","col","row","year"))
names(coco_data2)[6] <- "variable"



#melt data
mcoco <- melt(coco_data2, id=1:5, na.rm=TRUE)

#convert years as factors to years as numeric data type (for plotting...)
mcoco$year <- as.numeric(as.character(mcoco$year))

#loading set definitions (cost items, agric. activities etc.)
costitems <- read.csv('data/cost_items.csv', header=FALSE)
names(costitems)  <- c("item", "label")
unitvalues <- read.csv('data/unit_values.csv', header=FALSE)
names(unitvalues)  <- c("item", "label")
activities  <- read.csv('data/activities.csv', header=FALSE)
names(activities)  <- c("acode", "label")
countries  <- read.csv('data/countries.csv', header=FALSE)
names(countries)  <- c("countrycode", "label")

#yield dependent inputs = set IY in CAPRI
iy <- c("SEED", "PLAP", "REPM", "REPB", "ELEC", "EGAS", "EFUL", "ELUB", "WATR", "INPO", "SERI", "IPHA")
#all inputs used in this analysis
iall  <- c("SEED", "PLAP", "REPM", "REPB", "ELEC", "EGAS", "EFUL", "ELUB", 
           "WATR", "INPO", "SERI", "IPHA", "NITF", "PHOF", "POTF")



#concentrating on UVAP
#..use stage coco1 as it has values until 2010
coco_uvap  <- subset(mcoco, stage=="COCO1" & col=="UVAP", select=c("country", "row", "year", "value"))
#..filter only 'iy'
coco_uvap  <- coco_uvap[coco_uvap$row %in% iall, ]

#concentrating on EU27 countries (most data problems are with Norway & Western Balkans)
coco_uvap  <- coco_uvap[coco_uvap$country %in% countries$countrycode, ]


save.image('coco_uvap.Rdata')

# basic data qality checks

# everything is re-based on 2005=1000 index
# except EGAS, EFUL
# so different values are probably errors
suspicious  <- coco_uvap[coco_uvap$year==2005 & abs(coco_uvap$value-1000)>.1,]
unique(suspicious$row)
subset(suspicious, row=="INPO")
subset(suspicious, row=="SERI")

# simple graphs
p <- ggplot(subset(coco_uvap, row=="EGAS"), aes(year,value, color=country))
p + geom_line()
p <- ggplot(subset(coco_uvap, row=="WATR"), aes(year,value, color=country))
p + geom_line()


