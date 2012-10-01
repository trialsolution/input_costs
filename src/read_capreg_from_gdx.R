#read Capreg series from the original gdx file
library(ggplot2)
library(reshape2)
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")


gdxName <- "gams/GAMSandR"
symName <- "CapregData"
capreg_data <- rgdx.param(gdxName,symName,names=c("country","col","row","year"))
names(capreg_data)[5] <- "variable"



#melt data
mcapreg <- melt(capreg_data, id=1:4, na.rm=TRUE)

#convert years as factors to years as numeric data type (for plotting...)
mcapreg$year <- as.numeric(as.character(mcapreg$year))

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
capreg_uvap  <- subset(mcapreg, col=="UVAP", select=c("country", "row", "year", "value"))
#..filter only 'iy'
capreg_uvap  <- capreg_uvap[capreg_uvap$row %in% iall, ]

#concentrating on EU27 countries (most data problems are with Norway & Western Balkans)
capreg_uvap  <- capreg_uvap[capreg_uvap$country %in% countries$countrycode, ]


save.image('capreg_uvap.Rdata')

# basic data qality checks

# everything is re-based on 2005=1000 index
# except EGAS, EFUL
# so different values are probably errors
suspicious  <- capreg_uvap[capreg_uvap$year==2005 & abs(capreg_uvap$value-1000)>.1,]
unique(suspicious$row)
subset(suspicious, row=="INPO")
subset(suspicious, row=="SERI")

# simple graphs
p <- ggplot(subset(capreg_uvap, row=="EGAS"), aes(year,value, color=country))
p + geom_line()
p <- ggplot(subset(capreg_uvap, row=="WATR"), aes(year,value, color=country))
p + geom_line()


