library(ggplot2)
library(reshape2)
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")


gdxName <- "data/myresults"
symName <- "p_inputpActLevlScens"
cresults <- rgdx.param(gdxName,symName,names=c("draw","region","product","technology"))
names(cresults)[5] <- "variable"


# results show very little variance in activity levels. here an example for region DE230000
qplot(product, variable, data=subset(cresults, region=="DE230000" & technology=="T"), geom="boxplot")
qplot(product, variable, data=subset(cresults, region=="DE230000" & technology=="T" & product=="SWHE"), geom="boxplot")

# example for soft wheat
swhe  <- subset(cresults, product=="SWHE")
qplot(variable, data=subset(swhe, region!="DE000000" & technology=="T"), geom="density", color=region)

# the distribution for T1 technology is usually flatter, see e.g. region DE230000
qplot(variable, data=subset(swhe, region=="DE230000" & technology!="T"), geom="density", color=technology)
