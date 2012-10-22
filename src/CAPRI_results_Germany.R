library(ggplot2)
library(reshape2)
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")


# load random draws
gdxName <- "data/check_initialization"
symName <- "p_randomDraw"
draw <- rgdx.param(gdxName,symName,names=c("draw","region","input"))
names(draw)[4] <- "value"


# load activity levels
gdxName <- "data/myresults"
symName <- "p_inputpActLevlScens"
actlev <- rgdx.param(gdxName,symName,names=c("draw","region","activity","technology"))
# calling it 'value' makes it already "melted"
names(actlev)[5] <- "value"
save(actlev, file="actlev.RData")


# load costs per activity (SA assumptions)
symName <- "store_costs_act"
costs_act <- rgdx.param(gdxName,symName,names=c("draw","region","activity","technology"))
# calling it 'value' makes it already "melted"
names(costs_act)[5] <- "value"
save(costs_act, file="costs_act.RData")


# analyze the draws...
draw_cov <- dcast(draw, region+input~., function(x){sd(x)})
names(draw_cov)[3]  <- "sd"
save(draw_cov, file="draw_cov.RData")


# as the means are close to zero (remember they assumed to be normal with mean zero) we use the standard deviation instead of c.o.v.
p <- ggplot(subset(draw_cov, region=="DE000000"),aes(sd,reorder(input,sd)))
p + geom_point()




# results show very little variance in activity levels. here an example for region DE230000
qplot(activity, value, data=subset(actlev, region=="DE230000" & technology=="T"), geom="boxplot")
qplot(activity, value, data=subset(actlev, region=="DE230000" & technology=="T" & activity=="SWHE"), geom="boxplot")

# example for soft wheat
swhe  <- subset(actlev, activity=="SWHE")
qplot(value, data=subset(swhe, region!="DE000000" & technology=="T"), geom="density", color=region)
qplot(value, data=subset(actlev, region!="DE000000" & technology=="T" & activity=="SWHE"), geom="density", color=region)

# the distribution for T1 technology is usually flatter, see e.g. region DE230000
qplot(value, data=subset(swhe, region=="DE230000" & technology!="T"), geom="density", color=technology)


# some descriptive stats for the activity levels
# coeff of variation
mlevl  <- melt(actlev, id=1:4)

# coeff of variation
levl_cov <- dcast(mlevl, region+activity+technology~., function(x){sd(x)/mean(x)})
names(levl_cov)[4]  <- "cov"
save(levl_cov, file="levl_cov.RData")

# ordered list of cov's in a selected region
p <- ggplot(subset(levl_cov, technology=="T" & region=="DE400000"),aes(cov,reorder(activity,cov)))
p + geom_point()

# POUR shows big variance!!! (why?)
# check how activity levels behave
qplot(activity, value, data=subset(actlev, region=="DE400000" & technology=="T" & activity=="POUF"), geom="boxplot")




#scaling activity levels. This takes some time, be patient...
names(actlev)[5] <- "value"
scaled_actlev <-  ddply(actlev,.(region,activity,technology), transform, value=scale(value))
save(scaled_actlev, file="scaled_actlev.RData")



# z scores for soft wheat, comparison over the regions
qplot(value, data=subset(scaled_actlev, region!="DE000000" & technology=="T" & activity=="SWHE"), geom="density", color=region)
qplot(value, data=subset(scaled_actlev, region=="DE230000" & technology=="T"), geom="density", color=activity)





# --- costs per activity
qplot(activity, value, data=subset(costs_act, region=="DE230000" & technology=="T"), geom="boxplot")
qplot(activity, value, data=subset(costs_act, region=="DE230000" & technology=="T" & activity=="SWHE"), geom="boxplot")



# some basic descriptive stats.
mcosts_act  <- melt(costs_act, id=1:4)



# coeff of variation
cost_cov <- dcast(mcosts_act, region+activity+technology~., function(x){sd(x)/mean(x)})
names(cost_cov)[4]  <- "cov"
save(cost_cov, file="cost_cov.RData")

# ordered cov's in all regions
p <- ggplot(cost_cov, aes(cov,reorder(activity,cov)))
p+geom_point()


# ordered list of cov's in a selected region
p <- ggplot(subset(cost_cov, technology=="T" & region=="DED00000"),aes(cov,reorder(activity,cov)))
p + geom_point()



# POUR shows big variance!!! (why?)
# check how activity levels behave
qplot(activity, value, data=subset(actlev, region=="DE400000" & technology=="T" & activity=="POUF"), geom="boxplot")




# standard deviation
cost_var <- dcast(mcosts_act, region+activity+technology~., function(x){sd(x)})

# mean
cost_mean <- dcast(mcosts_act, region+activity+technology~., mean)

# mean minus median...
cost_mean2 <- dcast(mcosts_act, region+activity+technology~., function(x){mean(x)-median(x)})





#
# --- marginal gross value added
#
# load costs per activity (SA assumptions)
gdxName <- "data/myresults"
symName <- "store_mgva"
mgva <- rgdx.param(gdxName,symName,names=c("draw","region","activity"))
# calling it 'value' makes it already "melted"
names(mgva)[4] <- "value"
#save(mgva, file="mgva.RData")


#
# --- COVs for MGVA
#
mgva_cov <- dcast(mgva, region+activity~., function(x){sd(x)/abs(mean(x))})
names(mgva_cov)[3]  <- "cov"
save(mgva_cov, file="mgva_cov.RData")

# ordered cov's in all regions
p <- ggplot(mgva_cov, aes(cov,reorder(activity,cov)))
p+geom_point()


# aggregated cov's for Germany
p <- ggplot(subset(mgva_cov, region=="DE000000"), aes(cov,reorder(activity,cov)))
p+geom_point()

# cov's for all regions
p <- ggplot(subset(mgva_cov, region!="DE000000"), aes(cov,reorder(activity,cov)))
p+geom_point()+xlim(0,0.3)
