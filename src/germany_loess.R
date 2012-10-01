# LOESS (Local Polynomial Regression Fitting)

ger <- subset(capreg_uvap, country=="DE000000")

# try to do it with ddply in one go

my_loess  <- function(df){
  with(df, data.frame(
       year   = year,
       fitted = loess(value~year)$fitted,
       resid  = loess(value~year)$residuals
    ))
  
}


ger_calc  <- ddply(ger, .(row), my_loess)


# checks coefficient of variances
my_cov  <- function(df){
  with(df, data.frame(
    cov = sd(resid) / mean(fitted)
  ))
  
}

ger_cov  <- ddply(ger_calc, .(row), my_cov)


# calculate mean and covariance matrix of the residuals -> to draw a random sample later on...
ger_resid  <- data.frame(row=ger_calc$row,year=ger_calc$year,resid=ger_calc$resid)

# NITF is missing before 1991, delete these rows...
ger_resid <- subset(ger_resid, year>1990)


ger_resid.m <- melt(ger_resid, id=c("row","year"))
ger_resid.c <- dcast(ger_resid.m,year~row)

# delete the year column and convert to matrix form
ger_resid.c <- ger_resid.c[c(-1)]
cov_ger  <- cov(data.matrix(ger_resid.c))
cor_ger  <- cor(data.matrix(ger_resid.c))
mean_ger <- lapply(ger_resid.c,mean)



#generate random draws
library(MASS)


# nr. of draws
NrDraws  <- 1000
set.seed(234)

x  <- mvrnorm(n=NrDraws, rep(0,length(mean_ger)), cov_ger)
x  <- mvrnorm(n=NrDraws, as.numeric(mean_ger), cov_ger)


#compare covariances
var(x)
cov_ger

ger_calc.m  <- melt(ger_calc, id=c("row","year"))
ger_calc.means  <- dcast(ger_calc.m, row~variable, mean)


# convert the draws to relative changes compared to the mean values
for(i in 1:length(colnames(x))){
  x[,i]  <- x[,i] /
    #   means of the original input prices
    as.numeric(ger_calc.means$fitted[ger_calc.means$row==colnames(x)[i]]) * 100
}


# create a csv file with the relative changes => to be used later in the scenario file
write.table(x, file="reports/random_draw_relative_DE.csv", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)




#write out to gdx
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")

wgdx("reports/random_relative_DE.gdx",as.data.frame(x))


gdxName <- "data/coco2_output"
symName <- "DATA2"
coco_data2 <- rgdx.param(gdxName,symName,names=c("country","stage","col","row","year"))
names(coco_data2)[6] <- "variable"
