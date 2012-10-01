# LOESS (Local Polynomial Regression Fitting)


# try to do it with ddply in one go

my_loess  <- function(df){
  with(df, data.frame(
       year   = year,
       fitted = loess(value~year)$fitted,
       resid  = loess(value~year)$residuals
    ))
  
}


capreg_uvap.calc  <- ddply(capreg_uvap, .(country, row), my_loess)


# checks coefficient of variances
my_cov  <- function(df){
  with(df, data.frame(
    cov = sd(resid) / mean(fitted)
  ))
  
}

capreg_uvap.cov  <- ddply(capreg_uvap.calc, .(country, row), my_cov)


# create a csv file with the cov estimates
write.table(capreg_uvap.cov, file="reports/cov_AllCountry.csv", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)



# PLOTTING
library(ggplot2)

# average cov (using the median to get a more robust estimator than the mean)
avg.cov <- dcast(mcov, row ~ ., median)
names(avg.cov)[2] <- "median"
p <- ggplot(avg.cov, aes(median, reorder(row,median)))
p + geom_point() + ylab(c("")) + xlab(c(""))

# min/max of cov estimates
dcast(mcov, row ~ ., min)
dcast(mcov, row ~ ., max)


# histogram of selected cov estimates in the member states
# --- histogram of selected cost items
selected_inputs  <- c("EFUL", "PLAP", "NITF")
mcov_selected <- mcov[mcov$row %in% selected_inputs,]

# nicer headers
mcov_selected$row <- as.character(mcov_selected$row)
mcov_selected[mcov_selected$row=="EFUL",]$row  <- '"Energy and Fuel"'
mcov_selected[mcov_selected$row=="PLAP",]$row  <- '"Plant Protection"'
mcov_selected[mcov_selected$row=="NITF",]$row  <- '"Nitrogen in Fertilizer"'


p <- ggplot(mcov_selected, aes(x=value, ..density..))
p + geom_histogram(binwidth=.02) + facet_grid(row~.) +  xlab(c("")) + xlim(0,0.4)













#write out to gdx
library(gdxrrw)

# path to the gams installation
igdx("n:/soft/gams/gams23.8_64")

wgdx("reports/random_relative_DE.gdx",as.data.frame(x))


gdxName <- "data/coco2_output"
symName <- "DATA2"
coco_data2 <- rgdx.param(gdxName,symName,names=c("country","stage","col","row","year"))
names(coco_data2)[6] <- "variable"

