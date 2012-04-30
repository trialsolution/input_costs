library(MASS)
library(reshape2)
library(ggplot2)


# COCO data input
# read in coco
coco  <- read.csv("data/CoCoData.csv", header=TRUE)

#correct header
names(coco)  <- c("country","stage",  "dim3", "costitem", "1984",    "1985",   "1986",    "1987",    "1988" ,   "1989",    "1990",  "1991",    "1992",    "1993",    "1994",    "1995",
"1996",    "1997",    "1998",    "1999",    "2000",
"2001",    "2002",    "2003",    "2004",    "2005",
"2006",    "2007",    "2008",    "2009",    "2010")

#melt data
mcoco <- melt(coco, id=1:4, na.rm=TRUE)

names(mcoco)[5]  <- "year"

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
#..use coco1 as it has values until 2010
coco_uvap  <- subset(mcoco, stage=="COCO1" & dim3=="UVAP", select=c("country", "costitem", "year", "value"))
#..filter on the input list
indicator  <- coco_uvap$costitem %in% iall
coco_uvap  <- coco_uvap[indicator, ]

# filter on Germany
coco_uvap  <- coco_uvap[coco_uvap$country=="DE000000", ]
coco_uvap  <- coco_uvap[c(-1)]

# store the mean values to later on calculate relative changes
coco_zero  <- cast(coco_uvap, costitem~., mean)
names(coco_zero)[2] <- "means"


# we draw a correlated random sample on the residuals of a fitting trend model
# residuals are assumed to be normally distributed with zero mean
# residuals are not independent; the covariance matrix is estimated from the sample below


# de-trend time-series to get the error terms (unexplained price fluctuation)
# models for detrending: linear or log-linear

#loop over all inputs
  for(cost in unique(coco_uvap$costitem)){

  isfitted  <- FALSE
  mysubset  <- subset(coco_uvap, coco_uvap$costitem==cost)
  
  if(length(mysubset$value)<5){

# time serie is too short to fit a model    
    isfitted  <- TRUE
  }
  
  #try a linear model
  if(!isfitted){
    lm.temp  <- lm(value ~ year, data=mysubset)
    x <- summary(lm.temp)
    if(x$adj.r.squared > 0.75 & x$coefficients[2,4]<0.05) {
            coco_uvap[coco_uvap$costitem==cost,]$value  <- lm.temp$residuals      
            isfitted  <- TRUE
   }    
  }
  
  
  #if the linear model does not fit try a logarithmic transformation
  if(!isfitted){
    logm.temp  <- lm(log(value) ~ log(year), data=mysubset)
    x <- summary(logm.temp)
    if(x$adj.r.squared > 0.75 & x$coefficients[2,4]<0.05){
    
              coco_uvap[coco_uvap$costitem==cost,]$value  <- mysubset$value - exp(logm.temp$fitted.values)
              isfitted  <- TRUE
    }
  }
  
# --- if no trend model could be fitted above => subtract mean
  if(!isfitted){
    coco_uvap[coco_uvap$costitem==cost,]$value  <- mysubset$value - mean(mysubset$value)
  }
  
# --- returns with the model type and coefficient  
  }



# reorganize to get a matrix form
coco_uvap  <- cast(coco_uvap, year ~ costitem, sum)
coco_uvap  <- coco_uvap[c(-1)]

# covariance matrix => this will be used later to get the sample from the multivariate normal distribution
covDE  <- cov(data.matrix(coco_uvap))

# correlation matrix
corDE  <- cor(data.matrix(coco_uvap))

# means
meanDE  <- mean(coco_uvap)


#generate multivariate random sample
#covariance is calculated above
#make at least 1000 draws so that sample variance is close to the original
#we need the vector of means, and the covariance matrix
#! the covariance matrix needs to be positive definite
#x  <- mvrnorm(n=1000, rep(0,2), corDE)

# nr. of draws
NrDraws  <- 1000
set.seed(234)

x  <- mvrnorm(n=NrDraws, rep(0,length(meanDE)), covDE)


# to check variances on the screen
covDE
var(x)

# create a csv file with the correlated random draws
write.table(x, file="reports/random_draw.csv", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)


# convert the draws to relative changes compared to the mean values
for(i in 1:14){
x[,i]  <- x[,i] /
#   means of the original input prices
  as.numeric(coco_zero[coco_zero$costitem==colnames(x)[i],][2]) * 100
}

# create a csv file with the relative changes => to be used later in the scenario file
write.table(x, file="reports/random_draw_relative.csv", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)



# --- save workspace; to be used later by the reporting part
save.image('randomdraw.Rdata')
