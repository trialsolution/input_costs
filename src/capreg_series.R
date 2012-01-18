# capreg data input


capreg  <- read.csv("data/CapregData.csv", header=TRUE)

#correct header
names(capreg)  <- c("country",  "dim2", "costitem", "1984",    "1985",   "1986",    "1987",    "1988" ,   "1989",    "1990",  "1991",    "1992",    "1993",    "1994",    "1995",
"1996",    "1997",    "1998",    "1999",    "2000",
"2001",    "2002",    "2003",    "2004",    "2005")

#melt data
mcapreg <- melt(capreg, id=1:3, na.rm=TRUE)

#convert years as factors to years as numeric data type (for plotting...)
mcapreg$variable  <- as.numeric(as.character(mcapreg$variable))
names(mcapreg)[4]  <- "year"

#loading set definitions (cost items, agric. activities etc.)
costitems <- read.csv('data/cost_items.csv', header=FALSE)
names(costitems)  <- c("item", "label")
unitvalues <- read.csv('data/unit_values.csv', header=FALSE)
names(unitvalues)  <- c("item", "label")
activities  <- read.csv('data/activities.csv', header=FALSE)
names(activities)  <- c("acode", "label")
countries  <- read.csv('data/countries.csv', header=FALSE)
names(countries)  <- c("countrycode", "label")

#calculating costs: multiply input price with physical quantities applied

#.. a) separate quantities and price
indicator  <- mcapreg$dim2 %in% unique(activities$acode)
costsq  <- mcapreg[indicator, ]
costsp  <- subset(mcapreg, dim2 == "UVAP", select=c("country", "costitem", "year", "value"))

#.. b) create new data frame with costs
costsv  <- merge(costsq, costsp, by=c("costitem", "country", "year"))
#.. the column 'value' will contain actual costs (monetary values/hectare)
#.. values should be scaled with 0.001
costsv  <- data.frame(costsv, value=costsv$value.x*costsv$value.y*0.001)
names(costsv)[5]  <- "q"
names(costsv)[6]  <- "p"

# drop unnecessary data frames
rm(costsq, costsp)


#I. let's do a sample analysis for PLAP
#TODO: generalize it for all cost items...

#plant protection costs
plantp  <- subset(costsv, costitem=='PLAP', select=c("country", "dim2", "year", "value"))

#for maize
maiz_plap <- subset(plantp, dim2=="MAIZ")

qplot(year,value,data=maiz_plap,colour=country,geom="line") +
  opts(title="plant protection costs for maize in capreg (eur per ha)")

#some statistics (including coefficient of variance)
mystats <- function(x)(c(N=length(x), Mean=mean(x), SD=sd(x), CoV=sd(x)/mean(x)))

# plotting functions (for visualizing single time series)
mylineplot <- function(mycost,mycountry,myactivity){
  mytemp <- subset(costsv,costitem==mycost & country==mycountry & dim2==myactivity)
  p <- qplot(year,value,data=mytemp,geom="line")
  p + opts(title=paste(mycountry,".",mycost,".",myactivity,
                       "mean:" ,mean(mytemp$value),
                       "sd:"   ,sd(mytemp$value)))
}

myboxplot <- function(mycost,mycountry,myactivity){
  mytemp <- subset(costsv,costitem==mycost & country==mycountry & dim2==myactivity)
  p <- qplot(factor(dim2),value,data=mytemp,geom="boxplot")
  p + opts(title=paste(mycountry,".",mycost,".",myactivity))
                       
}


myplot2 <- function(mycost,mycountry,myactivity){
  mytemp <- subset(costsv,costitem==mycost & country==mycountry & dim2==myactivity)
  p <- ggplot(mytemp, aes(year,value)) + geom_point() + geom_line()
  p + stat_smooth(method="lm") 

}



#descriptive statistics on PLAP
plap_stats <- cast(plantp, dim2+country~.,mystats)

#..switch off scientific formatting of numbers
options(digits=2)
plap_stats$Mean <- format(plap_stats$Mean, scientific=FALSE)


#detrend with a linear model
#..function of (costitem,country,activity)
mydetrend <- function(mycostitem,mycountry,myactivity){
  mysubset  <- subset(costsv, costitem==mycostitem & country==mycountry & dim2==myactivity)
  lm.temp  <- lm(value ~ year, data=mysubset)
  x <- summary(lm.temp)
  #plotting....
  p <- qplot(year,value,data=mysubset,geom="line")
  p + geom_abline(slope=lm.temp$coefficients[2],intercept=lm.temp$coefficients[1],colour="red") +
  opts(title=paste("adj.R squared",x$adj.r.squared))

  
  #example call: mydetrend("PLAP","HU000000","SWHE")

  
}


calculate_cv <- function(mycostitem,mycountry,myactivity){
  # calculates coefficient of variance 
  # either from a detrended serie (if r-squared is above a limit)
  # or from the original serie
  mysubset  <- subset(costsv, costitem==mycostitem & country==mycountry & dim2==myactivity)
  lm.temp  <- lm(value ~ year, data=mysubset)
  x <- summary(lm.temp)
  if(x$adj.r.squared > 0.65) {
    mycv  <- sd(lm.temp$residuals)/mean(mysubset$value)

  }
  else{
    mycv  <- sd(mysubset$value)/mean(mysubset$value)
  }
  
    return(mycv)  
}

calculate_cv2 <- function(x){
# calculates coefficient of variance
# either from a detrended serie (if r-squared is above a limit)
# or from the original serie
t  <- 1:length(x)
lm.temp  <- lm(x ~ t)
modelsum <- summary(lm.temp)
if(modelsum$adj.r.squared > 0.65) {
mycv  <- sd(lm.temp$residuals)/mean(x)
}





