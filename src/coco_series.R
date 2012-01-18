#Coco data analysis

library(reshape2)
library(qqplot2)

# read in coco
coco  <- read.csv("data/Data2.csv", header=TRUE)

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


#concentrating on UVAP
#..use coco1 as it has values until 2010
coco_uvap  <- subset(mcoco, stage=="COCO1" & dim3=="UVAP", select=c("country", "costitem", "year", "value"))
#..filter only 'iy'
indicator  <- coco_uvap$costitem %in% iy
coco_uvap  <- coco_uvap[indicator, ]


#data checking: prices are rebased on year2005=1000
#=> so declining real prices are suspicious (but possible)
suspicious  <- coco_uvap[coco_uvap$year==2010 & coco_uvap$value<1000,]
suspicious



calculate_cv <- function(mydata,mycostitem,mycountry){
  # calculates coefficient of variance 
  # either from a detrended serie (if r-squared is above a limit)
  # or from the original serie
  #example: calculate_cv(coco_uvap,"PLAP","UK000000")
  
  mysubset  <- subset(mydata, costitem==mycostitem & country==mycountry)
  lm.temp  <- lm(value ~ year, data=mydata)
  x <- summary(lm.temp)
  if(x$adj.r.squared > 0.65) {
    mycv  <- sd(lm.temp$residuals)/mean(mysubset$value)

  }
  else{
    mycv  <- sd(mysubset$value)/mean(mysubset$value)
  }
  
    return(mycv)  
}


#creates a data frame that will contain the coev.
container  <- cast(coco_uvap, country+costitem~.)
names(container)[3]  <- "coev"

#loop over all countries and cost items
for(cou in unique(coco_uvap$country)){
  for(cost in unique(coco_uvap$costitem)){
    container[country==cou & costitem==cost, ]$coev  <- calculate_cv(coco_uvap, cost, cou)
  }
}
