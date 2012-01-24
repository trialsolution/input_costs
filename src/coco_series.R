#Coco data analysis

library(reshape2)
library(ggplot2)

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
  isfitted  <- FALSE
  mysubset  <- subset(mydata, mydata$costitem==mycostitem & mydata$country==mycountry)
  
  if(length(mysubset$value)<5){
    mycv <- NA
    return(mycv)
    break
  }
  
  #try a linear model
  lm.temp  <- lm(value ~ year, data=mydata)
  x <- summary(lm.temp)
  if(x$adj.r.squared > 0.75 & x$coefficients[2,4]<0.05) {
    mycv  <- sd(lm.temp$residuals)/mean(mysubset$value)
    isfitted  <- TRUE
  }
  
  #if the linear model does not fit try a logarithmic transformation
  logm.temp  <- lm(log(value) ~ log(year), data=mydata)
  x <- summary(logm.temp)
  if(x$adj.r.squared > 0.75 & isfitted=="FALSE" & x$coefficients[2,4]<0.05){
    
    mycv  <- sd(exp(logm.temp$fitted.values) - mysubset$value)    /mean(mysubset$value)
    isfitted  <- TRUE
    
  }
  
  if(isfitted=="FALSE"){
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
    container[container$country==cou & container$costitem==cost, ]$coev  <- calculate_cv(coco_uvap, cost, cou)
  }
}


#investigating CoV results...
mc <- container
names(mc)[3] <- "value"
cast(mc, costitem~., mean)
cast(mc, costitem~., sd)
mystats <- function(x)(c(N=length(x), Mean=mean(x), SD=sd(x), Min=min(x), Max=max(x)))
cast(mc, costitem~., mystats)

#looking at the cov distributions among countries
p <- ggplot(mc, aes(x=value))
p + geom_histogram() + facet_grid(costitem~.)

#check possible outliers
#.. order cov's to see the countries with high values
mc_egas  <- subset(mc, costitem=="EGAS")
mc_egas[order(mc_egas$value),]

mc_elub  <- subset(mc, costitem=="ELUB")
mc_elub[order(mc_elub$value),]


#.. full time-series
p <- ggplot(subset(coco_uvap, country=="LT000000" & costitem=="EGAS"), aes(year,value))
p + geom_point() + geom_line() + stat_smooth()

p <- ggplot(subset(coco_uvap, country=="UK000000" & costitem=="ELUB"), aes(year,value))
p + geom_point() + geom_line() + stat_smooth()

p <- ggplot(subset(coco_uvap, country=="PT000000" & costitem=="ELUB"), aes(year,value))
p + geom_point() + geom_line() + stat_smooth()





# --- PART II. 
# --- fertilizer costs

# --- A) start with the general category "FERT"
# it has values for EAAP (producer price) and PRII (price index: 100=2005)
#..use coco1 as it has values until 2010
coco_fert_prii  <- subset(mcoco, stage=="COCO1" & dim3=="PRII" & costitem=="FERT", select=c("country", "costitem", "year", "value"))


# --- calculate coeff. of variance for the price index PRII

#creates a data frame that will contain the coev.
container_prii  <- cast(coco_fert_prii, country+costitem~.)
names(container_prii)[3]  <- "coev"

#loop over all countries
for(cou in unique(coco_fert_prii$country)){
    container_prii[container_prii$country==cou & container_prii$costitem=="FERT", ]$coev  <- calculate_cv(coco_fert_prii, "FERT", cou)
}


#investigating CoV results...
mprii <- container_prii
names(mprii)[3] <- "value"
#remove NA's  
mprii  <- mprii[!is.na(mprii$value),]
cast(mprii, costitem~., mystats)

#looking at the cov distributions among countries
p <- ggplot(mprii, aes(x=value))
p + geom_histogram() + facet_grid(costitem~.)



# --- B) let's continue with the detailed fertilizer costs
fertilizers <- c("NITF", "POTF", "PHOF")

#..use coco1 as it has values until 2010
coco_uvap  <- subset(mcoco, stage=="COCO1" & dim3=="UVAP", select=c("country", "costitem", "year", "value"))

# --- use UVAP unit value prices
indicator  <- coco_uvap$costitem %in% fertilizers
coco_uvap_fert  <- coco_uvap[indicator, ]


# --- calculate coeffs. of variance 
container_fert_uvap  <- cast(coco_uvap_fert, country+costitem~.)
names(container_fert_uvap)[3]  <- "coev"

#loop over all countries and cost items
for(cou in unique(container_fert_uvap$country)){
  for(cost in fertilizers){
    if(length(container_fert_uvap[container_fert_uvap$country==cou & container_fert_uvap$costitem==cost, ]$coev))
    container_fert_uvap[container_fert_uvap$country==cou & container_fert_uvap$costitem==cost, ]$coev  <- calculate_cv(coco_uvap_fert, cost, cou)
  }
}

#investigating CoV results...
mfert <- container_fert_uvap
names(mfert)[3] <- "value"
#remove NA's  
mfert  <- mfert[!is.na(mfert$value),]
cast(mfert, costitem~., mystats)

#looking at the cov distributions among countries
p <- ggplot(mfert, aes(x=value))
p + geom_histogram() + facet_grid(costitem~.)


#check possible outliers
#.. order cov's to see the countries with high values
mf_nitf  <- subset(mfert, costitem=="NITF")
mf_nitf[order(mf_nitf$value),]


#.. full time-series for specific countries
p <- ggplot(subset(coco_uvap, country=="SI000000" & costitem=="NITF"), aes(year,value))
p + geom_point() + geom_line() + stat_smooth()



# --- write out results on CoV
write.csv(mc, file="reports/inputc_cov.csv", row.names=FALSE)
write.csv(mprii, file="reports/prii_cov.csv", row.names=FALSE)
write.csv(mfert, file="reports/fertilizers_cov.csv", row.names=FALSE)
