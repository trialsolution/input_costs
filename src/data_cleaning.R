#data cleaning routines

library(reshape2)
library(qqplot2)

# read in coco
coco  <- read.csv("data/Data2.csv", header=TRUE)

#correct header
names(coco)  <- c("Country",  "dim3", "CostItem", "1984",    "1985",   "1986",    "1987",    "1988" ,   "1989",    "1990",  "1991",    "1992",    "1993",    "1994",    "1995",
"1996",    "1997",    "1998",    "1999",    "2000",
"2001",    "2002",    "2003",    "2004",    "2005",
"2006",    "2007" )


# plant protection costs
plantp  <- subset(coco, CostItem=='PLAP' & dim3=='UVAG')
plantp <- plantp[c(-2,-3)]
mplantp  <- melt(plantp, id=1)

mplantp$variable  <- as.numeric(as.character(mplantp$variable))
names(mplantp)[2]  <- "year"


qplot(year,value,data=mplantp,geom="line",colour=Country)
#! WARNING
# in 2005 all costs are equal to 1000!

#looking at means and sd
cast(mplantp, Country~., mean)
cast(mplantp, Country~., sd)


#seed costs
seed  <- subset(coco, CostItem=='SEED' & dim3=='UVAG')
seed <- seed[c(-2,-3)]
mseed  <- melt(seed, id=1)

mseed$variable  <- as.numeric(as.character(mseed$variable))
names(mseed)[2]  <- "year"


qplot(year,value,data=mseed,geom="line",colour=Country)

#seed costs UVAB
seed  <- subset(coco, CostItem=='SEED' & dim3=='UVAB')
seed <- seed[c(-2,-3)]
mseed  <- melt(seed, id=1)

mseed$variable  <- as.numeric(as.character(mseed$variable))
names(mseed)[2]  <- "year"


qplot(year,value,data=mseed,geom="line",colour=Country)
