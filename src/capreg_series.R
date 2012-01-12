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


#plant protection costs
plantp_capreg  <- subset(mcapreg, costitem=='PLAP' & dim2=='UVAG')

qplot(year,value,data=plantp_capreg,geom="line",colour=country) + 
  opts(title="plant protection costs PLAP.UVAG in capreg time series")

#some statistics (including coefficient of variance)
mystats <- function(x)(c(N=length(x), Mean=mean(x), SD=sd(x), CoV=sd(x)/mean(x)))

cast(plantp_capreg, country~.,mystats)

#detrend with a linear model
#U.K. example
ukplantp  <- subset(plantp_capreg, country=='UK000000')
lm.uk  <- lm(value ~ year, data=ukplantp)

#graphical representation
p  <- qplot(year,value, data=ukplantp, geom="line")
p+geom_abline(intercept=lm.uk$coefficients[1],slope=lm.uk$coefficients[2])






