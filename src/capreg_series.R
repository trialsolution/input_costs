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
#.. the column 'v' will contain actual costs (monetary values/hectare)
costsv  <- data.frame(costsv, v=costsv$value.x*costsv$value.y)
names(costsv)[5]  <- "q"
names(costsv)[6]  <- "p"

# drop unnecessary data frames
rm(costsq, costsp)

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






