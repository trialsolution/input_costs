library(plyr)

ger <- subset(coco_uvap, country=="DE000000")

ddply(ger,.(row),subset,value==min(value))


ger_seed  <- subset(ger,row=="SEED")

#linear trend model
lm_seed  <- lm(value~year,data=ger_seed)
summary(lm_seed)

p <- ggplot(ger_seed,aes(year,value))
p+geom_point()+stat_smooth(method="lm")

#other simple trend models
loglog_seed <- lm(log(value)~log(year),data=ger_seed)
summary(loglog_seed)
exp_seed <- lm(log(value)~year,data=ger_seed)
summary(exp_seed)

#linear trend with dummy
ger_seed$dummy  <- (ger$year > 2005) * 1
lm_seed  <- lm(value~year+dummy, data=ger_seed)
summary(lm_seed)
ger_seed  <- subset(ger,row=="SEED")qplot(ger_seed$year,lm_seed$residuals,geom="line")

#test the normality of the residuals
ks.test(lm_seed$residuals,"pnorm")


#coefficient of variation ( sd / mean)

#cov of the original time serie
ger_seed$cov_orig  <- sd(ger_seed$value) / mean(ger_seed$value)
#we should reduce it by identifying a time trend
ger_seed$cov_detrended_dummy  <- sd(lm_seed$residuals) / mean(lm_seed$fitted.values)

# !!! Important consequence: time-trend matters, i.e. the calculated c.o.v. depends highly on how well the trend fits

lm_seed  <- lm(value~year,data=ger_seed)
ger_seed$cov_detrended_linear  <- sd(lm_seed$residuals) / mean(lm_seed$fitted.values)
ger_seed$cov_detrended_loglog  <- sd(loglog_seed$residuals) / mean(loglog_seed$fitted.values)
ger_seed$cov_detrended_exp     <- sd(exp_seed$residuals) / mean(exp_seed$fitted.values)




# 2 WATR
ger_watr  <- subset(ger,row=="WATR")
p <- ggplot(ger_watr,aes(year,value))
p+geom_line()+stat_smooth(method="lm")

#linear trend model
lm_watr  <- lm(value~year,data=ger_watr)
summary(lm_watr)

#other simple trend models
loglog_watr <- lm(log(value)~log(year),data=ger_watr)
summary(loglog_watr)
exp_watr <- lm(log(value)~year,data=ger_watr)
summary(exp_watr)
second_watr <- lm(value~poly(year,2),data=ger_watr)
summary(second_watr)


ger_watr.loess <- loess(value~year,data=ger_watr)
ger_watr.loess$residuals
summary(ger_watr.loess)
ks.test(ger_watr.loess$residuals,"pnorm")
sd(ger_watr.loess$residuals) / mean(ger_watr.loess$fitted)
sd(ger_watr$value) / mean(ger_watr$value)
