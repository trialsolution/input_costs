# heatmap of coeffs of variation by costitem and country

library(ggplot2)

mfull  <- rbind(mfert,mc)
mc2  <- cast(mfull, country ~ costitem)

# calculate quantiles

mc2$EFUL <- with(mc2, cut(EFUL, breaks=quantile(EFUL, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))

mybreaks  <- quantile(mc2$EGAS, probs=seq(0,1, by=0.1), include.lowest=TRUE) 
#break points should be unique
mybreaks[1]  <- mybreaks[1]-0.000001
mc2$EGAS <- with(mc2, cut(EGAS, breaks=mybreaks,labels=1:10))

mc2$ELEC <- with(mc2, cut(ELEC, breaks=quantile(ELEC, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))

mybreaks  <- quantile(mc2$ELUB, probs=seq(0,1, by=0.1), include.lowest=TRUE) 
mybreaks[3]  <- mybreaks[3]-0.000005
mybreaks[4]  <- mybreaks[4]-0.000002
mybreaks[5]  <- mybreaks[5]+0.000002

mc2$ELUB <- with(mc2, cut(ELUB, breaks=mybreaks,labels=1:10))


mc2$INPO <- with(mc2, cut(INPO, breaks=quantile(INPO, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$IPHA <- with(mc2, cut(IPHA, breaks=quantile(IPHA, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$PLAP <- with(mc2, cut(PLAP, breaks=quantile(PLAP, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$REPB <- with(mc2, cut(REPB, breaks=quantile(REPB, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$REPM <- with(mc2, cut(REPM, breaks=quantile(REPM, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$SEED <- with(mc2, cut(SEED, breaks=quantile(SEED, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$SERI <- with(mc2, cut(SERI, breaks=quantile(SERI, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))
mc2$WATR <- with(mc2, cut(WATR, breaks=quantile(WATR, probs=seq(0,1, by=0.1), include.lowest=TRUE),labels=1:10))


# fertilizers
mc2$NITF <- with(mc2, cut(NITF, breaks=quantile(NITF, probs=seq(0,1, by=0.1), include.lowest=TRUE, na.rm=TRUE),labels=1:10))
mc2$PHOF <- with(mc2, cut(PHOF, breaks=quantile(PHOF, probs=seq(0,1, by=0.1), include.lowest=TRUE, na.rm=TRUE),labels=1:10))
mc2$POTF <- with(mc2, cut(POTF, breaks=quantile(POTF, probs=seq(0,1, by=0.1), include.lowest=TRUE, na.rm=TRUE),labels=1:10))

mc2_short  <- mc2
row.names(mc2_short)  <- mc2_short$country
mc2_short <- mc2_short[,2:16]
mc2_matrix <- data.matrix(mc2_short)
myheatmap  <- heatmap(mc2_matrix,Rowv=NA,Colv=NA, col=cm.colors(256), scale="column")
myheatmap  <- heatmap(mc2_matrix,Rowv=NA,Colv=NA, col=heat.colors(256), scale="column")



