# --- load results of the stat. analysis
load('coco.Rdata')

# --- packages
library(ggplot2)

# --- merge data on fertilizers and other inputs
mfull  <- rbind(mc, mfert)


# --- plots for coeff. of variance
y <- cast(mfull, costitem~., mystats)

# --- correct and add labels
costitems$label  <- as.character(costitems$label)
costitems[costitems$item=='PHOF',]$label  <- '"Phosphate (P2O5) in fertilizer"' 
costitems[costitems$item=='POTF',]$label  <- '"Potassium (K2O) in fertilizer"'
costitems[costitems$item=='WATR',]$label  <- '"Water"'

y <- merge(y, costitems, by.x="costitem", by.y="item")

# --- plot sample means ordered by cost item
p  <- ggplot(y, aes(Mean,reorder(label,Mean)))
p + geom_point() + ylab(c("")) + xlab(c(""))
ggsave(filename="reports/coev_allitem.png")


# --- histogram of selected cost items
selected_costs  <- c("EFUL", "ELEC", "PLAP", "NITF")
indicator  <- mfull$costitem %in% selected_costs
selected_m  <- mfull[indicator,]

y <- merge(selected_m, costitems, by.x="costitem", by.y="item")

p <- ggplot(y, aes(x=value))
p + geom_histogram() + facet_grid(label~.) + ylab(c("")) + xlab(c(""))
ggsave(filename="reports/coev_selected.png")
