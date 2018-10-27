tiff("maxcumul.tif",width=15.5,height=11.5,res=600,units="cm",compression="lzw")

rat<-6

fitborder<-92.24

layout(
matrix(c(rep(1,rat^2),rep(2,rat)), nrow=rat)
)

source("4_total_cumul.R")
source("4_legend.R")

dev.off()