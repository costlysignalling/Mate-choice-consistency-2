perms<-read.delim("all.perms.txt")
res<-read.delim("raw.results.txt",stringsAsFactors=F)

res[,1]

res[5,1]<-"age difference"
res[8,1]<-"eye colour"
res[9,1]<-"hair colour"
res[10,1]<-"facial masculinity"
res[11,1]<-"beardedness"
res[12,1]<-"muscularity"
res[14,1]<-"relative height"
res[15,1]<-"hirsuteness"
res[16,1]<-"leg to body ration"
res[20,1]<-"emotional stability"




tiff("permhist.tif",width=18,height=17,res=600,units="cm",compression="lzw")

dist.crit<-(res[,11]-res[,10])/res[,12]
type.means<-tapply(dist.crit,res[,19],mean)

pos.text<-c(rep(-1,7),rep(19.5,14))

perms<-perms[order(dist.crit,decreasing=T),]
res<-res[order(dist.crit,decreasing=T),]


perms<-perms[order(res[,19]),]
res<-res[order(res[,19]),]


norma<-10

Llegend<-23
Rlegend<-30

Mlegend<-(Llegend+Rlegend)/2


par(mar=c(3, 4.6, 9.1,0),mgp=c(2.5,1,0),lwd=1.5)
plot(1,1,xlim=c(-0.5,Rlegend),ylim=c(-10,20),type="n",bty="n",xlab="",xaxt="n",xaxs="i",yaxs="i",
ylab=
expression(paste("Standard deviations of the distribution of expected   ", bar(Delta))))
rect(Llegend,-100,100,28,xpd=T,col="#444444")

for(i in nrow(res):1){

type<-res[i,19]

color1<-
ifelse(type=="dem","#CCFFCC88",
ifelse(type=="phys","#CC99FF88",
ifelse(type=="psych","#00CCFF88",NA)))

color2<-
ifelse(type=="dem","#228822",
ifelse(type=="phys","#663388",
ifelse(type=="psych","#006688",NA)))


exp<-perms[i,]
exp<-as.numeric(exp)

obs.mean<-res[i,10]
exp.mean<-res[i,11]
sd.mean<-res[i,12]

d<-density(exp)

x<-d$x
y<-d$y

newY<-(x-obs.mean)/sd.mean
newX<-(y*exp.mean)/norma+(i-1)

polygon(newX,newY,col=color1,lwd=1.2,border=color2)
abline(v=i-1,col=color2)

dist<-(exp.mean-obs.mean)/sd.mean

drawY<-newY[which(abs(newY-dist)==min(abs(newY-dist)))]
drawX<-newX[which(abs(newY-dist)==min(abs(newY-dist)))]


lines(c((i-1),(drawX)),c(drawY,drawY),col=color2)



text(i-1.15,pos.text[i],res[i,1],srt=270,pos=4,col=color2,cex=0.9)

}

colorT<-c("#228822","#663388","#006688")
colorT2<-c("#22EE22","#AA33EE","#00AAEE")


lines(c(0,2),rep(par("usr")[4],2),col=colorT[1],xpd=T)
lines(c(3,15),rep(par("usr")[4],2),col=colorT[2],xpd=T)
lines(c(16,20),rep(par("usr")[4],2),col=colorT[3],xpd=T)

axis(3, 1,"demographic", col = NA, col.ticks = colorT[1],col.axis=colorT[1],lwd.tick=1.5)
axis(3, 9,"physical", col = NA, col.ticks = colorT[2],col.axis=colorT[2],lwd.tick=1.5)
axis(3, 18,"psychological", col = NA, col.ticks = colorT[3],col.axis=colorT[3],lwd.tick=1.5)

mtext("Variables",3,line=2.5,at=9)
mtext("Legend",3,line=2.5,at=Mlegend,col="white",font=2)

arrows(Llegend+0.7,type.means[1],Llegend+0.7,-7,col="white",xpd=T,length=0.1)

abline(h=type.means,lty=2,col=colorT2)
abline(h=0,col="#FF4444")

for(i in 1:21){
lines(c(i-1,i-0.3),rep(par("usr")[3],2),col=1)
}
mtext(expression(paste("Density of expected  ",bar(Delta))),1,line=1,at=9)



text(Mlegend,19,
c("Distribution
of expected\n", expression(paste(bar(Delta), "  values"))),
col="#C1C1C1",xpd=T,cex=0.9)

Lhist<-rnorm(10000,15,1)
d<-density(Lhist)
d$y<-(d$y*3)

polygon(d$y+Llegend+0.7,d$x,col="#FFFFFFAA",border="white",lwd=1.2)

take<-which(abs(d$x-mean(Lhist))==min(abs(d$x-mean(Lhist))))
lines(c(Llegend+0.7,d$y[take]+Llegend+0.7),rep(d$x[take],2),col="white")

text(Mlegend-1.5,13.5,
c("Mean
expected\n", expression(paste(bar(Delta), "  value"))),
col="white",xpd=T,cex=0.9,pos=4)


text(Mlegend,-1.3,
c("Observed\n", expression(paste(bar(Delta), "  value"))),
col="#FF4444",xpd=T,cex=0.9,pos=1)

text(Mlegend,6.5,
c("Mean value
per
variable group"),
col=colorT2[1],xpd=T,cex=0.9)


rect(-100,26,100,100,col="#EEEEEE",xpd=T,border=NA)
text(-4.5,31,"Results of permutation tests of mate choice consistency",xpd=T,pos=4,font=2,cex=1.2)
text(-4.5,29,expression(paste("comparing observed average difference between partners of an individual (",bar(Delta),")")),xpd=T,pos=4,cex=1)
text(-4.5,27.5,expression(paste("with the distribution of expected ",bar(Delta)," values in population with random pairing")),xpd=T,pos=4,cex=1)


text(Mlegend,-4,expression(paste("Lower ",bar(Delta)," :")),xpd=T,pos=1,cex=0.9,col="white")
text(Mlegend,-5.9,"higher \nconsistency \nof mate choice",xpd=T,pos=1,cex=0.9,col="white")


dev.off()
