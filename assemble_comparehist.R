

S1<-matrix(1,ncol=16,nrow=2)
S2<-matrix(rep(2:8,each=5),ncol=1,nrow=5*7)

S9<-matrix(9,ncol=5,nrow=5)
S10<-matrix(10,ncol=5,nrow=5)
S11<-matrix(11,ncol=5,nrow=5)
S3<-cbind(S9,S10,S11)

S12<-matrix(12,ncol=5,nrow=5)
S13<-matrix(13,ncol=5,nrow=5)
S14<-matrix(14,ncol=5,nrow=5)
S4<-cbind(S12,S13,S14)

S15<-matrix(15,ncol=5,nrow=5)
S16<-matrix(16,ncol=5,nrow=5)
S17<-matrix(17,ncol=5,nrow=5)
S5<-cbind(S15,S16,S17)

S18<-matrix(18,ncol=5,nrow=5)
S19<-matrix(19,ncol=5,nrow=5)
S20<-matrix(20,ncol=5,nrow=5)
S6<-cbind(S18,S19,S20)

S21<-matrix(21,ncol=5,nrow=5)
S22<-matrix(22,ncol=5,nrow=5)
S23<-matrix(23,ncol=5,nrow=5)
S7<-cbind(S21,S22,S23)

S24<-matrix(24,ncol=5,nrow=5)
S25<-matrix(25,ncol=5,nrow=5)
S26<-matrix(26,ncol=5,nrow=5)
S8<-cbind(S24,S25,S26)

S27<-matrix(27,ncol=5,nrow=5)
S28<-matrix(28,ncol=5,nrow=5)
S29<-matrix(29,ncol=5,nrow=5)
S9<-cbind(S27,S28,S29)


SA<-rbind(S3,S4,S5,S6,S7,S8,S9)
SA<-cbind(S2,SA)
SA<-rbind(S1,SA)


SA<-cbind(SA,matrix(30,ncol=5,nrow=37))

tiff("compare_compose2.tif",width=21,height=35,res=600,units="cm",compression="lzw")

layout(SA)

par(mar=c(0,0,0,0))
plot(1:100,1:100,type="n",bty="n",axes=F)
rect(-100,-100,1000,1000,col="#EEEEEE",xpd=T,border=NA)
text(0,50,"Distribution comparison of fathers and non-fathers",cex=2.2,font=2,pos=4)

for(ploti in 1:7){
plot(1:100,1:100,axes=F,type="n")
lines(c(60,80,80,60),c(90,90,16,16),lwd=2)
text(53,53,"Density",cex=1.5,srt=90)
}



for(distrib in 1:21){
plot.comp(distrib)
}

#for(ploti in 1:20){
#plot(1:100,1:100,axes=F,col=4,pch=1.5)
#}

#####################
###here starts legend
#####################

par(mar=c(0,0,0,0))
plot(1:100,seq(-20,100,length.out=100),type="n",bty="n",axes=F)

rect(-20,97.75,200,200,col="#444444",border=NA,xpd=T)

mean.bar<-(par("usr")[4]+97.75)/2

text(50,mean.bar,"Legend",cex=2.2,font=2,col="white")

rect(-20,85,200,97.75,col="#AACCEE",border=NA,xpd=T)

text(50,96.5,"Distributions",cex=1.5,col="#444444")

text(50,94,"fathers",cex=1.5,pos=4,col="#000000")
text(50,94,"non-fathers",cex=1.5,pos=2,col="#FFFFFF")


P<-rnorm(10000,60,10)
N<-rnorm(10000,40,10)

P<-density(P)
N<-density(N)

O1<-data.frame(P$x[P$x<max(N$x)],P$y[P$x<max(N$x)],NA,NA)
O2<-data.frame(N$x[N$x>min(P$x)],N$y[N$x>min(P$x)],NA,NA)

names(O1)<-c("x","y","x2","y2")
names(O2)<-c("x","y","x2","y2")

for(i in 1:nrow(O1)){
take<-which.min(abs(O2$x-O1$x[i]))
O1$x2[i]<-O2$x[take]
O1$y2[i]<-O2$y[take]
}

for(i in 1:nrow(O2)){
take<-which.min(abs(O1$x-O2$x[i]))
O2$x2[i]<-O1$x[take]
O2$y2[i]<-O1$y[take]
}

O1$ydef<-apply(rbind(O1$y,O1$y2),2,min)
O2$ydef<-apply(rbind(O2$y,O2$y2),2,min)

polygon(P$x,P$y*100+88.5,border=NA,col="#000000")
polygon(N$x,N$y*100+88.5,border=NA,col="#FFFFFF")
polygon(O1$x,O1$ydef*100+88.5,border=NA,col="#808080")
polygon(O2$x,O2$ydef*100+88.5,border=NA,col="#808080")

text(50,88,"overlap",cex=1.5,pos=1,col="#444444")


rect(-20,65.5,200,85,col="#444444",border=NA,xpd=T)

text(50,84,"Significance of the \ndifferecnce between \ngroup means",cex=1.5,pos=1,col="#FF6600")
text(50,78.5,"***",cex=4,pos=1,col="#FF6600")
text(50,76.5,"Differecnce between \ngroup means",cex=1.5,pos=1,col="#FF6600")
lines(c(30,70),c(72,72),lwd=2,col="#FF6600")
lines(c(30,30),c(67,72),lwd=2,col="#FFFFFF")
lines(c(70,70),c(67,72),lwd=2,col="#000000")

text(50,71.5,"Group\nmeans",cex=1.5,pos=1,col="#EEEEEE")

rect(-20,41,200,65.5,col="#444444",border=NA,xpd=T)

text(50,64.5,"Significance of the \ndifferecnce between \ngroup variances",cex=1.5,pos=1,col="#FFCC00")
text(50,59,"***",cex=4,pos=1,col="#FFCC00")
text(50,57,"Differecnce between \ngroup variances",cex=1.5,pos=1,col="#FFCC00")
arrows(40,52,60,52,code=3,angle=90,lwd=2,length=0.1,col="#FFCC00")
arrows(15,51.5,85,51.5,code=3,angle=90,lwd=2,length=0.1,col="#000000")
arrows(25,51,75,51,code=3,angle=90,lwd=2,length=0.1,col="#FFFFFF")
text(50,49.5,"Measure of group \nvariances (mean \nwithin group distance \nfrom group mean)",cex=1.5,pos=1,col="#EEEEEE")

rect(-20,23.5,200,41,col="#444444",border=NA,xpd=T)

text(50,38,"Red box marks\ntraits where fathers\nare exceptional\nwithin partner sets.\nSignificance is\nindicated.",cex=1.5,pos=1,col="red")
rect(0,26,100,40,border=2,lwd=2)
text(8,31,"***",cex=4,pos=4,srt=90,col=2)

rect(-20,10.5,200,23.5,col="#AACCEE",border=NA,xpd=T)
axtop<-20
lines(c(20,80),c(axtop,axtop),lwd=2)
lines(rep(20,2),c(axtop,axtop-1),lwd=2)
lines(rep(40,2),c(axtop,axtop-1.5),lwd=2)
lines(rep(60,2),c(axtop,axtop-2),lwd=2)
lines(rep(80,2),c(axtop,axtop-0.5),lwd=2)

text(50,21.5,"trait value axis",cex=1.5)
text(20,axtop,"min",pos=2,cex=1.5)
text(80,axtop,"max",pos=4,cex=1.5)

text(50,17.5,"Tic length corresponds\nto the frequency of \nreported value.",pos=1,cex=1.5)

rect(-20,-100,200,10.5,col="#444444",border=NA,xpd=T)

text(50,8.5,"Variables",cex=1.5,col="#EEEEEE")

rect(0,1,100,7,col="#D7FFD7",border=NA,xpd=T)
rect(0,-5,100,1,col="#D7AFFF",border=NA,xpd=T)
rect(0,-11,100,-5,col="#38D7FF",border=NA,xpd=T)

text(50,4,"demographic",cex=1.5)
text(50,-2,"physical",cex=1.5)
text(50,-8,"personality",cex=1.5)

rect(-20,-100,200,-11,col="#444444",border=NA,xpd=T)


dev.off()

