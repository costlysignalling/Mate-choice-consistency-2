
change.res<-read.table("raw.results.changes.adjusted.txt",header=T,stringsAsFactors=F)
all.res<-read.table("raw.results.adjusted.txt",header=T,stringsAsFactors=F)

change.res[5,1]<-"age difference"
change.res[8,1]<-"eye colour"
change.res[9,1]<-"hair colour"
change.res[10,1]<-"facial masculinity"
change.res[11,1]<-"beardedness"
change.res[12,1]<-"muscularity"
change.res[14,1]<-"relative height"
change.res[15,1]<-"hirsuteness"
change.res[16,1]<-"leg to body ration"
change.res[20,1]<-"emotional stability"


plot.comp<-function(i){
desc<-mean.results[[i]]
chan<-change.res[i,]
alline<-all.res[i,]

type<-alline[[19]]

color1<-
ifelse(type=="dem","#D7FFD7",
ifelse(type=="phys","#D7AFFF",
ifelse(type=="psych","#38D7FF",NA)))

N<-nonfather.base[[i]]
P<-father.base[[i]]


if(bw.opt[i]==1){
N<-density(N,bw=0.5)
P<-density(P,bw=0.5)
}else{
N<-density(N)
P<-density(P)
}

tot<-c(nonfather.base[[i]],father.base[[i]])

xtot<-c(N$x,P$x)
ytot<-c(N$y,P$y)

minx<-min(xtot)
maxx<-max(xtot)
ranx<-maxx-minx

miny<-min(ytot)
maxy<-max(ytot)
rany<-maxy-miny

rat.ad<-0.1

par(mar=c(1.3,0,2.7,0))

plot(N$x,N$y,axes=F,bty="n",type="n",xlim=c(minx-ranx*rat.ad,maxx+ranx*rat.ad),ylim=c(miny-rany*rat.ad,maxy+rany*rat.ad))
rect(-1000,-1000,1000,1000,col=color1,xpd=T)

inter.min<-apply(rbind(N$y,P$y),2,min)

if(N$y[1]<N$y[length(N$y)]){
polygon(c(N$x,N$x[length(N$x)]),c(N$y,N$y[1]),col="#FFFFFF",border=NA)
polygon(c(P$x,P$x[length(P$x)]),c(P$y,P$y[1]),col="#000000",border=NA)
polygon(c(P$x,P$x[length(P$x)]),c(inter.min,inter.min[1]),col="#808080",border=NA)

}else{
polygon(c(N$x,N$x[1]),c(N$y,N$y[length(N$y)]),col="#FFFFFF",border=NA)
polygon(c(P$x,P$x[1]),c(P$y,P$y[length(P$y)]),col="#000000",border=NA)
polygon(c(P$x,P$x[length(P$x)]),c(inter.min,inter.min[length(inter.min)]),col="#808080",border=NA)
}

lines(c(desc[1],desc[1]),c(miny+rany*(5*rat.ad),maxy+rany*(rat.ad/2)),lwd=2,col="#FFFFFF")
lines(c(desc[2],desc[2]),c(miny+rany*(5*rat.ad),maxy+rany*(rat.ad/2)),lwd=2,col="#000000")
lines(c(desc[1],desc[2]),c(maxy+rany*(rat.ad/2),maxy+rany*(rat.ad/2)),lwd=2,col="#FF6600")

arrows(desc[[1]]-desc[[7]],
miny-rany*(-1*rat.ad/5),
desc[[1]]+desc[[7]],
miny-rany*(-1*rat.ad/5),code=3,angle=90,length=0.1,lwd=2,col="#FFFFFF"
)

arrows(desc[[2]]-desc[[8]],
miny-rany*(-4*rat.ad/5),
desc[[2]]+desc[[8]],
miny-rany*(-4*rat.ad/5),code=3,angle=90,length=0.1,lwd=2,col="#000000"
)


difvar<-abs(desc[[7]]-desc[[8]])
mean.c<-mean(c(desc[[1]],desc[[2]]))

arrows(mean.c-difvar,
miny-rany*(-7*rat.ad/5),
mean.c+difvar,
miny-rany*(-7*rat.ad/5),code=3,angle=90,length=0.1,lwd=2,col="#FFCC00"
)

sig1<-chan[[18]]
sig1<-ifelse(sig1<0.001,"***",ifelse(sig1<0.01,"**",ifelse(sig1<0.05,"*","")))

sig2<-chan[[24]]
sig2<-ifelse(sig2<0.001,"***",ifelse(sig2<0.01,"**",ifelse(sig2<0.05,"*","")))

sig3<-chan[[12]]
if(sig3<0.05){
box("figure",lwd=4,col=2)
}
sig3<-ifelse(sig3<0.001,"***",ifelse(sig3<0.01,"**",ifelse(sig3<0.05,"*","")))



text(mean.c,maxy+rany*(1.6*rat.ad),sig1,col="#FF6600",cex=4,xpd=T)
text(mean.c,miny-rany*(-18*rat.ad/5),sig2,col="#FFCC00",cex=4,xpd=T)
text(minx,(miny+maxy)/2,sig3,col="#FF0000",cex=4,xpd=T,pos=4,srt=90)

sumaz<-summary(as.factor(tot),maxsum=1000)

tok<-as.numeric(names(sumaz))

len.tic<-sumaz/sum(sumaz)*(-0.18)

minax<-min(tok)
maxax<-max(tok)

par(mgp=c(3,1,-0.5))

axis(1,at=tok,labels=NA,tck=0,lwd=2)

for(i in 1:length(tok)){
axis(1,at=tok[i],tck=len.tic[i],labels="",lwd.ticks=2)
}

text(minax,miny-rany*(1.1*rat.ad),minax,xpd=T,pos=2,cex=1.5)
text(maxax,miny-rany*(1.1*rat.ad),maxax,xpd=T,pos=4,cex=1.5)

text(minx+ranx/2,maxy+rany/3,chan[[1]],xpd=T,cex=2)
}

bw.opt<-c(1,1,2,2,2,1,1,1,2,1,1,1,1,1,1,1,2,2,2,2,2)

tiff("compare_compose.tif",width=15,height=35,res=300,units="cm")

par(mfrow=c(7,3))

for(distrib in 1:21){
plot.comp(distrib)
}

dev.off()



