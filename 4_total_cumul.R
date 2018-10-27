shared.total<-read.table("shared.total1.txt",header=T,sep=";")



shared.total[shared.total==0]<-0.001

unique<-function(mat){
res.un<-NA

for(i in 1:ncol(mat)){

vec<-mat[,i]
target<-vec[i]
coef<-1/vec[i]

shared.rat<-vec*coef
shared.rat<-shared.rat[-i]

nonshared<-1-shared.rat
target*prod(nonshared)
res.un[i]<-target*prod(nonshared)
}

names(res.un)<-colnames(mat)

return(res.un)
}



#Here starts the process of arrangement

lab<-names(shared.total)

orig.vec<-1:length(lab)
orde.vec<-NA


un1<-diag(as.matrix(shared.total))

maxim<-max(un1)
take<-which(un1==maxim)

orde.vec<-take
orig.vec<-orig.vec[-take]

cumul<-maxim

steps<-matrix(NA,ncol=length(lab),nrow=1)
colnames(steps)<-lab
steps<-rbind(steps,un1,un1)
steps<-steps[-1,]

#Here starts the itteration from second item

for(i in 1:(length(lab)-1)){

compare.un<-NA

for(x in orig.vec){
vec.un<-unique(shared.total[c(orde.vec,x),c(orde.vec,x)])
compare.un<-c(compare.un,vec.un[length(vec.un)])
}
compare.un<-compare.un[-1]

un2<-compare.un[match(colnames(steps),names(compare.un))]

steps<-rbind(steps,un2)

compare.un2<-apply(steps,2,min)

maxim<-max(compare.un2[is.finite(compare.un2)],na.rm=T)
take<-which(compare.un2==maxim)
namtake<-names(take)

orde.vec<-c(orde.vec,which(lab==namtake))
names(orde.vec)[length(orde.vec)]<-namtake

orig.vec<-orig.vec[-which(orig.vec==take)]

cumul<-c(cumul,maxim)

print(i)
}

#end of loop

steps<-steps[-1,]

names(cumul)<-names(orde.vec)

#Now it is sorted

cumul

cumul2<-cumul
cumul2[cumul2<0]<-0

grad.sum<-NA

for(i in 1:length(cumul)){
grad.sum[i]<-sum(cumul2[1:i])
}

actual.grad<-grad.sum
sort1<-un1[match(names(cumul),names(un1))]

lx<-1:length(lab)-0.30
rx<-1:length(lab)+0.30

by<-grad.sum-sort1
ty<-grad.sum


results<-read.delim("raw.results.txt")

ef.pas<-results$effect.size
foc<-results$focal.person.var

model<-lm((foc*100)~ef.pas)
model

sumaz<-summary(model)

inter<-sumaz[[4]][1,1]
slope<-sumaz[[4]][2,1]

exp.foc<-grad.sum*slope+inter
exp.foc<-ifelse(exp.foc>100,100,exp.foc)

par(mar=c(5.1, 4.1, 5.1, 3.1))
par(mgp=c(2.5,1,0))
plot(1:length(lab),grad.sum,ylim=c(0,100),col=1,type="n",bty="n",axes=F,ylab="Effect size",xlab="Variables ordered by unique contribution to overall consistency")


lines(1:length(lab),grad.sum,ylim=c(0,100),lwd=1.5,col="#FF0066")
lines(1:length(lab),exp.foc,ylim=c(0,100),lwd=1.5,col="#0066FF")
abline(h=100,lty=2,col="#CCCCCC")
abline(h=50,lty=2,col="#CCCCCC")

rect(lx,by,rep(rx[length(lab)],length(lab)),ty,border=NA,col="#FFEEEE")
rect(lx,by,rx,ty,col="#66FF6666",border=NA)
rect(lx,c(0,ty[1:(length(ty)-1)]),rx,ty,col="#00DD00",border=NA)

axis(2,seq(0,100,25),paste(seq(0,100,25),"%"))
axis(1,seq(1,length(lab),4),seq(1,length(lab),4))

labnames<-names(cumul)
labnames[4]<-"age difference"
labnames[14]<-"eye colour"
labnames[6]<-"hair colour"
labnames[20]<-"facial masculinity"
labnames[21]<-"beardedness"
labnames[19]<-"muscularity"
labnames[3]<-"relative height"
labnames[18]<-"hirsuteness"
labnames[13]<-"leg to body ration"
labnames[15]<-"emotional stability"


text(1:length(lab)-0.35,grad.sum+2,labels=labnames,srt=90,pos=4,cex=0.95)

cumul2
grad.sum

sum(cumul2[1:10])

sum(cumul2[11:21])



list.files(getwd())

#Load in and draw expected level
source("4_total_cumul_perm.R")

polygon(c(lx[1],(lx+rx)/2,rx[length(rx)],rx[length(rx)]),c(0,grad.sum,grad.sum[length(grad.sum)],0),col="#DDCCCC",border=NA)

lines(1:length(lab),grad.sum,ylim=c(0,100),lwd=1.5,col="#555555")


alleft<-10.8

text(c(alleft,alleft),c(30.5,22.5),c(
"Maximal cummulative effect size",
"in % of partners that need to be 
switched between respondents to 
avoid mate choice consistency"),
pos=4,cex=c(0.95,0.8),col=c("#FF0066",1))

rect(lx[1],0,rx[1],actual.grad[1],col="#00DD00",border=NA)

text(c(alleft,alleft),c(5.5,2.2),c(
"Expected maximal cummulative effect size",
"based on the assumption of random pairing"),
pos=4,cex=c(0.95,0.8),col=c("#555555",1))

tics<-c(actual.grad[length(actual.grad)],grad.sum[length(grad.sum)])
par(mgp=c(2,1,0))
axis(4,c(tics),c("",""),col="#FF8888")

mtext("The actual contribution of 
consistent mate choice", 
side=4, line=1.1,cex=0.62,at=mean(tics),col="#FF8888")

text(c(alleft,alleft),c(94,88),c(
"Maximal cumulative effect size",
"in % of variance accounted for by 
respondent (estimated)"),
pos=4,cex=c(0.95,0.8),col=c("#0066FF",1))


rect(-20,110,120,150,col="#EEEEEE",xpd=T,border=NA)
text(-2,118,
"Effect sizes of all variables and their relative 
contribution to overall mate choice consistency",
pos=4,font=2,cex=1.2,xpd=T)





