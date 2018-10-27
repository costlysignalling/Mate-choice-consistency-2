cor.results<-read.table("cor.results1.txt",header=T,sep=";")
cor.pvals<-read.table("cor.pvals.txt",header=T,sep=";")
explained.var.trait1<-read.table("explained.var.trait1.txt",header=T,sep=";")
resid.var.trait2<-read.table("resid.var.trait2.txt",header=T,sep=";")

shared<-(explained.var.trait1-t(resid.var.trait2))
shared2<-(t(explained.var.trait1)-(resid.var.trait2))

shared.total<-(shared+shared2)/2
View(shared.total)

toadj<-as.matrix(cor.pvals)

toadj[lower.tri(toadj, diag = T)]<-NA
adjusted<-matrix(p.adjust(toadj,method="BH"),ncol=ncol(toadj))

cor.pvals<-as.data.frame(adjusted)

colnames(cor.pvals)<-colnames(cor.results)
rownames(cor.pvals)<-rownames(cor.results)


radkuR<-nrow(cor.results)
sloupcuR<-ncol(cor.results)

cor.stars<-ifelse(cor.pvals<0.05,"*","")

p.info<-paste(unlist(format(round(cor.results,2),nsmall=2)),unlist(cor.stars), sep="")
p.info<-array(p.info,dim=c(radkuR,sloupcuR))
p.info

shared.info<-format(round(shared.total,1),nsmall=1)
shared.info<-paste(unlist(shared.info),"%")
shared.info<-array(shared.info,dim=c(radkuR,sloupcuR))
shared.info

resultino<-p.info

for(i in 1:radkuR){
for(j in 1:sloupcuR){
if(i==j){
resultino[i,j]<-shared.info[i,j]
break
}else{
resultino[i,j]<-shared.info[i,j]
}
}
}

newnames<-colnames(cor.stars)

newnames[5]<-"age difference"
newnames[8]<-"eye colour"
newnames[9]<-"hair colour"
newnames[10]<-"facial masculinity"
newnames[11]<-"beardedness"
newnames[12]<-"muscularity"
newnames[14]<-"relative height"
newnames[15]<-"hirsuteness"
newnames[16]<-"leg to body ration"
newnames[20]<-"emotional stability"

colnames(resultino)<-newnames
rownames(resultino)<-newnames


write.table(resultino, "resultino_final.txt", sep=";")


sump<-adjusted
sump<-sump[!is.na(sump)]
length(sump)
sum(sump<0.05)
