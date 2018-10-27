data<-data2

sumaz<-summary(as.factor(usedID),maxsum=10000)
mean(sumaz)
median(sumaz)

usable<-NA
contribute<-NA

for(i in 1:nrow(data)){
radek<-match(data$ID[i],names(sumaz))
usable[i]<-ifelse(is.na(radek),FALSE,TRUE)
contribute[i]<-ifelse(is.na(radek),0,sumaz[radek])
}

contribute

data$usable<-usable
data$contribute<-contribute

data.second<-data

for(name in 1:21){
subset<-data[getcol(rownames(definitivo)[name])]
isi<-!is.na(subset)
orma<-matrix(1:10,ncol=10,nrow=nrow(data),byrow=T)
for(ein in 1:nrow(orma)){
for(zwei in 1:ncol(orma)){
orma[ein,zwei]<-ifelse(isi[ein,zwei]==T,orma[ein,zwei],0)
}
}

data[,ncol(data)+1]<-apply(orma,1,max)
data.second[,ncol(data.second)+1]<-apply(isi,1,sum)
print(ncol(data))
print(ncol(data.second))
}

partinds<-data[,317:337]

data<-data[data$usable,]

Npartners<-apply(partinds,1,max)
length(Npartners)

sum(Npartners)

mean(Npartners)
median(Npartners)
sd(Npartners)
range(Npartners)
summary(as.factor(Npartners))/sum(summary(as.factor(Npartners)))


mean(data$age)
median(data$age)
sd(data$age)
range(data$age)

Nresp<-raw.results[,1]
Npart<-raw.results[,2]

mean(Nresp)
median(Nresp)
sd(Nresp)
range(Nresp)


mean(Npart)
median(Npart)
sd(Npart)
range(Npart)

mean()
median()
sd()
range()









