shared.total<-read.table("shared.total1_perm.txt",header=T,sep=";")



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

grad.sum
sort1<-un1[match(names(cumul),names(un1))]

lx<-1:length(lab)-0.30
rx<-1:length(lab)+0.30

by<-grad.sum-sort1
ty<-grad.sum
















