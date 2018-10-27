##Combination of all scripts in one - delegation of N runs in the beginning 

library(stringi)
library(lmerTest)


runs.consistency.normal<-10000
runs.consistency.pasians<-1000

runs.consistency.changes<-10000
runs.shared.base<-10000
runs.shared.pas<-1000


#in all cases focusing on partners with all information

data<-read.delim("raw.data.txt",stringsAsFactors=F)

View(data)

###uprava dat
names(data)

###takový to "více než" jsem nahradil dalším nejmenším èíslem, ale je to asi dost ošemetný, protože pak budu mít u nìkterejch lidí fùru úplnì stejjnejch hodnot. Možná trochu bacha s interpretací 

nrow(data)

data<-data[data$age>17,]
nrow(data)

data<-data[data$age<46,]
nrow(data)


data<-data[data$kinsey<4,]
nrow(data)

data<-data[ifelse(is.na(data$grow.up),F,ifelse(data$grow.up==1,T,F)),]
nrow(data)


###zacinaj.permutace
###TESTU KONZISTENCE

runs<-runs.consistency.normal
runs.pas<-runs.consistency.pasians


get.result.line<-function(vector){

substrat<-data[,vector]

partneru<-1:nrow(substrat)

for(i in 1:nrow(substrat)){
partneru[i]<-(10-sum(is.na(substrat[i,])))
}

partneru

substrat<-substrat[partneru>1,]
partneru<-partneru[partneru>1]

Nresp<-nrow(substrat)
Npart<-sum(partneru)

znak<-NA
resp<-NA

for(i in 1:nrow(substrat)){
znak<-c(znak,substrat[i,])
resp<-c(resp,rep(i,times=partneru[i]))
}
znak<-znak[!is.na(znak)]
znak<-as.numeric(znak)
resp<-resp[!is.na(resp)]

prumer<-mean(znak)
odchylka<-sd(znak)
rozpeti<-range(znak)

model<-lmer(znak~(1|resp))
model2<-lm(znak~1)

sumaz<-summary(model)

resp.var<-sumaz[[13]][[1]][1]
res.var <- attr(VarCorr(model), "sc")^2

resp.var<-resp.var/(resp.var+res.var) #we have to calculate the proportion of all variance


p.resp.var<-anova(model,model2)[[8]][[2]]

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in 1:nrow(substrat)){
radek<-znak[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

corel<-cor(dvojice$jeden, dvojice$druhy, method="pearson")

diffs<-1:nrow(substrat)

for(i in 1:nrow(substrat)){
poddvojice<-dvojice[mark==i,]
diffs[i]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

avg.dif<-mean(diffs)

###permutacni test

perm.cors<-1:runs
perm.difs<-1:runs

for(run in 1:runs){
znak2<-sample(znak)

dvojice<-data.frame("jeden"=NA,"druhy"=NA)

for(i in 1:nrow(substrat)){
radek<-znak2[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
}
dvojice<-dvojice[-1,]

perm.cors[run]<-cor(dvojice$jeden, dvojice$druhy, method="pearson")

diffs<-1:nrow(substrat)

for(i in 1:nrow(substrat)){
poddvojice<-dvojice[mark==i,]
diffs[i]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

perm.difs[run]<-mean(diffs)
}

expected.dif<-mean(perm.difs)
sd.dif<-sd(perm.difs)
expected.cor<-mean(perm.cors)


###efekt pasians

effect<-1:runs.pas

for(run.pas in 1:runs.pas){

pruhoz<-znak

prvni<-sample(1:length(znak),1)

rotujici<-znak[prvni]

moznosti<-1:length(znak)

vzdalenost<-avg.dif

pruhozu<-0
vymen<-0

for(vymena in 1:10000){

vymenit<-sample(moznosti[-prvni],1)

pruhoz2<-pruhoz
pruhoz2[vymenit]<-rotujici

predlajna<-pruhoz[resp==resp[vymenit]]

polajna<-pruhoz2[resp==resp[vymenit]]

predlajna
polajna

pruhozu<-pruhozu+1

preddvoj<-t(combn(predlajna,2))
preddifer<-mean(abs(preddvoj[,1]-preddvoj[,2]))
preddifer

podvoj<-t(combn(polajna,2))
podifer<-mean(abs(podvoj[,1]-podvoj[,2]))
podifer

if(podifer>preddifer){

rotujici<-pruhoz[vymenit]
pruhoz<-pruhoz2
vzdalenost<-(vzdalenost*Nresp-preddifer+podifer)/Nresp
vymen<-vymen+1

if(vzdalenost>expected.dif){
effect[run.pas]<-vymen####thle uprav jestli to funguje
break
}
}else{

rotujici<-rotujici
pruhoz<-pruhoz
vzdalenost<-vzdalenost
}
}


}

ef<-round(100*mean(effect)/Npart,2)
ef.down<-round(100*quantile(effect, c(0.025,0.975))/Npart,2)[1]
ef.up<-round(100*quantile(effect, c(0.025,0.975))/Npart,2)[2]
paste(ef,"(",ef.down,",",ef.up,")", sep="")


vysledky<-data.frame(
Nresp,
Npart,
round(prumer,2),
round(odchylka,2),
paste(rozpeti[1],"-",rozpeti[2], sep=""),
round(corel,2),
round(sum(perm.cors>corel)/length(perm.cors),3),
round(avg.dif,2),
round(expected.dif,2),
round(sd.dif,2),
round(sum(perm.difs<avg.dif)/length(perm.difs),3),
paste(ef,"(",ef.down,",",ef.up,")", sep=""),
round(resp.var*100,2),
round(p.resp.var,3))

raw<-data.frame(
Nresp,
Npart,
prumer,
odchylka,
rozpeti[1],rozpeti[2],
corel,
sum(perm.cors>corel)/length(perm.cors),
avg.dif,
expected.dif,
sd.dif,
sum(perm.difs<avg.dif)/length(perm.difs),
ef,ef.down,ef.up,
resp.var,p.resp.var)



names(vysledky)<-c("N resp.","N part.","mean","SD","range","Pearson r","p-value","average difference","expected difference","SD(exp.dif)","p-value","effect size % (95% CI)","focal person var.","p val. random")
names(raw)<-c("N resp.","N part.","mean","SD","range.low","range.up","Pearson r","p-value","average difference","expected difference","SD(exp.dif)","p-value","effect size","lower(95% CI)","upper(95% CI)","focal person var.","p val. random")

return(list(vysledky,perm.difs,raw))
}

###definovani vlastnosti



getcol<-function(name){which(substr(names(data),1,stri_length(name))==name)}

residence<-getcol("residence")
education<-getcol("education")[-1]
weight<-getcol("weight")
height<-getcol("height")
age.diff<-getcol("age.diff")
attractiveness<-getcol("attractiveness")
masculinity<-getcol("masculinity")
eyes<-getcol("eyes")
hair<-getcol("hair")
face.masculinity<-getcol("face.masculinity")
beard<-getcol("beard")
muscles<-getcol("muscles")
BMI<-getcol("BMI")
relative.height<-getcol("relative.height")
body.hair<-getcol("body.hair")
leg.length<-getcol("leg.length")
extraversion<-getcol("extraversion")
agreeableness<-getcol("agreeableness")
conscientiousness<-getcol("conscientiousness")
stability<-getcol("stability")
openness<-getcol("openness")

residenceR<-get.result.line(residence)
educationR<-get.result.line(education)
weightR<-get.result.line(weight)
heightR<-get.result.line(height)
age.diffR<-get.result.line(age.diff)
attractivenessR<-get.result.line(attractiveness)
masculinityR<-get.result.line(masculinity)
eyesR<-get.result.line(eyes)
hairR<-get.result.line(hair)
face.masculinityR<-get.result.line(face.masculinity)
beardR<-get.result.line(beard)
musclesR<-get.result.line(muscles)
BMIR<-get.result.line(BMI)
relative.heightR<-get.result.line(relative.height)
body.hairR<-get.result.line(body.hair)
leg.lengthR<-get.result.line(leg.length)
extraversionR<-get.result.line(extraversion)
agreeablenessR<-get.result.line(agreeableness)
conscientiousnessR<-get.result.line(conscientiousness)
stabilityR<-get.result.line(stability)
opennessR<-get.result.line(openness)


definitivo<-rbind(
residenceR[[1]],
educationR[[1]],
weightR[[1]],
heightR[[1]],
age.diffR[[1]],
attractivenessR[[1]],
masculinityR[[1]],
eyesR[[1]],
hairR[[1]],
face.masculinityR[[1]],
beardR[[1]],
musclesR[[1]],
BMIR[[1]],
relative.heightR[[1]],
body.hairR[[1]],
leg.lengthR[[1]],
extraversionR[[1]],
agreeablenessR[[1]],
conscientiousnessR[[1]],
stabilityR[[1]],
opennessR[[1]]
)

all.perms<-rbind(
residenceR[[2]],
educationR[[2]],
weightR[[2]],
heightR[[2]],
age.diffR[[2]],
attractivenessR[[2]],
masculinityR[[2]],
eyesR[[2]],
hairR[[2]],
face.masculinityR[[2]],
beardR[[2]],
musclesR[[2]],
BMIR[[2]],
relative.heightR[[2]],
body.hairR[[2]],
leg.lengthR[[2]],
extraversionR[[2]],
agreeablenessR[[2]],
conscientiousnessR[[2]],
stabilityR[[2]],
opennessR[[2]]
)

raw.results<-rbind(
residenceR[[3]],
educationR[[3]],
weightR[[3]],
heightR[[3]],
age.diffR[[3]],
attractivenessR[[3]],
masculinityR[[3]],
eyesR[[3]],
hairR[[3]],
face.masculinityR[[3]],
beardR[[3]],
musclesR[[3]],
BMIR[[3]],
relative.heightR[[3]],
body.hairR[[3]],
leg.lengthR[[3]],
extraversionR[[3]],
agreeablenessR[[3]],
conscientiousnessR[[3]],
stabilityR[[3]],
opennessR[[3]]
)

rownames(definitivo)<-c(
"residence",
"education",
"weight",
"height",
"age.diff",
"attractiveness",
"masculinity",
"eyes",
"hair",
"face.masculinity",
"beard",
"muscles",
"BMI",
"relative.height",
"body.hair",
"leg.length",
"extraversion",
"agreeableness",
"conscientiousness",
"stability",
"openness"
)

rownames(all.perms)<-rownames(definitivo)
rownames(raw.results)<-rownames(definitivo)


simp.r<-raw.results[,7]
foc<-raw.results[,16]
ef.pas<-raw.results[,13]

p1<-raw.results[,8]
p2<-raw.results[,12]
p3<-raw.results[,17]

pA1<-raw.results[,8]
pA2<-raw.results[,12]
pA3<-raw.results[,17]

ef.size<-cbind(simp.r,foc,ef.pas)
p.vals<-cbind(p1,p2,p3)

sink("sink.cors.etc.txt")
cor(ef.size)

cor(p.vals)

model<-lm((foc*100)~ef.pas)
model
sink()

names(raw.results)
variable.type<-c(
"dem","dem","phys",
"phys","dem","phys",
"phys","phys","phys",
"phys","phys","phys",
"phys","phys","phys",
"phys","psych","psych",
"psych","psych","psych")

definitivo$variable.type<-variable.type
raw.results$variable.type<-variable.type

write.table(definitivo,"results.txt",sep="\t",col.names=NA)
write.table(all.perms,"all.perms.txt",sep="\t")
write.table(raw.results,"raw.results.txt",sep="\t",col.names=NA)

sink("sink.pairwise.txt")
compare<-lm(ef.pas~variable.type)
summary(compare)
TukeyHSD(aov(compare))

compare<-lm(foc~variable.type)
summary(compare)
TukeyHSD(aov(compare))

compare<-lm(simp.r~variable.type)
summary(compare)
TukeyHSD(aov(compare))
sink()

p1<-p.adjust(p1,method="BH")
p2<-p.adjust(p2,method="BH")
p3<-p.adjust(p3,method="BH")

raw.results[,8]<-p1
raw.results[,12]<-p2
raw.results[,17]<-p3


definitivo[,7]<-round(p1,3)
definitivo[,11]<-round(p2,3)
definitivo[,14]<-round(p3,3)


write.table(definitivo,"results.adjusted.txt",sep="\t",col.names=NA)
write.table(raw.results,"raw.results.adjusted.txt",sep="\t",col.names=NA)




###consistency changes fathers vs nonfathers



runs<-runs.consistency.changes


#take presence of children with given partner as a filter
cols.filter<-getcol("Nkids")
filter<-data[,cols.filter]
filter<-ifelse(is.na(filter),F,ifelse(filter>0,T,F))


get.change.line<-function(vector){

orig.sample<-data[,vector]

avg.diff<-function(substrat){

partneru<-1:nrow(substrat)

for(i in 1:nrow(substrat)){
partneru[i]<-(10-sum(is.na(substrat[i,])))
}

substrat<-substrat[partneru>1,]
partneru<-partneru[partneru>1]

Nresp<-nrow(substrat)
Npart<-sum(partneru)

znak<-NA
resp<-NA

for(i in 1:nrow(substrat)){
znak<-c(znak,substrat[i,])
resp<-c(resp,rep(i,times=partneru[i]))
}
znak<-znak[!is.na(znak)]
znak<-as.numeric(znak)
resp<-resp[!is.na(resp)]

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in 1:nrow(substrat)){
radek<-znak[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

diffs<-1:nrow(substrat)

for(i in 1:nrow(substrat)){
poddvojice<-dvojice[mark==i,]
diffs[i]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

return(c(Nresp,Npart,mean(diffs)))
}

orig.vec<-avg.diff(orig.sample)
orig.N<-orig.vec[1]
orig.Npart<-orig.vec[2]
orig.dist<-orig.vec[3]


filtered.orig<-orig.sample
only.fathers<-orig.sample
filt.ID<-matrix(rep(1:nrow(orig.sample),each=10),ncol=10,byrow=T)
fath.ID<-filt.ID


for(i in 1:nrow(orig.sample)){
for(j in 1:ncol(orig.sample)){
filtered.orig[i,j]<-ifelse(filter[i,j]==T,NA,filtered.orig[i,j])
only.fathers[i,j]<-ifelse(filter[i,j]==F,NA,only.fathers[i,j])
filt.ID[i,j]<-ifelse(is.na(filtered.orig[i,j]),NA,filt.ID[i,j])
fath.ID[i,j]<-ifelse(is.na(only.fathers[i,j]),NA,fath.ID[i,j])
}
}

fathers<-unlist(only.fathers)
nonfathers<-unlist(filtered.orig)
fath.ID<-unlist(fath.ID)
filt.ID<-unlist(filt.ID)



fathers<-fathers[!is.na(fathers)]
nonfathers<-nonfathers[!is.na(nonfathers)]
fath.ID<-fath.ID[!is.na(fath.ID)]
filt.ID<-filt.ID[!is.na(filt.ID)]

allmen<-c(fathers,nonfathers)
fatherhood<-c(rep(T,length(fathers)),rep(F,length(nonfathers)))
ID<-c(fath.ID,filt.ID)


means<-tapply(allmen,fatherhood,mean)

model<-lmer(allmen~fatherhood+(1|ID))
sumaz<-summary(model)
intercept<-sumaz[[10]][1,1]
father.effect<-sumaz[[10]][2,1]
stderr<-sumaz[[10]][2,2]
pval<-sumaz[[10]][2,5]


fath.dist<-abs(fathers-mean(fathers))
filt.dist<-abs(nonfathers-mean(nonfathers))

distances<-c(fath.dist,filt.dist)

LTmeans<-tapply(distances,fatherhood,mean)

model<-lmer(distances~fatherhood+(1|ID))
sumaz<-summary(model)
LTintercept<-sumaz[[10]][1,1]
LTfather.effect<-sumaz[[10]][2,1]
LTstderr<-sumaz[[10]][2,2]
LTpval<-sumaz[[10]][2,5]


change.vec<-avg.diff(filtered.orig)
change.N<-change.vec[1]
change.Npart<-change.vec[2]
obs.change.dist<-change.vec[3]-orig.dist


outtakes<-orig.sample
taken<-rowSums(!is.na(outtakes))>1

for(i in 1:nrow(orig.sample)){
for(j in 1:ncol(orig.sample)){
outtakes[i,j]<-ifelse((filter[i,j]==T)&(!is.na(orig.sample[i,j]))&taken[i],T,F)
}
}

N.outtakes<-sum(outtakes)
outtakes.from<-sum(rowSums(outtakes)>0)

exp.changes<-NA

for(run in 1:runs){
perm.sample<-orig.sample
for(i in 1:nrow(orig.sample)){
radek<-perm.sample[i,]
whereput<-which(!is.na(radek))
radek[whereput]<-sample(radek[whereput])
perm.sample[i,]<-radek
}

filtered.perm<-perm.sample
for(i in 1:nrow(perm.sample)){
for(j in 1:ncol(perm.sample)){
filtered.perm[i,j]<-ifelse(filter[i,j]==T,NA,filtered.perm[i,j])
}
}

exp.changes[run]<-avg.diff(filtered.perm)[3]-orig.dist
}

exp.change.dist<-mean(exp.changes)
exp.change.sd<-sd(exp.changes)

two.tailed.p<-min(c(
sum(exp.changes<obs.change.dist)/runs,
sum(exp.changes>obs.change.dist)/runs))*2

vysledky<-data.frame(
orig.N,
orig.Npart,
N.outtakes,
outtakes.from,
change.N,
change.Npart,
round(orig.dist,2),
round(obs.change.dist,2),
round(exp.change.dist,2),
round(exp.change.sd,2),
round(two.tailed.p,3),
round(means[1],2),
round(means[2],2),
round(intercept,2),
round(father.effect,2),
round(stderr,2),
round(pval,2),
round(LTmeans[1],2),
round(LTmeans[2],2),
round(LTintercept,2),
round(LTfather.effect,2),
round(LTstderr,2),
round(LTpval,2)
)

raw<-data.frame(
orig.N,
orig.Npart,
N.outtakes,
outtakes.from,
change.N,
change.Npart,
orig.dist,
obs.change.dist,
exp.change.dist,
exp.change.sd,
two.tailed.p,
means[1],
means[2],
intercept,
father.effect,
stderr,
pval,
LTmeans[1],
LTmeans[2],
LTintercept,
LTfather.effect,
LTstderr,
LTpval
)

names(vysledky)<-c("N resp.","N part.","N part. with chidren","from N ind.","new N","new N part.","avg.diff","change in avg. diff","exp. change","exp. change SD","p-value","mean non-father","mean father","intercept non-father","effect father","std. err.","p-val. of fixed effect","mean res. non-father","mean res. father","res. intercept non-father","res. effect father","res. std. err.","res. p-val. of fixed effect")
names(raw)<-c("N resp.","N part.","N part. with chidren","from N ind.","new N","new N part.","avg.diff","change in avg. diff","exp. change","exp. change SD","p-value","mean non-father","mean father","intercept non-father","effect father","std. err.","p-val. of fixed effect","mean res. non-father","mean res. father","res. intercept non-father","res. effect father","res. std. err.","res. p-val. of fixed effect")

return(list(vysledky,exp.changes,raw))
}


#end of function definition



getcol<-function(name){which(substr(names(data),1,stri_length(name))==name)}

residence<-getcol("residence")
education<-getcol("education")[-1]
weight<-getcol("weight")
height<-getcol("height")
age.diff<-getcol("age.diff")
attractiveness<-getcol("attractiveness")
masculinity<-getcol("masculinity")
eyes<-getcol("eyes")
hair<-getcol("hair")
face.masculinity<-getcol("face.masculinity")
beard<-getcol("beard")
muscles<-getcol("muscles")
BMI<-getcol("BMI")
relative.height<-getcol("relative.height")
body.hair<-getcol("body.hair")
leg.length<-getcol("leg.length")
extraversion<-getcol("extraversion")
agreeableness<-getcol("agreeableness")
conscientiousness<-getcol("conscientiousness")
stability<-getcol("stability")
openness<-getcol("openness")

residenceR<-get.change.line(residence)
educationR<-get.change.line(education)
weightR<-get.change.line(weight)
heightR<-get.change.line(height)
age.diffR<-get.change.line(age.diff)
attractivenessR<-get.change.line(attractiveness)
masculinityR<-get.change.line(masculinity)
eyesR<-get.change.line(eyes)
hairR<-get.change.line(hair)
face.masculinityR<-get.change.line(face.masculinity)
beardR<-get.change.line(beard)
musclesR<-get.change.line(muscles)
BMIR<-get.change.line(BMI)
relative.heightR<-get.change.line(relative.height)
body.hairR<-get.change.line(body.hair)
leg.lengthR<-get.change.line(leg.length)
extraversionR<-get.change.line(extraversion)
agreeablenessR<-get.change.line(agreeableness)
conscientiousnessR<-get.change.line(conscientiousness)
stabilityR<-get.change.line(stability)
opennessR<-get.change.line(openness)


change.results<-rbind(
residenceR[[1]],
educationR[[1]],
weightR[[1]],
heightR[[1]],
age.diffR[[1]],
attractivenessR[[1]],
masculinityR[[1]],
eyesR[[1]],
hairR[[1]],
face.masculinityR[[1]],
beardR[[1]],
musclesR[[1]],
BMIR[[1]],
relative.heightR[[1]],
body.hairR[[1]],
leg.lengthR[[1]],
extraversionR[[1]],
agreeablenessR[[1]],
conscientiousnessR[[1]],
stabilityR[[1]],
opennessR[[1]]
)

all.changes<-rbind(
residenceR[[2]],
educationR[[2]],
weightR[[2]],
heightR[[2]],
age.diffR[[2]],
attractivenessR[[2]],
masculinityR[[2]],
eyesR[[2]],
hairR[[2]],
face.masculinityR[[2]],
beardR[[2]],
musclesR[[2]],
BMIR[[2]],
relative.heightR[[2]],
body.hairR[[2]],
leg.lengthR[[2]],
extraversionR[[2]],
agreeablenessR[[2]],
conscientiousnessR[[2]],
stabilityR[[2]],
opennessR[[2]]
)

raw.changes<-rbind(
residenceR[[3]],
educationR[[3]],
weightR[[3]],
heightR[[3]],
age.diffR[[3]],
attractivenessR[[3]],
masculinityR[[3]],
eyesR[[3]],
hairR[[3]],
face.masculinityR[[3]],
beardR[[3]],
musclesR[[3]],
BMIR[[3]],
relative.heightR[[3]],
body.hairR[[3]],
leg.lengthR[[3]],
extraversionR[[3]],
agreeablenessR[[3]],
conscientiousnessR[[3]],
stabilityR[[3]],
opennessR[[3]]
)

rownames(change.results)<-c(
"residence",
"education",
"weight",
"height",
"age.diff",
"attractiveness",
"masculinity",
"eyes",
"hair",
"face.masculinity",
"beard",
"muscles",
"BMI",
"relative.height",
"body.hair",
"leg.length",
"extraversion",
"agreeableness",
"conscientiousness",
"stability",
"openness"
)

rownames(all.changes)<-rownames(change.results)
rownames(raw.changes)<-rownames(change.results)

View(change.results)
View(raw.changes)

write.table(change.results,"results.changes.txt",sep="\t",col.names=NA)
write.table(all.changes,"all.perms.changes.txt",sep="\t")
write.table(raw.changes,"raw.results.changes.txt",sep="\t",col.names=NA)

names(change.results)

p1<-raw.changes[,11]
p2<-raw.changes[,17]
p3<-raw.changes[,23]

pA4<-raw.changes[,11]
pA5<-raw.changes[,17]
pA6<-raw.changes[,23]

p1<-p.adjust(p1,method="BH")
p2<-p.adjust(p2,method="BH")
p3<-p.adjust(p3,method="BH")

raw.changes[,11]<-p1
raw.changes[,17]<-p2
raw.changes[,23]<-p3

change.results[,11]<-round(p1,3)
change.results[,17]<-round(p2,3)
change.results[,23]<-round(p3,3)

write.table(change.results,"results.changes.adjusted.txt",sep="\t",col.names=NA)
write.table(raw.changes,"raw.results.changes.adjusted.txt",sep="\t",col.names=NA)



####Here starts megapermutation of shared effect


runs<-runs.shared.base
runs.pas<-runs.shared.pas


#vector1<-promenne[[1]]
#vector2<-promenne[[2]]

get.results<-function(vector1,vector2){

substrat1<-data[,vector1]
substrat2<-data[,vector2]


znak1<-NA
znak2<-NA
resp<-NA


for(i in 1:nrow(substrat1)){
znak1<-c(znak1,substrat1[i,])
znak2<-c(znak2,substrat2[i,])
resp<-c(resp,rep(i,10))
}

resp

znak1<-as.numeric(znak1)
znak2<-as.numeric(znak2)

znak1
znak2

tabulka<-data.frame(resp,znak1,znak2)

chybi<-1:nrow(tabulka)

for(i in 1:nrow(tabulka)){
chybi[i]<-sum(is.na(tabulka[i,]))
}

tabulka2<-tabulka[chybi<1,]

kolik.part<-1:max(tabulka2$resp)

for(i in 1:max(tabulka2$resp)){
kolik.part[i]<-sum(tabulka2$resp==i)
}

kolik.part

vybrat<-1:nrow(tabulka2)

for(i in 1:nrow(tabulka2)){
vybrat[i]<-sum(tabulka2$resp[i]==which(kolik.part>1))>0
}

vybrat

tabulka3<-tabulka2[vybrat==1,]


###tady uz mam dva vektory znaku ktery muzu pouzit je tam identita respondentu a jsou tam jen taci, ktery maji 2 partnery s informaci od kazdyho znaku

korelo<-cor.test(tabulka3$znak1,tabulka3$znak2)
pval<-as.numeric(korelo[3])
korelace<-as.numeric(korelo[4])

###nejdriv spocitam vsdalenosti a ocekavany vzdalenosti
levels(tabulka3$resp)

respondenti<-as.numeric(levels(as.factor(tabulka3$resp)))
length(respondenti)

resp<-tabulka3$resp
znak1<-tabulka3$znak1
znak2<-tabulka3$znak2

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in respondenti){
radek<-znak1[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

avg.dif.znak1<-mean(diffs)


###permutacni test

perm.difs1<-1:runs
perm.difs2<-1:runs

for(run in 1:runs){

znak1B<-sample(znak1)

dvojice<-data.frame("jeden"=NA,"druhy"=NA)

for(i in respondenti){
radek<-znak1B[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
}
dvojice<-dvojice[-1,]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

perm.difs1[run]<-mean(diffs)



znak2B<-sample(znak2)

dvojice<-data.frame("jeden"=NA,"druhy"=NA)

for(i in respondenti){
radek<-znak2B[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
}
dvojice<-dvojice[-1,]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

perm.difs2[run]<-mean(diffs)

}

(expected.dif1<-mean(perm.difs1))
(expected.dif2<-mean(perm.difs2))

Nresp<-length(respondenti)
Npart<-length(znak1)

###ted uz to smazim tim pasiansem pocitam rezidualni 
###vysvetlovaci kapacitu znaku 2 po vyrovnani znaku 1

effect<-1:runs.pas
effect.resid<-1:runs.pas

for(run.pas in 1:runs.pas){

pruhoz<-znak1
pruhoz.side<-znak2

prvni<-sample(1:length(znak1),1)

rotujici<-znak1[prvni]
rotujici2<-znak2[prvni]

moznosti<-1:length(znak1)

vzdalenost<-avg.dif.znak1

pruhozu<-0
vymen<-0


for(vymena in 1:10000){

vymenit<-sample(moznosti[-prvni],1)

pruhoz2<-pruhoz
pruhoz2[vymenit]<-rotujici

pruhoz.side2<-pruhoz.side
pruhoz.side2[vymenit]<-rotujici2

predlajna<-pruhoz[resp==resp[vymenit]]

polajna<-pruhoz2[resp==resp[vymenit]]

predlajna
polajna

pruhozu<-pruhozu+1

preddvoj<-t(combn(predlajna,2))
preddifer<-mean(abs(preddvoj[,1]-preddvoj[,2]))
preddifer

podvoj<-t(combn(polajna,2))
podifer<-mean(abs(podvoj[,1]-podvoj[,2]))
podifer

if(podifer>preddifer){

rotujici<-pruhoz[vymenit]
pruhoz<-pruhoz2
rotujici2<-pruhoz.side[vymenit]
pruhoz.side<-pruhoz.side2
vzdalenost<-(vzdalenost*Nresp-preddifer+podifer)/Nresp
vymen<-vymen+1

if(vzdalenost>expected.dif1){
effect[run.pas]<-vymen ####t0hle uprav jestli to funguje
pruhoz.side[prvni]<-rotujici2
break
}
}else{
rotujici<-rotujici
pruhoz<-pruhoz
vzdalenost<-vzdalenost
}
}

###tadz spocitam aktualni vzdalenost po prohazeni podle prvni promeny

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in respondenti){
radek<-pruhoz.side[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

avg.dif.znak2<-mean(diffs)


pruhoz<-pruhoz.side

prvni<-sample(1:length(znak2),1)

rotujici<-pruhoz.side[prvni]

moznosti<-1:length(znak2)

vzdalenost<-avg.dif.znak2

pruhozu<-0
vymen<-0

for(vymena in 1:10000){

vymenit<-sample(moznosti[-prvni],1)

pruhoz2<-pruhoz
pruhoz2[vymenit]<-rotujici

predlajna<-pruhoz[resp==resp[vymenit]]

polajna<-pruhoz2[resp==resp[vymenit]]

predlajna
polajna

pruhozu<-pruhozu+1

preddvoj<-t(combn(predlajna,2))
preddifer<-mean(abs(preddvoj[,1]-preddvoj[,2]))
preddifer

podvoj<-t(combn(polajna,2))
podifer<-mean(abs(podvoj[,1]-podvoj[,2]))
podifer

if(podifer>preddifer){

rotujici<-pruhoz[vymenit]
pruhoz<-pruhoz2
vzdalenost<-(vzdalenost*Nresp-preddifer+podifer)/Nresp
vymen<-vymen+1

if(vzdalenost>expected.dif2){
effect.resid[run.pas]<-vymen ####thle uprav jestli to funguje
break
}
}else{

rotujici<-rotujici
pruhoz<-pruhoz
vzdalenost<-vzdalenost
}
}


}

ef<-100*mean(effect)/Npart
ef.resid<-100*mean(effect.resid)/Npart

return(c(korelace, pval, ef, ef.resid))

}

getcol<-function(name){which(substr(names(data),1,stri_length(name))==name)}

promenne<-list(
residence<-getcol("residence"),
education<-getcol("education")[-1],
weight<-getcol("weight"),
height<-getcol("height"),
age.diff<-getcol("age.diff"),
attractiveness<-getcol("attractiveness"),
masculinity<-getcol("masculinity"),
eyes<-getcol("eyes"),
hair<-getcol("hair"),
face.masculinity<-getcol("face.masculinity"),
beard<-getcol("beard"),
muscles<-getcol("muscles"),
BMI<-getcol("BMI"),
relative.height<-getcol("relative.height"),
body.hair<-getcol("body.hair"),
leg.length<-getcol("leg.length"),
extraversion<-getcol("extraversion"),
agreeableness<-getcol("agreeableness"),
conscientiousness<-getcol("conscientiousness"),
stability<-getcol("stability"),
openness<-getcol("openness")
)

jmena<-c(
"residence",
"education",
"weight",
"height",
"age.diff",
"attractiveness",
"masculinity",
"eyes",
"hair",
"face.masculinity",
"beard",
"muscles",
"BMI",
"relative.height",
"body.hair",
"leg.length",
"extraversion",
"agreeableness",
"conscientiousness",
"stability",
"openness"
)

names(promenne)<-jmena

####
cor.results<-array(NA,dim=c(length(jmena),length(jmena)))

colnames(cor.results)<-jmena
rownames(cor.results)<-jmena

cor.pvals<-cor.results
explained.var.trait1<-cor.results
resid.var.trait2<-cor.results

for(sloupec in 1:length(promenne)){
for(radek in 1:length(promenne)){
vysledky<-get.results(promenne[[sloupec]],promenne[[radek]])
cor.results[radek,sloupec]<-vysledky[1]
cor.pvals[radek,sloupec]<-vysledky[2]
explained.var.trait1[radek,sloupec]<-vysledky[3]
resid.var.trait2[radek,sloupec]<-vysledky[4]
print(c(sloupec,radek))
}
}


shared<-(explained.var.trait1-t(resid.var.trait2))
shared2<-(t(explained.var.trait1)-(resid.var.trait2))

shared.total<-(shared+shared2)/2
View(shared.total)


###zapisu nekam ty hotovy veci, pak dodelam dalsi kusy

write.table(cor.results, "cor.results1.txt", sep=";")
write.table(cor.pvals, "cor.pvals.txt", sep=";")
write.table(explained.var.trait1, "explained.var.trait1.txt", sep=";")
write.table(resid.var.trait2, "resid.var.trait2.txt", sep=";")
write.table(shared.total, "shared.total1.txt", sep=";")


###timhle to pak zas nactu podle toho udelej i ostatni podle toho, na kolik kusu se to bude delit
cor.results<-read.table("cor.results1.txt",header=T,sep=";")
cor.pvals<-read.table("cor.pvals.txt",header=T,sep=";")
explained.var.trait1<-read.table("explained.var.trait1.txt",header=T,sep=";")
resid.var.trait2<-read.table("resid.var.trait2.txt",header=T,sep=";")

View(resid.var.trait2)


View(cor.results)
View(cor.pvals)
View(explained.var.trait1)
View(resid.var.trait2)

View(shared.total)

radkuR<-nrow(cor.results)
sloupcuR<-ncol(cor.results)

cor.stars<-ifelse(cor.pvals<0.001,"***",ifelse(cor.pvals<0.01,"**",ifelse(cor.pvals<0.05,"*",ifelse(cor.pvals<0.1,".",""))))

p.info<-paste(unlist(format(round(cor.results,3),nsmall=3)),unlist(cor.stars), sep="")
p.info<-array(p.info,dim=c(radkuR,sloupcuR))
p.info

shared.info<-paste(as.character(format(round(shared.total,2),nsmall=2)),"%")
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

colnames(resultino)<-colnames(cor.stars)
rownames(resultino)<-rownames(cor.stars)

View(resultino)


write.table(resultino, "resultino_shared1.txt", sep=";")

cor.pvals.save<-cor.pvals

toadj<-as.matrix(cor.pvals)
#toadj<-toadj[,-1]

toadj[lower.tri(toadj, diag = T)]<-NA
adjusted<-matrix(p.adjust(toadj,method="BH"),ncol=ncol(toadj))

cor.pvals<-as.data.frame(adjusted)

colnames(cor.pvals)<-colnames(cor.results)
rownames(cor.pvals)<-rownames(cor.results)

cor.stars<-ifelse(cor.pvals<0.001,"***",ifelse(cor.pvals<0.01,"**",ifelse(cor.pvals<0.05,"*",ifelse(cor.pvals<0.1,".",""))))

p.info<-paste(unlist(format(round(cor.results,3),nsmall=3)),unlist(cor.stars), sep="")
p.info<-array(p.info,dim=c(radkuR,sloupcuR))
p.info

shared.info<-paste(as.character(format(round(shared.total,2),nsmall=2)),"%")
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

colnames(resultino)<-colnames(cor.stars)
rownames(resultino)<-rownames(cor.stars)


write.table(resultino, "resultino_shared_adjusted_p.txt", sep=";",col.names=NA)


####Here starts megapermutation of expected shared effect 
#first scramble the data
jmena<-c(
"residence",
"education",
"weight",
"height",
"age.diff",
"attractiveness",
"masculinity",
"eyes",
"hair",
"face.masculinity",
"beard",
"muscles",
"BMI",
"relative.height",
"body.hair",
"leg.length",
"extraversion",
"agreeableness",
"conscientiousness",
"stability",
"openness"
)


getcol<-function(name){which(substr(names(data),1,stri_length(name))==name)}

for(vlastnost in 1:length(jmena)){
sloupce<-getcol(jmena[vlastnost])
vytrh<-data[,sloupce]

kam<-!is.na(vytrh)

napln<-unlist(vytrh)
napln<-napln[!is.na(napln)]
napln<-sample(napln)

pocitadlo<-1

for(i in 1:nrow(vytrh)){
for(j in 1:ncol(vytrh)){
if(kam[i,j]==T){
vytrh[i,j]<-napln[pocitadlo]
pocitadlo<-pocitadlo+1
}
}
}

data[,sloupce]<-vytrh
}


####toto nastavit!!!

runs<-runs.shared.base
runs.pas<-runs.shared.pas


#vector1<-promenne[[1]]
#vector2<-promenne[[2]]

get.results<-function(vector1,vector2){

substrat1<-data[,vector1]
substrat2<-data[,vector2]


znak1<-NA
znak2<-NA
resp<-NA


for(i in 1:nrow(substrat1)){
znak1<-c(znak1,substrat1[i,])
znak2<-c(znak2,substrat2[i,])
resp<-c(resp,rep(i,10))
}

resp

znak1<-as.numeric(znak1)
znak2<-as.numeric(znak2)

znak1
znak2

tabulka<-data.frame(resp,znak1,znak2)

chybi<-1:nrow(tabulka)

for(i in 1:nrow(tabulka)){
chybi[i]<-sum(is.na(tabulka[i,]))
}

tabulka2<-tabulka[chybi<1,]

kolik.part<-1:max(tabulka2$resp)

for(i in 1:max(tabulka2$resp)){
kolik.part[i]<-sum(tabulka2$resp==i)
}

kolik.part

vybrat<-1:nrow(tabulka2)

for(i in 1:nrow(tabulka2)){
vybrat[i]<-sum(tabulka2$resp[i]==which(kolik.part>1))>0
}

vybrat

tabulka3<-tabulka2[vybrat==1,]


###tady uz mam dva vektory znaku ktery muzu pouzit je tam identita respondentu a jsou tam jen taci, ktery maji 2 partnery s informaci od kazdyho znaku

korelo<-cor.test(tabulka3$znak1,tabulka3$znak2)
pval<-as.numeric(korelo[3])
korelace<-as.numeric(korelo[4])

###nejdriv spocitam vsdalenosti a ocekavany vzdalenosti
levels(tabulka3$resp)

respondenti<-as.numeric(levels(as.factor(tabulka3$resp)))
length(respondenti)

resp<-tabulka3$resp
znak1<-tabulka3$znak1
znak2<-tabulka3$znak2

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in respondenti){
radek<-znak1[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

avg.dif.znak1<-mean(diffs)


###permutacni test

perm.difs1<-1:runs
perm.difs2<-1:runs

for(run in 1:runs){

znak1B<-sample(znak1)

dvojice<-data.frame("jeden"=NA,"druhy"=NA)

for(i in respondenti){
radek<-znak1B[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
}
dvojice<-dvojice[-1,]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

perm.difs1[run]<-mean(diffs)



znak2B<-sample(znak2)

dvojice<-data.frame("jeden"=NA,"druhy"=NA)

for(i in respondenti){
radek<-znak2B[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
}
dvojice<-dvojice[-1,]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

perm.difs2[run]<-mean(diffs)

}

(expected.dif1<-mean(perm.difs1))
(expected.dif2<-mean(perm.difs2))

Nresp<-length(respondenti)
Npart<-length(znak1)

###ted uz to smazim tim pasiansem pocitam rezidualni 
###vysvetlovaci kapacitu znaku 2 po vyrovnani znaku 1

effect<-1:runs.pas
effect.resid<-1:runs.pas

for(run.pas in 1:runs.pas){

pruhoz<-znak1
pruhoz.side<-znak2

prvni<-sample(1:length(znak1),1)

rotujici<-znak1[prvni]
rotujici2<-znak2[prvni]

moznosti<-1:length(znak1)

vzdalenost<-avg.dif.znak1

pruhozu<-0
vymen<-0


for(vymena in 1:10000){

vymenit<-sample(moznosti[-prvni],1)

pruhoz2<-pruhoz
pruhoz2[vymenit]<-rotujici

pruhoz.side2<-pruhoz.side
pruhoz.side2[vymenit]<-rotujici2

predlajna<-pruhoz[resp==resp[vymenit]]

polajna<-pruhoz2[resp==resp[vymenit]]

predlajna
polajna

pruhozu<-pruhozu+1

preddvoj<-t(combn(predlajna,2))
preddifer<-mean(abs(preddvoj[,1]-preddvoj[,2]))
preddifer

podvoj<-t(combn(polajna,2))
podifer<-mean(abs(podvoj[,1]-podvoj[,2]))
podifer

if(podifer>preddifer){

rotujici<-pruhoz[vymenit]
pruhoz<-pruhoz2
rotujici2<-pruhoz.side[vymenit]
pruhoz.side<-pruhoz.side2
vzdalenost<-(vzdalenost*Nresp-preddifer+podifer)/Nresp
vymen<-vymen+1

if(vzdalenost>expected.dif1){
effect[run.pas]<-vymen ####t0hle uprav jestli to funguje
pruhoz.side[prvni]<-rotujici2
break
}
}else{
rotujici<-rotujici
pruhoz<-pruhoz
vzdalenost<-vzdalenost
}
}

###tadz spocitam aktualni vzdalenost po prohazeni podle prvni promeny

dvojice<-data.frame("jeden"=NA,"druhy"=NA)
mark<-NA

for(i in respondenti){
radek<-pruhoz.side[resp==i]
pridat<-t(combn(radek,2))
colnames(pridat)<-c("jeden","druhy")
dvojice<-rbind(dvojice,pridat)
mark<-c(mark,rep(i,nrow(pridat)))
}
dvojice<-dvojice[-1,]
mark<-mark[-1]

diffs<-1:length(respondenti)
pocitadlo<-0

for(i in respondenti){
poddvojice<-dvojice[mark==i,]
pocitadlo<-pocitadlo+1
diffs[pocitadlo]<-mean(abs(poddvojice[,1]-poddvojice[,2]))
}

avg.dif.znak2<-mean(diffs)


pruhoz<-pruhoz.side

prvni<-sample(1:length(znak2),1)

rotujici<-pruhoz.side[prvni]

moznosti<-1:length(znak2)

vzdalenost<-avg.dif.znak2

pruhozu<-0
vymen<-0

for(vymena in 1:10000){

vymenit<-sample(moznosti[-prvni],1)

pruhoz2<-pruhoz
pruhoz2[vymenit]<-rotujici

predlajna<-pruhoz[resp==resp[vymenit]]

polajna<-pruhoz2[resp==resp[vymenit]]

predlajna
polajna

pruhozu<-pruhozu+1

preddvoj<-t(combn(predlajna,2))
preddifer<-mean(abs(preddvoj[,1]-preddvoj[,2]))
preddifer

podvoj<-t(combn(polajna,2))
podifer<-mean(abs(podvoj[,1]-podvoj[,2]))
podifer

if(podifer>preddifer){

rotujici<-pruhoz[vymenit]
pruhoz<-pruhoz2
vzdalenost<-(vzdalenost*Nresp-preddifer+podifer)/Nresp
vymen<-vymen+1

if(vzdalenost>expected.dif2){
effect.resid[run.pas]<-vymen ####thle uprav jestli to funguje
break
}
}else{

rotujici<-rotujici
pruhoz<-pruhoz
vzdalenost<-vzdalenost
}
}


}

ef<-100*mean(effect)/Npart
ef.resid<-100*mean(effect.resid)/Npart

return(c(korelace, pval, ef, ef.resid))

}

promenne<-list(
residence<-getcol("residence"),
education<-getcol("education")[-1],
weight<-getcol("weight"),
height<-getcol("height"),
age.diff<-getcol("age.diff"),
attractiveness<-getcol("attractiveness"),
masculinity<-getcol("masculinity"),
eyes<-getcol("eyes"),
hair<-getcol("hair"),
face.masculinity<-getcol("face.masculinity"),
beard<-getcol("beard"),
muscles<-getcol("muscles"),
BMI<-getcol("BMI"),
relative.height<-getcol("relative.height"),
body.hair<-getcol("body.hair"),
leg.length<-getcol("leg.length"),
extraversion<-getcol("extraversion"),
agreeableness<-getcol("agreeableness"),
conscientiousness<-getcol("conscientiousness"),
stability<-getcol("stability"),
openness<-getcol("openness")
)

jmena<-c(
"residence",
"education",
"weight",
"height",
"age.diff",
"attractiveness",
"masculinity",
"eyes",
"hair",
"face.masculinity",
"beard",
"muscles",
"BMI",
"relative.height",
"body.hair",
"leg.length",
"extraversion",
"agreeableness",
"conscientiousness",
"stability",
"openness"
)

names(promenne)<-jmena

####
cor.results<-array(NA,dim=c(length(jmena),length(jmena)))

colnames(cor.results)<-jmena
rownames(cor.results)<-jmena

cor.pvals<-cor.results
explained.var.trait1<-cor.results
resid.var.trait2<-cor.results

for(sloupec in 1:length(promenne)){
for(radek in 1:length(promenne)){
vysledky<-get.results(promenne[[sloupec]],promenne[[radek]])
cor.results[radek,sloupec]<-vysledky[1]
cor.pvals[radek,sloupec]<-vysledky[2]
explained.var.trait1[radek,sloupec]<-vysledky[3]
resid.var.trait2[radek,sloupec]<-vysledky[4]
print(c(sloupec,radek))
}
}


shared<-(explained.var.trait1-t(resid.var.trait2))
shared2<-(t(explained.var.trait1)-(resid.var.trait2))

shared.total<-(shared+shared2)/2
View(shared.total)


###zapisu nekam ty hotovy veci, pak dodelam dalsi kusy

write.table(cor.results, "cor.results1_perm.txt", sep=";")
write.table(cor.pvals, "cor.pvals_perm.txt", sep=";")
write.table(explained.var.trait1, "explained.var.trait1_perm.txt", sep=";")
write.table(resid.var.trait2, "resid.var.trait2_perm.txt", sep=";")
write.table(shared.total, "shared.total1_perm.txt", sep=";")


#Total adjust everything together

cor.pvals<-cor.pvals.save

toadj<-as.matrix(cor.pvals)

toadj[lower.tri(toadj, diag = T)]<-NA


plength<-c(length(pA1),length(pA2),length(pA3),length(pA4),length(pA5),length(pA6),length(unlist(toadj)))
allps<-c(pA1,pA2,pA3,pA4,pA5,pA6,unlist(toadj))
allps<-p.adjust(allps,method="BH")


raw.results[,8]<-allps[1:plength[1]]
raw.results[,12]<-allps[(plength[1]+1):(plength[1]+plength[2])]
raw.results[,17]<-allps[(sum(plength[1:2])+1):(sum(plength[1:3]))]

definitivo[,7]<-round(allps[1:plength[1]],3)
definitivo[,11]<-round(allps[(plength[1]+1):(plength[1]+plength[2])],3)
definitivo[,14]<-round(allps[(sum(plength[1:2])+1):(sum(plength[1:3]))],3)


write.table(definitivo,"results.adjusted_all.txt",sep="\t",col.names=NA)
write.table(raw.results,"raw.results.adjusted_all.txt",sep="\t",col.names=NA)


raw.changes[,11]<-allps[(sum(plength[1:3])+1):(sum(plength[1:4]))]
raw.changes[,17]<-allps[(sum(plength[1:4])+1):(sum(plength[1:5]))]
raw.changes[,23]<-allps[(sum(plength[1:5])+1):(sum(plength[1:6]))]

change.results[,11]<-round(allps[(sum(plength[1:3])+1):(sum(plength[1:4]))],3)
change.results[,17]<-round(allps[(sum(plength[1:4])+1):(sum(plength[1:5]))],3)
change.results[,23]<-round(allps[(sum(plength[1:4])+1):(sum(plength[1:5]))],3)


write.table(change.results,"results.changes.adjusted_all.txt",sep="\t",col.names=NA)
write.table(raw.changes,"raw.results.changes.adjusted_all.txt",sep="\t",col.names=NA)

adjusted<-matrix(allps[(sum(plength[1:6])+1):(sum(plength[1:7]))],ncol=ncol(toadj))

cor.pvals<-as.data.frame(adjusted)

colnames(cor.pvals)<-colnames(cor.results)
rownames(cor.pvals)<-rownames(cor.results)

cor.stars<-ifelse(cor.pvals<0.001,"***",ifelse(cor.pvals<0.01,"**",ifelse(cor.pvals<0.05,"*",ifelse(cor.pvals<0.1,".",""))))

p.info<-paste(unlist(format(round(cor.results,3),nsmall=3)),unlist(cor.stars), sep="")
p.info<-array(p.info,dim=c(radkuR,sloupcuR))
p.info

shared.info<-paste(as.character(format(round(shared.total,2),nsmall=2)),"%")
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

colnames(resultino)<-colnames(cor.stars)
rownames(resultino)<-rownames(cor.stars)

write.table(resultino, "resultino_shared_adjusted_all.txt", sep=";",col.names=NA)





