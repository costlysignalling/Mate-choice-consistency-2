library(stringi)
library(lmerTest)

data<-read.delim("raw.data.txt",stringsAsFactors=F)

data$ID<-paste("ID",1:nrow(data),sep="")

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


summary(data)



###zacinaj.permutace

runs<-5
runs.pas<-3


get.result.line<-function(vector){

substrat<-data[,vector]

partneru<-1:nrow(substrat)

IDs<-data$ID

for(i in 1:nrow(substrat)){
partneru[i]<-(10-sum(is.na(substrat[i,])))
}

partneru

IDs<-IDs[partneru>1]
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

return(list(vysledky,perm.difs,raw,IDs))
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

usedID<-c(
residenceR[[4]],
educationR[[4]],
weightR[[4]],
heightR[[4]],
age.diffR[[4]],
attractivenessR[[4]],
masculinityR[[4]],
eyesR[[4]],
hairR[[4]],
face.masculinityR[[4]],
beardR[[4]],
musclesR[[4]],
BMIR[[4]],
relative.heightR[[4]],
body.hairR[[4]],
leg.lengthR[[4]],
extraversionR[[4]],
agreeablenessR[[4]],
conscientiousnessR[[4]],
stabilityR[[4]],
opennessR[[4]]
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

ef.size<-cbind(simp.r,foc,ef.pas)
p.vals<-cbind(p1,p2,p3)

cor(ef.size)
cor(p.vals)

model<-lm((foc*100)~ef.pas)
model

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


compare<-lm(ef.pas~variable.type)
summary(compare)
TukeyHSD(aov(compare))

compare<-lm(foc~variable.type)
summary(compare)
TukeyHSD(aov(compare))

compare<-lm(simp.r~variable.type)
summary(compare)
TukeyHSD(aov(compare))

p1<-p.adjust(p1,method="BH")
p2<-p.adjust(p2,method="BH")
p3<-p.adjust(p3,method="BH")

raw.results[,8]<-p1
raw.results[,12]<-p2
raw.results[,17]<-p3


definitivo[,7]<-round(p1,3)
definitivo[,11]<-round(p2,3)
definitivo[,14]<-round(p3,3)


#write.table(definitivo,"results.adjusted.p.txt",sep="\t",col.names=NA)
#write.table(all.perms,"all.perms.txt",sep="\t")
#write.table(raw.results,"raw.results.adjusted.p.txt",sep="\t",col.names=NA)

summary(usedID)



