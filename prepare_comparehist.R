library(stringi)
library(lmerTest)


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

runs<-5

getcol<-function(name){which(substr(names(data),1,stri_length(name))==name)}


#take presence of children with given partner as a filter
cols.filter<-getcol("Nkids")
filter<-data[,cols.filter]
filter<-ifelse(is.na(filter),F,ifelse(filter>0,T,F))


get.groups<-function(vector){

orig.sample<-data[,vector]


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


raw<-data.frame(
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

names(raw)<-c("mean non-father","mean father","intercept non-father","effect father","std. err.","p-val. of fixed effect","mean res. non-father","mean res. father","res. intercept non-father","res. effect father","res. std. err.","res. p-val. of fixed effect")

return(list(raw,fathers,nonfathers))
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

residenceR<-get.groups(residence)
educationR<-get.groups(education)
weightR<-get.groups(weight)
heightR<-get.groups(height)
age.diffR<-get.groups(age.diff)
attractivenessR<-get.groups(attractiveness)
masculinityR<-get.groups(masculinity)
eyesR<-get.groups(eyes)
hairR<-get.groups(hair)
face.masculinityR<-get.groups(face.masculinity)
beardR<-get.groups(beard)
musclesR<-get.groups(muscles)
BMIR<-get.groups(BMI)
relative.heightR<-get.groups(relative.height)
body.hairR<-get.groups(body.hair)
leg.lengthR<-get.groups(leg.length)
extraversionR<-get.groups(extraversion)
agreeablenessR<-get.groups(agreeableness)
conscientiousnessR<-get.groups(conscientiousness)
stabilityR<-get.groups(stability)
opennessR<-get.groups(openness)



mean.results<-list(
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

father.base<-list(
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

nonfather.base<-list(
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

var.names<-c(
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



