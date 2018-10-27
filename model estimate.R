results<-read.delim("raw.results.txt")

View(results)

ef.pas<-results$effect.size
foc<-results$focal.person.var

model<-lm((foc*100)~ef.pas)
model

sumaz<-summary(model)

inter<-sumaz[[4]][1,1]
stder<-sumaz[[4]][1,2]

inter+stder*1.96
inter-stder*1.96


inter<-sumaz[[4]][2,1]
stder<-sumaz[[4]][2,2]

inter+stder*1.96
inter-stder*1.96
