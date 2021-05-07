
library(lme4)
library(lmerTest)
library(reshape2)
library(plyr)
library(stringr)
library(ggplot2)



											### This section is for obtaining Figure 7###
# open the raw data
totaldata <- read.csv("Exp1-responses.csv")
totaldata$Attention_status <- revalue(totaldata$Attention_status,c("No-attention"="Without attentional task on precursor","Attention"="With attentional task on precursor"))
totaldata$step_num <- substr(totaldata$step,5,nchar(totaldata$step))
# Prepare the data for Figure 7
bardata <- summarySE(totaldata,measurevar = "T4_percent",groupvars = c("step","Context_type","Tone","Attention_status"))
bardata$Context_tone <- ifelse(bardata$Tone=="Tone1","Tone 1 (high-level)",ifelse(bardata$Tone=="Tone2","Tone 2 (rising)","Tone 4 (falling)"))
# Obtain Figure 7
ggplot(bardata,aes(x=step,y=T4_percent,group=Context_tone))+geom_point(aes(shape=Context_tone)) +geom_line(aes(linetype=Context_tone))+geom_errorbar(aes(ymin=T4_percent-se, ymax=T4_percent+se), width=1,position=position_dodge(0)) +scale_x_discrete(labels=c(1:14))+facet_grid(Context_type~Attention_status)+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.grid = element_blank(),strip.text.x = element_text(size = 13,face = "bold"), strip.text.y = element_text(size=13,face = "bold"),axis.title.x =element_text(size = 13,face="bold"),axis.title.y = element_text(size = 13,face = "bold"),axis.text = element_text(size=13), legend.title = element_text(size=13,face = "bold"),legend.text = element_text(size = 13,face = "bold"),legend.position = "bottom")+ylab("Tone 4 responses")+coord_fixed(8/1)+xlab("Step")


												###This Section is for Figure 8 and statistics###
#The values of boundary position were obtained by the probit analysis in SPSS
boundarydata <- read.csv("Exp1-boundary.csv")
#Prepare the data for Figure 8
errorbar <- summarySE(data=boundarydata, measurevar="Boundary", groupvars=c("Context","Speechness","attention_stats"))
errorbar$Attention <- ifelse(errorbar$attention_stats=="Attention","With attentional task on precursor","No attentional task on precursor")
plotdata <- aggregate(boundarydata[,5],list(boundarydata$Context,boundarydata$Speechness,boundarydata$attention_stats,boundarydata$Subject),mean)
names(plotdata) <- c("Context","Speechness","Attention","Subject","Boundary")
plotdata$Attention <- ifelse(plotdata$Attention=="Attention","With attentional task on precursor","No attentional task on precursor")


ggplot(data=errorbar,aes(x = Context, y =Boundary)) +
geom_jitter(data=plotdata,size=0.1, width = 0.08)+geom_violin(data = plotdata,fill=NA)+
geom_bar(data=errorbar,position=position_dodge(),stat = "identity",fill="black",alpha=0.2)+geom_errorbar(data=errorbar,aes(ymin=Boundary-se, ymax=Boundary+se), width=0.9,position=position_dodge())+
  scale_y_continuous("Boundary position", limits=c(0,15))+
facet_grid(Speechness~Attention)+
  #theme_ipsum()+
  theme( panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title= element_blank(), legend.position="bottom",legend.text = element_text(size=12, face="bold"),        axis.text.y = element_text(size=12,face="bold"), axis.text.x = element_text(size=12,face="bold"),        axis.title.y = element_text(size=12,face="bold"), axis.title.x=element_text(size=0,face="bold"),        strip.text.x = element_text(size = 12,face="bold"), strip.text.y = element_text(size = 12,face="bold"), legend.key = element_rect(colour = "black"), panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1, "lines"))
  
# statistics
# sum coding
# boundarydata$Speechness_num <- ifelse(boundarydata$Speechness=="Speech",1,-1)
# boundarydata$Attention_num <- ifelse(boundarydata$attention_stats=="Attention",1,-1)
# boundarydata$Context_num <- ifelse(boundarydata$Context=="Tone 1",-1,ifelse(boundarydata$Context=="Tone 2",0,1))
# boundary.m <- lmer(Boundary~Context_num*Speechness_num*Attention_num+(1+Context_num+Speechness_num+Attention_num|Subject),data=boundarydata,REML = FALSE)
# summary(boundary.m)


##dummy coding
boundarydata$Context <- as.factor(boundarydata$Context)
boundarydata$attention_stats <- as.factor(boundarydata$attention_stats)
boundarydata$Speechness <- as.factor(boundarydata$Speechness)
boundary.m <- lmer(Boundary~Context*Speechness*attention_stats+(1+Context+Speechness+attention_stats+Context:Speechness|Subject),data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
step(boundary.m)
optimal <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data = boundarydata,REML = F)




#reference is nonspeech, Tone 1, With attentional task
boundarydata$attention_stats <- relevel(boundarydata$attention_stats,ref = "Attention")
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 1")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone1 <- lmer(Boundary~Context*Speechness*attention_stats+(1+Context+Speechness+attention_stats+Context:Speechness|Subject),data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone1)

#change reference to nonspeech, Tone 2, With attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 2")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone2 <- lmer(Boundary~Context*Speechness*attention_stats+(1+Context+Speechness+attention_stats+Context:Speechness|Subject),data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone2)

#change reference to nonspeech, Tone 4, With attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 4")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone4 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone4)

#change reference to speech, Tone 1, With attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 1")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone1 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone1)

#change reference to speech, Tone 2, With attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 2")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone2 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone2)


#change reference to speech, Tone 4, With attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 4")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone4 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone4)


#reference is nonspeech, Tone 1, and No attentional task
boundarydata$attention_stats <- relevel(boundarydata$attention_stats,ref = "No_attention")
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 1")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone1 <- lmer(Boundary~Context*Speechness*attention_stats+(1+Context+Speechness+attention_stats+Context:Speechness|Subject),data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone1)
#change reference to nonspeech, Tone 2, and No attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 2")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone2 <- lmer(Boundary~Context*Speechness*attention_stats+(1+Context+Speechness+attention_stats+Context:Speechness|Subject),data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone2)

#change reference to nonspeech, Tone 4, and No attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 4")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Nonspeech")
boundary.optimal.non.tone4 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.non.tone4)

#change reference to speech, Tone 1, and No attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 1")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone1 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone1)

#change reference to speech, Tone 2, and No attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 2")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone2 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone2)


#change reference to speech, Tone 4, and No attentional task
boundarydata$Context <- relevel(boundarydata$Context,ref = "Tone 4")
boundarydata$Speechness <- relevel(boundarydata$Speechness,ref = "Speech")
boundary.optimal.speech.tone4 <- lmer(Boundary ~ Context + Speechness + attention_stats + (Context + Speechness | Subject) + Context:Speechness + Context:attention_stats + Speechness:attention_stats + Context:Speechness:attention_stats,data=boundarydata,REML=F,control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(boundary.optimal.speech.tone4)

