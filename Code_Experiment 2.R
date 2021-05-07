library(reshape2)
library(ggplot2)
library(stringr)

block_01<- read.csv("Exp2-block1.csv")
block_02<- read.csv("Exp2-block2.csv")
block_03<- read.csv("Exp2-block3.csv")
block_01 <- aggregate(block_01[,c(3:35)],list(block_01$step,block_01$Context.tone),sum)
names(block_01)[c(1,2)] <- c("step","Context tone")
block_02 <- aggregate(block_02[,c(3:35)],list(block_02$step,block_02$Context.tone),sum)
names(block_02)[c(1,2)] <- c("step","Context tone")
block_03 <- aggregate(block_03[,c(3:35)],list(block_03$step,block_03$Context.tone),sum)
names(block_03)[c(1,2)] <- c("step","Context tone")
block_01.1 <- cbind(block_01,Speechness="Speech")
block_01.2 <- cbind(block_01,Speechness="Nonspeech")
block_02$Speechness <- "Nonspeech"
block_03$Speechness <- "Speech"
block_01.1_long <- melt(block_01.1,id.vars = c("step","Context tone","Speechness"),variable.name = "subject",value.name = "T4_count")
block_01.2_long<- melt(block_01.2,id.vars = c("step","Context tone","Speechness"),variable.name = "subject",value.name = "T4_count")
block_02_long <- melt(block_02,id.vars = c("step","Context tone","Speechness"),variable.name = "subject",value.name = "T4_count")
block_03_long <- melt(block_03,id.vars = c("step","Context tone","Speechness"),variable.name = "subject",value.name = "T4_count")


block123 <- rbind(block_01.1_long,block_01.2_long,block_02_long,block_03_long)
write.csv(block123,"Exp2-block123-responses.csv")

##Figure 9
block123 <- read.csv("Exp2-block123-responses.csv")
block123$T4_percent <- block123$T4_count/7
bardata <- summarySE(block123,measurevar = "T4_percent",groupvars = c("step","Context.tone","Speechness"),na.rm = T)
bardata$Context_tone <- ifelse(bardata$Context.tone=="Tone 1","Tone 1 (high-level)",ifelse(bardata$Context.tone=="Tone 2","Tone 2 (rising)",ifelse(bardata$Context.tone=="Tone 4","Tone 4 (falling)","No context")))
ggplot(bardata,aes(x=step,y=T4_percent,group=Context_tone))+geom_point(aes(shape=Context_tone)) +geom_line(aes(linetype=Context_tone))+geom_errorbar(aes(ymin=T4_percent-se, ymax=T4_percent+se), width=1,position=position_dodge(0)) +scale_x_discrete(labels=c(1:14))+facet_grid(.~Speechness)+ylab("Tone 4 responses")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.text.x = element_text(size = 17,face = "bold"),strip.text.y = element_text(size=17,face="bold"), axis.title = element_text(size=17,face="bold"),axis.text = element_text(size = 17), legend.title = element_text(size=16,face="bold"),legend.text = element_text(size=15,face = "bold"), legend.position = "bottom")+coord_fixed(8/1)+xlab("Step")



# obtain the values of boundary position using probit analysis
library(MASS)
block_01$step_num <- as.numeric(substr(as.character(block_01$step),5,6))
boundarydata_01 <- as.data.frame(sapply(block_01[3:35],function(x){
glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_01)
dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_01) <- "boundary"
boundarydata_01$subject <- substr(row.names(boundarydata_01),1,4)
boundarydata_01$Context <- "No context"
boundarydata_01$Speechness <- "Neutral"
# # 

block_02$step_num <- as.numeric(substr(as.character(block_02$step),5,6))
block_02.1 <- block_02[c(1:14),]
block_02.2 <- block_02[c(15:28),]
block_02.3 <- block_02[c(29:42),]
boundarydata_02.1 <- as.data.frame(sapply(block_02.1[3:35],function(x){
glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_02.1)
dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_02.1) <- "boundary"
boundarydata_02.1$subject <- substr(row.names(boundarydata_02.1),1,4)
boundarydata_02.1$Context <- "Tone 1"
boundarydata_02.1$Speechness <- "Nonspeech"

boundarydata_02.2 <- as.data.frame(sapply(block_02.2[3:35],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_02.2)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_02.2) <- "boundary"
boundarydata_02.2$subject <- substr(row.names(boundarydata_02.2),1,4)
boundarydata_02.2$Context <- "Tone 2"
boundarydata_02.2$Speechness <- "Nonspeech"
boundarydata_02.3 <- as.data.frame(sapply(block_02.3[3:35],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_02.3)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_02.3) <- "boundary"
boundarydata_02.3$subject <- substr(row.names(boundarydata_02.3),1,4)
boundarydata_02.3$Context <- "Tone 4"
boundarydata_02.3$Speechness <- "Nonspeech"

block_03$step_num <- as.numeric(substr(as.character(block_03$step),5,6))
block_03.1 <- block_03[c(1:14),]
block_03.2 <- block_03[c(15:28),]
block_03.3 <- block_03[c(29:42),]
boundarydata_03.1 <- as.data.frame(sapply(block_03.1[3:35],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_03.1)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_03.1) <- "boundary"
boundarydata_03.1$subject <- substr(row.names(boundarydata_03.1),1,4)
boundarydata_03.1$Context <- "Tone 1"
boundarydata_03.1$Speechness <- "Speech"
boundarydata_03.2 <- as.data.frame(sapply(block_03.2[3:35],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_03.2)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_03.2) <- "boundary"
boundarydata_03.2$subject <- substr(row.names(boundarydata_03.2),1,4)
boundarydata_03.2$Context <- "Tone 2"
boundarydata_03.2$Speechness <- "Speech"
boundarydata_03.3 <- as.data.frame(sapply(block_03.3[3:35],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block_03.3)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(boundarydata_03.3) <- "boundary"
boundarydata_03.3$subject <- substr(row.names(boundarydata_03.3),1,4)
boundarydata_03.3$Context <- "Tone 4"
boundarydata_03.3$Speechness <- "Speech"
total_boundary <- rbind(boundarydata_01,boundarydata_02.1,boundarydata_02.2,boundarydata_02.3,boundarydata_03.1,boundarydata_03.2,boundarydata_03.3)
#the probic analysis generated values of boundary position less than 1, but this is impossible. Therefore, we standardize the values less than 1 to 1.
total_boundary$stdize_b <- ifelse(total_boundary$boundary>=1, total_boundary$boundary,1)

write.csv(total_boundary,"Exp2-block123-boundary.csv",row.names = F)

##obtain Figure 10
total_boundary <- read.csv("Exp2-block123-boundary.csv")
bardata <- summarySE(total_boundary,measurevar = "stdize_b",groupvars = c("Context","Speechness"))
bardata$Speechness <- as.factor(bardata$Speechness)
ggplot(data=bardata,aes(x = Context, y =stdize_b)) +  labs(y="Boundary position")+
  geom_jitter(data=total_boundary,size=0.1, width = 0.08)+geom_violin(data = total_boundary,fill=NA)+
  geom_bar(data=bardata,position=position_dodge(),stat = "identity",fill="black",alpha=0.3)+geom_errorbar(data=bardata,aes(ymin=stdize_b-se, ymax=stdize_b+se), width=0.9,position=position_dodge())+
  scale_y_continuous("Boundary position", limits=c(0,15))+
  facet_grid(.~Speechness,scales = "free",space = "free_x")+theme_bw()+
  #theme_ipsum()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title= element_blank(), legend.position="none", axis.text = element_text(size=17),        axis.title = element_text(size=17,face="bold"),         strip.text.x = element_text(size = 17,face="bold"), strip.text.y = element_text(size = 17,face="bold"))+theme(panel.spacing.x=unit(1, "lines"),panel.spacing.y=unit(1, "lines"))

#statistics
total_boundary$levels <- str_c(total_boundary$Context,total_boundary$Speechness,sep = "-")
model <- lmer(stdize_b~levels+(1|subject),data=total_boundary)
summary(model)
