block01 <- read.csv("Exp3-block1.csv")

block01 <- aggregate(block01[,c(1:28)],list(block01$step,block01$Context),sum)
block01$step_num <- c(11:1)
library(reshape2)
long <- melt(block01,id.vars = c("step_num","Group.2"),value.name = "T4_count",variable.name = "Subject")
names(long)[1] <- "step"
names(long)[2] <- "Context"
long$T4_percent <- long$T4_count/7
block01_long<- long



block02 <- read.csv("Exp3-block2.csv")
block02 <- aggregate(block02[,c(1:28)],list(block02$step,block02$Context),sum)
block02$step_num <- c(11:1)
library(reshape2)
long <- melt(block02,value.name = "T4_count",variable.name = "Subject",id.vars = c("step_num","Group.2"))
names(long)[1] <- "step"
names(long)[2] <- "Context"
long$T4_percent <- long$T4_count/7
block02_long<- long

block12 <- rbind(block01_long,block02_long)
write.csv(block12,"Exp3-responses.csv")
#draw Figure 11

block12<- read.csv("Exp3-block12-responses.csv")
bardata <- summarySE(block12,measurevar = "T4_percent",groupvars = c("step","Context"),na.rm = T)
bardata$Context_tone <- ifelse(bardata$Context=="Tone1","Tone 1 (high-level)",ifelse(bardata$Context=="Tone2","Tone 2 (rising)",ifelse(bardata$Context=="Tone4","Tone 4 (falling)","No context")))
ggplot(bardata,aes(x=step,y=T4_percent,group=Context_tone))+geom_point(aes(shape=Context_tone)) +geom_line(aes(linetype=Context_tone))+geom_errorbar(aes(ymin=T4_percent-se, ymax=T4_percent+se), width=1,position=position_dodge(0)) +scale_x_discrete(labels=c(1:14))+ylab("Tone 4 responses")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.text.x = element_text(size = 17,face = "bold"),strip.text.y = element_text(size=17,face="bold"), axis.title = element_text(size=17,face="bold"),axis.text = element_text(size = 17), legend.title = element_text(size=12,face="bold"),legend.text = element_text(size=9,face = "bold"), legend.position = "bottom")+coord_fixed(8/1)+xlab("Step")


#obtain the values of boundary position using probit analysis
library(MASS)
bdata <- as.data.frame(sapply(block01[3:30],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=block01)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}))
names(bdata) <- "boundary"
bdata$subject <- substr(row.names(bdata),1,nchar(row.names(bdata))-9)
bdata$context <- "No context"

#the probic analysis generated values of boundary position less than 1, but this is impossible. Therefore, we standardize the values less than 1 to 1.
bdata$std_b <- ifelse(bdata$boundary>=1,bdata$boundary,1)
bdata_block01 <- bdata

stepdata_tone1 <- block02[1:11,]
stepdata_tone2 <- block02[12:22,]
stepdata_tone4 <- block02[23:33,]

bdata <- as.data.frame(sapply(stepdata_tone1[3:30],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=stepdata_tone1)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}),col.names="boundary")

names(bdata) <- "boundary"
bdata$subject <- substr(row.names(bdata),1,nchar(row.names(bdata))-9)
bdata$context <- "Tone 1"
bdata_block02.1 <- bdata

bdata <- as.data.frame(sapply(stepdata_tone2[3:30],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=stepdata_tone2)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}),col.names="boundary")

names(bdata) <- "boundary"
bdata$subject <- substr(row.names(bdata),1,nchar(row.names(bdata))-9)
bdata$context <- "Tone 2"

bdata_block02.2 <- bdata

bdata <- as.data.frame(sapply(stepdata_tone4[3:30],function(x){
  glm.logit <- glm(cbind(as.numeric(as.character(x)),7-as.numeric(as.character(x))) ~ step_num, family=binomial('probit'), data=stepdata_tone4)
  dose.p(glm.logit,p=seq(0.5,0.5,0.1))
}),col.names="boundary")

names(bdata) <- "boundary"
bdata$subject <- substr(row.names(bdata),1,nchar(row.names(bdata))-9)
bdata$context <- "Tone 4"

bdata_block02.3 <- bdata

bdata_block02 <- rbind(bdata_block02.1,bdata_block02.2,bdata_block02.3)
#the probic analysis generated values of boundary position less than 1, but this is impossible. Therefore, we standardize the values less than 1 to 1.
bdata_block02$std_b <- ifelse(bdata_block02$boundary>11,11,ifelse(bdata_block02$boundary<1,1,bdata_block02$boundary))
total <- rbind(bdata_block01,bdata_block02)

write.csv(total,"Exp3-block12-boundary.csv",row.names = F)


block12 <- read.csv("Exp3-block12-boundary.csv")
bardata <- summarySE(block12,measurevar = "std_b",groupvars = c("context"))
ggplot(bardata,aes(x=context,y=std_b))+geom_bar(stat = "identity",alpha=0.3)+geom_errorbar(aes(ymin=std_b-se, ymax=std_b+se), width=0.7,position=position_dodge(0))+geom_jitter(data=block12,size=0.1, width = 0.08)+geom_violin(data = block12,fill=NA)+scale_y_continuous("Boundary position", limits=c(0,12))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title= element_blank(), legend.position="none", axis.text = element_text(size=15),        axis.title = element_text(size=17,face="bold"))

model <- lmer(std_b~context+(1|subject),data=block12)
summary(model)
block12$context <- as.factor(block12$context)
block12$context <- relevel(block12$context,ref = "Tone 2")
