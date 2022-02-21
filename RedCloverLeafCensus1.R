# This code separates the information in Plot_ID and Sample_ID columns into separate labeled columns
# 2021/07/21  Jennifer Jones 

library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)


df<-read.csv(file="C:\\Users\\madar\\OneDrive\\Documents\\PlantCommMCSP1\\RedLeaf_2022.csv",header=TRUE)
names(df)

#df<-df[-which(df$Sample_ID_MCSP3 == ""), ] #deletes all rows that have blank values
names(df)
names(df)[1]<- 'Plot_ID'

head(df$Plot_ID)
head(df$Sample_ID)

df<- df %>% 
  mutate(Sample_ID1 = Sample_ID_MCSP3) %>% #copy data to a new column
  separate(Sample_ID1,c("Series_ID","ID_number","Replicate_Num"),"_") %>% #separate column text into two columns
  mutate(Plot_ID1 = Plot_ID) %>% # copy data to a new column
  separate(Plot_ID1, c("Treatment","Replicate","Footprint","Subplot","e"),"_") %>% # separate column text into 5 columns
  separate(e, c("Footprint_location","Subplot_location"),1) # further separate the footprint/subplot location into 2 columns 

names(df)
df[,9:17] <- lapply(df[,9:17], factor) # reclassify 10 columns as factors.

df<- df %>% 
  mutate(SLA = LA/LW_dry) %>% 
  mutate(LDMC = LW_dry/LW_fresh) %>%
  select(-Sample_ID_MCSP3)

df.mean<-aggregate(x = df[,c(3:6,17:18)], by = list(df$ID_number), FUN = "mean")
colnames(df.mean) <- paste(colnames(df.mean),"mean",sep=".")
names(df.mean)[1] <-"ID_number"

df.cut<-df %>%
  subset(Replicate_Num=="1r") %>%
  select(Plot_ID:Date,Series_ID:Subplot_location)

df.mean<-merge(df.mean,df.cut,by="ID_number")
df.mean<-subset(df.mean, Date=="7/13/2021")
hist(df.mean$LDMC.mean)
hist(df.mean$SLA.mean)
#this is time one, for time two create a different script and change the date 

#linear mixed effects model with Replicate as a random effect
fm2 <- lmer(SLA.mean~Footprint*Subplot + (1|Replicate), data = df.mean, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction

#grafica
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)
emmeans.graph

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F", "S"),labels=c("control","nematicide", "fungicide", "sorghum"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Specific Leaf Area")
p

attach(df.mean)
boxplot(SLA.mean ~ Footprint*Subplot)
boxplot(SLA.mean ~ Subplot)
boxplot(SLA.mean ~Replicate)





## LDMC

#linear mixed effects model with Replicate as a random effect
fm2 <- lmer(LDMC.mean~Footprint*Subplot + (1|Replicate), data = df.mean, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint, adjust = "tukey")
emmeans(fm2, pairwise ~ Subplot, adjust = "tukey")
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction

#grafica
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F", "S"),labels=c("control","nematicide", "fungicide", "sorghum"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Leaf Dry Matter Content")
p



attach(df.mean)
boxplot(LDMC.mean ~Footprint*Subplot)
boxplot(LDMC.mean ~ Subplot)
boxplot(LDMC.mean ~Replicate)

# type 3 anova with no mixed effects
aov1<-aov(LDMC.mean~Footprint*Subplot)
Anova(aov1,type="III")
TukeyHSD(aov1)

attach(df)
hist(LDMC.mean)

