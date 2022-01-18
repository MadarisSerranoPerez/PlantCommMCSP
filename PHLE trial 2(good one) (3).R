# This code separates the information in Plot_ID and Sample_ID columns into separate labeled columns
# 2021/07/21  Jennifer Jones 

library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)


### data import ---- 


df<-read.csv(file="C:\\Users\\madar\\OneDrive\\Desktop\\PLHE data sheet.xlsx - Sheet1.csv",header=TRUE)
names(df)

attach(df)
hist(PLHE_red1)
hist(PLHE_red2)
hist(PLHE_red3)
hist(PLHE_golden1)
hist(PLHE_golden2)
hist(PLHE_golden3)
hist(Flower_Num)
hist(FC_perc)
hist(GC_perc)


#df<-df[-which(df$Sample_ID_MCSP3 == ""), ] #deletes all rows that have blank values
names(df)

head(df$Plot_ID)
head(df$Sample_ID)

df<- df %>% 
  mutate(Sample_ID1 = Sample_ID) %>% #copy data to a new column
  separate(Sample_ID1,c("Series_ID","ID_number"),"_") %>% #separate column text into two columns
  mutate(Plot_ID1 = Plot_ID) %>% # copy data to a new column
  separate(Plot_ID1, c("Treatment","Replicate","Footprint","Subplot","e"),"_") %>% # separate column text into 5 columns
  separate(e, c("Footprint_location","Subplot_location"),1) # further separate the footprint/subplot location into 2 columns 

names(df)
df[,c(2:3,13:20)] <- lapply(df[c(2:3,13:20)], factor) # reclassify 10 columns as factors.

### PLHE means ----
df<- df %>% 
  mutate(PLHE.meanred=(PLHE_red1+PLHE_red2+PLHE_red3)/3) %>%
  mutate(PLHE.meangold=(PLHE_golden1+PLHE_golden2+PLHE_golden3)/3)%>%
  mutate(FlowerNum.log=log(Flower_Num))%>%
  mutate(FlowerCover.log=log(FC_perc)) %>%
  mutate(GroundCover.log=log(GC_perc+0.1))
  

  
hist(log(Flower_Num))
hist(log(FC_perc))
hist(df$GroundCover.log)





### Red Clover ----
#linear mixed effects model with Replicate as a random effect
fm2 <- lmer(PLHE.meanred~Footprint*Subplot*Date + (1|Replicate), data = df, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "pink")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Red Clover Height (cm)")
p



attach(df)
boxplot(PLHE.meanred ~ Footprint*Subplot)
boxplot(PLHE.meanred ~ Subplot)
boxplot(PLHE.meanred ~Replicate)

boxplot(PLHE.meangold~ Footprint*Subplot)
boxplot(PLHE.meangold ~ Subplot)
boxplot(PLHE.meangold ~Replicate)


boxplot(FlowerCover.log ~ Footprint*Subplot)
boxplot(FlowerCover.log ~ Subplot)
boxplot(FlowerCover.log ~Replicate)

boxplot(GroundCover.log ~ Footprint*Subplot)
boxplot(GroundCover.log ~ Subplot)
boxplot(GroundCover.log ~Replicate)
 
### Goldenrod----
fm2 <- lmer(PLHE.meangold~Footprint*Subplot + (1|Replicate), data = df, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction

#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "orange")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Goldenrod Height (cm)")
p

attach(df)
boxplot(PLHE.meangold ~ Footprint*Subplot)
boxplot(PLHE.meangold ~ Subplot)
boxplot(PLHE.meangold ~Replicate)


###FC and GC---- 
#Flower
fm2 <- lmer(FlowerCover.log~Footprint*Subplot + (1|Replicate), data = df, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Flower Cover")
p

attach(df)
boxplot(FlowerCover.log ~ Footprint*Subplot)
boxplot(FlowerCover.log ~ Subplot)
boxplot(FlowerCover.log ~ Replicate)

#Ground
fm2 <- lmer(GroundCover.log~Footprint*Subplot + (1|Replicate), data = df, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Groud Cover")
p


attach(df)
boxplot(GroundCover.log ~ Footprint*Subplot)
boxplot(GroundCover.log ~ Subplot)
boxplot(GroundCover.log ~Replicate)





# type 3 anova with no mixed effects REDCLOVER, GOLDEN, FC AND GC
#RED
aov1<-aov(PLHE.meanred~Footprint*Subplot)
Anova(aov1,type="III")
TukeyHSD(aov1)
#GOLDEN
aov1<-aov(PLHE.meangold~Footprint*Subplot)
Anova(aov1,type="III")
TukeyHSD(aov1)
#FC
aov1<-aov(FlowerCover.log~Footprint*Subplot)
Anova(aov1,type="III")
TukeyHSD(aov1)
#GC
aov1<-aov(GroundCover.log~Footprint*Subplot)
Anova(aov1,type="III")
TukeyHSD(aov1)

####

#makes a factor variable out of date
df$Date<-as.factor(ifelse(df$Date=="7/7/2021","cen1","cen2"))
summary(df$Date)

#separating the different times into different columns
df.wide<- df %>% 
  select(Date,Plot_ID,Treatment:GroundCover.log) %>% # select columns that we want for analysis later
  # Need to get rid of all of the references to sample number, because that will mess up the rest of the code
  gather(variable,value,-(Date:Subplot_location)) %>%
  spread(Date,value) %>%
  mutate(difference = cen2-cen1) %>% # calculates the difference between 2nd and first values
  mutate(change_scaled = (cen2-cen1)/cen1) # scaled change based on the initial value

names(df.wide)
df.wide <-df.wide %>%
  gather(summary.var,value,-(Plot_ID:variable)) %>%
  unite(new_variable,variable,summary.var) %>%
  spread(new_variable,value)

names(df.wide)


#Flower
fm2 <- lmer(FlowerCover.log_difference ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Flower Cover")
p

#Ground
fm2 <- lmer(GroundCover.log_difference ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "orange")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Ground Cover")
p

#PLHE golden
fm2 <- lmer(PLHE.meangold_difference~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Godlenrod Height (cm)")
p

#PLHE Red

fm2 <- lmer(PLHE.meanred_difference ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "pink")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Red Clover Height (cm)")
p


#CENSUS 1

#Flower
fm2 <- lmer(FlowerCover.log_cen1 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Flower Cover")
p

#Ground
fm2 <- lmer(GroundCover.log_cen1 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "orange")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Ground Cover")
p
#PLHE Golden
fm2 <- lmer(PLHE.meangold_cen1~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Godlenrod Height (cm)")
p

#PLHE Red 
fm2 <- lmer(PLHE.meanred_cen1 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "pink")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Red Clover Height (cm)")
p


####Cen2
#Flower
fm2 <- lmer(FlowerCover.log_cen2 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "blue")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Flower Cover")
p

#Ground
fm2 <- lmer(GroundCover.log_cen2 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE,na.action = na.fail)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "orange")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Ground Cover")
p
#PLHE Golden
fm2 <- lmer(PLHE.meangold_cen2~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "purple")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Godlenrod Height (cm)")
p

#PLHE Red 
fm2 <- lmer(PLHE.meanred_cen2 ~Footprint*Subplot + (1|Replicate), data = df.wide, REML=FALSE)
anova(fm2) #Satterthwaite's method
summary(fm2) #Satterthwaite's method
Anova(fm2,type='III') # Wald chisquare test
emmeans(fm2, pairwise ~ Footprint*Subplot, adjust = "tukey")
# pairwise comparison, this is how you interpret the interaction
####PLANT HEIGH RED CLOVER GRAPH---- 
#this code saves the output from the emmeans command into a new dataframe that you can use for the graph
emmeans.graph<-data.frame(emmeans(fm2,pairwise~Footprint*Subplot)$emmeans)

# example graph code with eemeans output
library(ggplot2)
col1<-c("#66C2A5","brown4", "pink")
pd <- position_dodge(.9) ### can be .1 to .3 depending on how many columns there are
p <- ggplot(emmeans.graph, aes(x=Footprint, y=emmean, fill=Subplot))
p <- p + geom_bar(position = pd, stat="identity")
p <- p + geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), size = 1, width=.5, position=pd)
p <- p + scale_x_discrete(breaks=c("D1","IR", "VR"),labels=c("drought","irrigated", "variable"))
p <- p + scale_fill_manual("Subplot", values = col1,breaks=c("C","N", "F"),labels=c("control","nematicide", "fungicide"))
p <- p + theme_classic()
#p <- p + ylim(0,7.4)
p <- p + theme(axis.text.x = element_text(colour="black",size=15),
               axis.text.y = element_text(colour="black",size=10,angle=90,hjust=.5),
               axis.title.y = element_text(colour="black",size=15),
               axis.title.x = element_text(colour="black",size=15),
               legend.title=element_text(size=15),legend.text=element_text(size=15))
p <- p + ylab("Red Clover Height (cm)")
p


###CN 2








####Do this analyses with each column, priotritizing change difference, census 1, and then do the others 
