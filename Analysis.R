library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lme4)
library(sjPlot)
library(performance)
library(ggsignif)
library(ggpubr)
library(nortest)
library(EnvStats) 
library(fitdistrplus)

#Aggressive vocalizations during intergroup interactions in roost 
#defense in the Spix's disk-winged bat. Silvia Chaves-Ramírez et al.


# data 1, Checking roost and Enter-exist roost 
data1 <- read.csv("data1.csv")
head(data1)
data1
#Variables as a factor for the models 

data1$Type_interaction <-as.factor(data1$Type_interaction)
class(data1$Type_interaction)
class(data1$ID_focal)
data1$ID_focal <-as.factor(data1$ID_focal)
class(data1$NUDistress)
data1$NUDistress <-as.numeric(data1$NUDistress)
data1$NDistress <-as.numeric(data1$NDistress)

### Occurrence of Checking roost model, Binomial distribution
class(data1$Checkleaf)
class(data1$Type_interaction)
data1$Type_interaction <-as.factor(data1$Type_interaction)

#NULL
m5 <- glmer(Checkleaf ~ 1 + (1|ID_focal), family = binomial,  data = data1)
m5
summary(m5)
m6 <- glmer(Checkleaf ~ Type_interaction + (1|ID_focal), family = binomial,  data = data1)
m6
summary(m6)
plot_model(m6, type= "pred")
anova(m5,m6)
summary(glht(m6, mcp(Type_interaction ="Tukey")))  #There are no differences

### Occurrence of Enter-exit roost model, Binomial distribution
class(data1$ENTER.OUT)

#NULL
m7 <- glmer(ENTER.OUT ~ 1 + (1|ID_focal), family = binomial,  data = data1)
m7
summary(m7)

m8 <- glmer(ENTER.OUT ~ Type_interaction + (1|ID_focal), family = binomial,  data = data1)
m8
summary(m8)
plot_model(m8, type= "pred")

anova(m7,m8)
#To know how different they are in relation to the focal points. 
summary(glht(m8, mcp(Type_interaction ="Tukey")))  #There are no differences

### data2, Number of Aggressive vocalizations poisson distribution

datac <- read.csv("data2.csv")
m3 <- glmer(NUDistress ~ 1  + (1|ID_focal), family = poisson,  data = datac)
m3
summary(m3)

m4 <- glmer(Nuvoc ~ Type_interaction +  (1|ID_focal), family = poisson,  data = datac)
m4
summary(m4)
plot_model(m4, type= "pred")
anova(m3,m4)
# How different they are from each other 
library (multcomp)
a  <- glht(m4, mcp(Type_interaction ="Tukey")) 
summary(glht(m4, mcp(Type_interaction ="Tukey")))
summary(a)
# result: differences between focals and intruders
# increased aggressive vocalizations in intergroup interactions with neighbors.  

### PLOTS
# data set
data1 <- read.csv("data1.csv")
head(data1)
data1

### Checking roost

x3 <- data1 # para cambiar  nombre de los tratamientos a español
x3

x3$Type_interaction<- factor(x3$Type_interaction, levels = c("far", "focal", "near"), labels = c("Distant", "Focal", "Nearby"))

x3$Checkleaf2<- factor(x3$Checkleaf2, levels = c("no", "si"), labels = c("No", "Yes"))

color_table <- tibble( 
  Checkleaf2 = c("no", "si"),
  Color = c("#AB82FF", "#9AFF9A"))

J <-ggplot(x3, aes(Type_interaction, fill =Checkleaf2)) + 
  geom_bar (position = "fill", alpha = 0.7)  + scale_fill_manual(values = color_table$Color) +
  guides(fill = guide_legend(title = "Ocurrence")) +
  labs(x= "",y="Checking roost") + 
  theme_classic() 

L<-J + theme(axis.text=element_text(size=12),
             axis.title.y = element_text(size=14, margin = margin(r=10)),
             legend.margin =margin(0,0,0,0), legend.box.margin = margin(-10,10,-10,-10))
L
### Enter-exist roost

x4 <- data1 # para cambiar  nombre de los tratamientos a español
x4

x4$Type_interaction<- factor(x4$Type_interaction, levels = c("far", "focal", "near"), labels = c("Distant", "Focal", "Nearby"))

x4$ENTEROUT2<- factor(x4$ENTEROUT2, levels = c("no", "si"), labels = c("No", "Yes"))

color_table <- tibble( 
  ENTEROUT2 = c("no", "si"),
  Color = c("#AB82FF", "#9AFF9A"))




K<-ggplot(x4, aes(Type_interaction, fill =ENTEROUT2)) + 
  geom_bar (position = "fill", alpha = 0.7)  + scale_fill_manual(values = color_table$Color) +
  guides(fill = guide_legend(title = "Ocurrence")) +
  labs(x= "",y="Enter-exit roost") + 
  theme_classic() 

H<-K + theme(axis.text=element_text(size=12),
             axis.title.y = element_text(size=14, margin = margin(r=10)),
             legend.margin =margin(0,0,0,0), legend.box.margin = margin(-10,10,-10,-10))

H


# Join plots Checking and Enter-exist roost

D <-ggarrange( L + rremove("x.text") , H, ncol = 1, nrow = 3 + stat_n_text())
D
ggsave("Figure 2.png", width=  5, height =  5.5, dpi = 600 )


#  Aggressive vocalizations

data2 <- read.csv("data2.csv")
head(data2)
data2
x2 <- data2
x2

x2$Type_interaction<- factor(x2$Type_interaction, levels = c("far", "focal", "near"), labels = c("Distant", "Focal", "Nearby"))

voca<- ggplot(x2, aes( x=Type_interaction, y= Nuvoc,  fill=Type_interaction)) + 
  geom_boxplot( alpha = 0.6)+ labs(x= "",y="Number of aggressive vocalizations") + ylim(0,30) +
  scale_fill_manual(values = c("deeppink", "#8B8989","#FFD700")) +
  geom_signif(aes(Type_interaction, voc), annotation="***", y_position=28, xmin=1, xmax=1.8, tip_length = 0, vjust = 0.1) + 
  geom_signif(aes(Type_interaction, voc), annotation="***", y_position=28, xmin=2.2, xmax=3, tip_length = 0,  vjust = 0.1) +
  geom_signif(aes(Type_interaction, voc), annotation="*", y_position=25.5, xmin=1, xmax=3, tip_length = 0, vjust = 0.1)+
  theme_classic() 
voca
A<- voca + theme(axis.text=element_text(size=12),
                 axis.title.y = element_text(size=16, margin = margin(r=10)),
                 legend.margin =margin(0,0,0,0), legend.box.margin = margin(-10,10,-10,-10), legend.position='none')
A
ggsave("Figure 6", width=  5, height =  5.5, dpi = 600 )


