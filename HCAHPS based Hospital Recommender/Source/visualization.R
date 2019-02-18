install.packages("gridExtra")
install.packages("grid")

library(tidyverse)
library(gridExtra)
library(grid)
newdata=read.csv(file="HCAHPS_Data.CSV",header = TRUE)
attach(newdata)

#boxplot on significant variables
a1<-ggplot(data = newdata)+
  geom_boxplot(aes(x="",y=H_COMP_1_SN_P))
a2<-ggplot(data = newdata)+
  geom_boxplot(aes(x="",y=H_COMP_7_D_SD))
a3<-ggplot(data = newdata)+
  geom_boxplot(aes(x="",y=H_COMP_3_A_P))
a4<-ggplot(data = newdata)+
  geom_boxplot(aes(x="",y=H_COMP_2_SN_P))
grid.arrange(a1,a2,a3,a4,nrow=1)
# boxplots show us outpliers in our data. but we are keeping our outliers as each row in our data represent a hospital
# and removing outliers will remove a complete row and inturn remove one or more hospitals. we dont want to remove any
# hospital and thus we are keeping our outliers as it is.

#scatterplots
ggplot(data = newdata)+
  geom_point(aes(x=H_COMP_1_A_P,y=H_COMP_1_LINEAR_SCORE))

ggplot(data = newdata)+
  geom_point(aes(x=H_COMP_1_A_P,y=H_COMP_1_LINEAR_SCORE,color=RCMND))
# H_COMP_1_A_P: Patients who reported that their nurses "Always" communicated well 
# H_COMP_1_LINEAR_SCORE: Nurse communication
# by looking at these two question, we can make an asumption that those patient who say hospital's nurses communicated
# well should also give high linear score to overall nurse communication. By looking at our scatter plot, we can see
# that this assumption is true i.e. H_COMP_1_A_P value increases with increase in value of H_COMP_1_LINEAR_SCORE.
# when we compared this plot with RCMND i.e. is the hopital recommendable, we see that lower the values of
# H_COMP_1_A_P in our graph, lower the chances of hospital being recommended. 

ggplot(data = newdata)+
  geom_point(aes(x=H_COMP_1_SN_P,y=H_COMP_1_LINEAR_SCORE))
ggplot(data = newdata)+
  geom_point(aes(x=H_COMP_1_SN_P,y=H_COMP_1_LINEAR_SCORE,color=RCMND))
# H_COMP_1_SN_P: Patients who reported that their nurses "Sometimes" or "Never" communicated well.
# H_COMP_1_LINEAR_SCORE: Nurse communication
# by looking at these two question, we can make an asumption that those patient who say hospital's nurses didnt 
# communicate well should also give lower linear score to overall nurse communication. By looking at our scatter 
# plot, we can see that this assumption is true i.e. H_COMP_1_A_P value decreases with increase in value of 
# H_COMP_1_LINEAR_SCORE. when we compared this plot with RCMND i.e. is the hopital recommendable, we see that lower 
# the values of H_COMP_1_SN_P in our graph, higher the chances of hospital being recommended. 


#histogram
p1<-ggplot(data = newdata)+
  geom_histogram(aes(x=H_COMP_1_A_P),color="RED")
p2<-ggplot(data = newdata)+
  geom_histogram(aes(x=H_COMP_2_A_P),color="BLUE")
p3<-ggplot(data = newdata)+
  geom_histogram(aes(x=H_COMP_3_A_P),color="YELLOW")
p4<-ggplot(data = newdata)+
  geom_histogram(aes(x=H_COMP_5_A_P),color="GREEN")
grid.arrange(p1,p2,p3,p4,nrow=2)
# each of the histograms show us the distribution of hospitals according to different question ID's
# like H_COMP_1_A_P: Patients who reported that their nurses "Sometimes" or "Never" communicated well. by looking at
# the histogram we can see that most patients gave higher ratings to nurse communication and the same conclusions can
# be made from other communication histograms


#pie chart
ggplot(data = newdata)+
  geom_bar(aes(x=State))+ coord_polar()
# pie chart can be used to see distribution of each variable like in above piechart we can see which state has highest
# number of hospitals

pie <- ggplot(newdata, aes(x = factor(1), fill = factor(H_COMP_1_STAR_RATING))) +
  geom_bar(width = 1)
pie + coord_polar(theta = "y")
# In above piechart we can see how many patients gave how much rating out of 5 to hospitals on basis of nurse 
# communication. we can see most of the patients gave 3/5 star ratings to hospitals.

ggplot(data=newdata, aes(x=factor(1), fill=RCMND))+
  geom_bar(width = 1)+
  coord_polar("y")
# in the above pie chart we can see distribution of recommnedation of hospitals. we can see there are more hopitals that 
# are being recommended 


