rm(list=ls())


library(party)
library(fpc)
library(ggplot2)
library(gridExtra)
library(hrbrthemes)
library(dplyr)
library(cowplot)
library(ggpubr)
library(VIM)
library(plyr)
library(psych)
library(corrplot)
library(corrgram)


library(randomForest)
require(caTools)


setwd("C:\\Users\\mladi\\Desktop\\Data Mininig")

Vad = read.csv("VascularAccessData.csv")


#--------Data structure/Exploration

str(Vad)
#Basinc undestending of Vascularacces dataframe


#------Data Cleaning ---------

#Using table to count total number of missing values in date i.e. 313
table(is.na(Vad))

sapply(Vad,function(x) sum(is.na(x)))# Number of NA values with respect to each feature


#The missing fata graph shows the distribution of missing values in each variable
aggr(Vad, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, 
                    labels=names(Vad), gap=3, cex.axis=.5,cex.numbers=0.6,
                    ylab=c("Histogram of missing data","Pattern"))



#Removing 11 patient from current dataset
Vad<-Vad[!is.na(Vad$HYPERTENSION),]

#Removing 11 patient from current dataset
Vad<-Vad[!is.na(Vad$HOPB),]


Vad$AverageBloodFlow<-as.double(Vad$AverageBloodFlow) 

Vad <- Vad%>%
  dplyr::mutate(Bloodflow = if_else(
    is.na(AverageBloodFlow)
    , mean(AverageBloodFlow, na.rm = TRUE)
    , AverageBloodFlow
  )
  )


table(is.na(Vad))




#-----------Data Visualization-----------------

str(Vad)


#Pie chart of Genders 
ggplot(Vad, aes(x="", y="", fill=Gender)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() 

table(Vad$Gender)
prop.table(table(Vad$Gender))


ggplot(data = Vad) + 
  geom_bar(mapping = aes(x = AchievedParticular_VA, fill = Gender), position = "dodge")+
  scale_y_continuous(name = "Number Of patient")+ xlab("Type of vascular access that patient recived")





ggplot(Vad, aes(x=Age_years, y=timeOnStudy, color=AchievedParticular_VA)) +
  labs(color = "Three types of access:")+
  scale_x_continuous(name = "Age",
                     limits = c(20, 90),
                     breaks = seq(20, 90, by = 5))+
  scale_y_continuous(name = "Time on Study",
                     limits = c(0, 4000),
                     breaks = seq(0, 4000, by = 250))+
  geom_point(size=2)+theme_minimal()+geom_smooth(method="lm",se=F)



ggplot(Vad, aes(x=Bloodflow, y=Age_years, color=AchievedParticular_VA)) +
  labs(color = "Three types of access:")+
  scale_y_continuous(name = "Age",
                     limits = c(20, 90),
                     breaks = seq(20, 90, by = 5))+
  scale_x_continuous(name = "Bloodflow")+
  geom_point(size=2)+theme_minimal()+geom_smooth(method="lm",se=F)

Vad$Bloodflow
Vad$Age_years
Va


Vad$timeOnStudy
Vad$Age_years



ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
               color="AchievedParticular_VA",palette = "jco",
               add = "jitter")    


ggboxplot(Vad, x = "AchievedParticular_VA", y = "Age_years",
          color = "AchievedParticular_VA", palette = "jco")+    # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",label.y = 40) 



plot_grid(p1, p2, labels = "AUTO")




#TEST



pairs.panels(Vad,pch=10)
pairs.panels(Vad [c(24,15,3)])


my_cols <- c("#00AFBB", "#E7B800", "#FC4E07") 
pairs(Vad [c(24,15,3)], pch = 19,  cex = 1,
      col = my_cols[Vad$AchievedParticular_VA],
      lower.panel=NULL)



Corr<-Vad[c(24,15,3,18,19,20)]
corrplot(corrgram(Corr), method = "square")

Vad$AchievedParticular_VA


library(plyr)

# Basic density
p <- ggplot(Vad, aes(x=Age_years)) + 
  geom_density()+ geom_vline(aes(xintercept=mean(Age_years)),color="blue", linetype="dashed", size=1)
# Add mean line
p



ggplot(Vad, aes(x=timeOnStudy, color=Gender)) +
  geom_density()+ geom_vline(aes(xintercept=mean(Age_years)),color="blue", linetype="dashed", size=1)

ggplot(Vad, aes(x=timeOnStudy, color=AchievedParticular_VA)) +
  geom_density()+ geom_vline(aes(xintercept=mean(Age_years)),color="blue", linetype="dashed", size=1)


p

Vad$timeOnStudy
Vad$Gender
Vad$AchievedParticular_VA








#Clustering

set.seed(7000)
Vad2<-Vad[c(24,15,3,17)]
Vad3<-Vad[c(24,15,3)]

kmeans.result <-kmeans(Vad3,3)

table(Vad2$AchievedParticular_VA,kmeans.result$cluster)

plot(Vad3, col = kmeans.result$cluster)
points(kmeans.result$centers,
       col = 1:3, pch = 8, cex=2)


Vad$Age_years

Vad_Ctree <- ctree(AchievedParticular_VA ~ Age_years +
                      Bloodflow + timeOnStudy, data=Vad)


print(Vad_Ctree)

plot(Vad_Ctree)















rf <- randomForest(
  Age_years ~ .,
  data=Vad
)

summary(rf)


plot(rf)



rf_default <- randomForest(Age_years~.,
                    data = Vad,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
print(rf_default)


