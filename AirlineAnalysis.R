#The following URL is the location of the survey datafile
#NPS = %promoters - %detractors
library(arulesViz)
library(car)
library(carData)
library(caret)
library(ggmap)
library(readr)
library(tidyverse)
library(rjson)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(mapdata)
library(dplyr)
library(corrplot)
setwd("C:/Users/acumm/Downloads")
# https://drive.google.com/file/d/1G7f3LiSW-NmqsiBENwYd-nEfh4_9eV7D/view?usp=sharing
system("gdown --id 1G7f3LiSW-NmqsiBENwYd-nEfh4_9eV7D")

#we can now read in the datafile - which is in JSON format

mydata.list <- jsonlite::fromJSON("completeSurvey.json")
survey <- data.frame(mydata.list)
surveyOriginal=survey
summary(survey$Loyalty)

#Data Validation
count(unique(surveyOriginal, incomparables = FALSE, MARGIN = 1,
       fromLast = FALSE))
uniqueCount=distinct(surveyOriginal,.keep_all = FALSE)

repeats=duplicated(surveyOriginal)

range(survey)

survey$weekday=as.factor(weekdays(as.Date(survey$Flight.date,'%m/%d/%Y')))

survey$DelayTotal=survey$Departure.Delay.in.Minutes+survey$Arrival.Delay.in.Minutes
survey$AirportExp=survey$Eating.and.Drinking.at.Airport+survey$Shopping.Amount.at.Airport
survey$Detractor=survey$Likelihood.to.recommend<8
survey$DelayGreaterThan5Mins=survey$DelayTotal>5


highRec=survey[survey$Likelihood.to.recommend>8,]
medRec=survey[survey$Likelihood.to.recommend==7,]
medRec=rbind(medRec,survey[survey$Likelihood.to.recommend==8,])
lowRec=survey[survey$Likelihood.to.recommend<7,]



survey$Detractor<-as.factor(survey$Detractor)





survey$weekdayNum <- recode(survey$weekday, 
                         "Sunday"=0,
                         "Monday"=1,
                         "Tuesday"=2,
                         "Wednesday"=3,
                         "Thursday"=4,
                         "Friday"=5,
                         "Saturday"=6)
# Convert all to numeric
survey$DetractorL=as.logical(survey$Detractor)
survey$DetractorL=as.numeric(survey$DetractorL)
dat <- cbind(var1=survey$Loyalty,var2=lowRec$Loyalty,var3=highRec$Loyalty)
dat <- as.data.frame(dat)   # get this into a data frame as early as possible
barplot(sapply(dat,mean))

lowRec$AirportExp=lowRec$Eating.and.Drinking.at.Airport+lowRec$Shopping.Amount.at.Airport
medRec$AirportExp=medRec$Eating.and.Drinking.at.Airport+medRec$Shopping.Amount.at.Airport
highRec$AirportExp=highRec$Eating.and.Drinking.at.Airport+highRec$Shopping.Amount.at.Airport

hist(summary(lowRec$AirportExp))
ggplot(lowRec, aes(x=AirportExp)) + geom_histogram(binwidth=5)
boxplot(lowRec$AirportExp)$out
ggplot(lowRec, aes(x=AirportExp)) + geom_histogram(binwidth=5)
summary(highRec$AirportExp)
##potential factors so far, loyalty, delay, flights per year
summary(lowRec$Airline.Status)
View(lowRec$Airline.Status)

summary(lowRec$Class)
View(lowRec$Class)

ggplot(lowRec,aes(x=,y=Likelihood.to.recommend))+geom_boxplot()#good tool!!
ggplot(survey,aes(x=survey$Class,y=Likelihood.to.recommend))+geom_boxplot()
ggplot(survey,aes(x=interaction(Gender,Class),y=Likelihood.to.recommend))+geom_histogram()#good tool!!
ggplot(survey,aes(x=interaction(Type.of.Travel,Class),y=Likelihood.to.recommend))+geom_boxplot()





survey=survey[!is.na(x1),]
survey=survey[!is.na(x2),]
survey=survey[!is.na(x3),]
survey=survey[!is.na(x4),]
survey=survey[!is.na(x5),]
survey=survey[!is.na(x6),]
survey=survey[!is.na(x7),]#NA values
survey=survey[!is.na(x8),]#NA values
survey=survey[!is.na(x9),]
survey=survey[!is.na(x10),]#NA values
survey=survey[!is.na(x11),]
summary(survey$Flight.Distance)
plot(medNPS$Loyalty~medNPS$Flight.Distance)
lowMiles=survey[survey$Flight.Distance<1000,]
summary(lowMiles$Loyalty)
y=survey1$Likelihood.to.recommend
x1=survey1$Age
x2=survey1$Gender
x3=survey1$Price.Sensitivity
x4=survey1$Loyalty
x5=survey1$Flights.Per.Year
x6=survey1$Class
x7=survey1$Departure.Delay.in.Minutes
x8=survey1$Arrival.Delay.in.Minutes
x9=survey1$Flight.Distance
x10=survey1$Flight.time.in.minutes
x11=survey1$Flight.cancelled
fit1=lm(waittime~height+duration, data=gdata)
fit2=lm(waittime~1, data=gdata)
app.step.fw=step(fit2, direction="forward", scope=list(upper=fit1, lower=fit2))
surveyNumerics=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,y)
surveyNumerics=surveyNumerics[complete.cases(surveyNumerics)]
final[complete.cases(final), ]
fit1=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=survey1)
fit2=lm(y~1, data=survey1)
app.step.fw=step(fit2, direction="forward", scope=list(upper=fit1, lower=fit2))

survey1=survey<- subset (survey, select = -freeText)
survey1=survey1[complete.cases(survey1),]
leapssubsets=regsubsets(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, data=survey1)
plot(leapssubsets, scale="adjr2")

subsets(leapssubsets, statistic="adjr2", main="Adjusted R^2 plot" , legend= False, min.size=1)

subsets(leapssubsets, statistic="cp", main="Cp plot for all subset regression", legend=FALSE, min.size=1)


subsets(leapssubsets, statistic="adjr2", main="Adjusted R^2 plot" , legend= FALSE, min.size=1)

subsets(leapssubsets, statistic="cp", main="Cp plot for all subset regression", legend=FALSE, min.size=1)

##map data
################## Origion state ###########################
survey$Origin.State <- tolower(survey$Origin.State)
US <- map_data("state")
map1 <- ggplot(survey,aes(map_id=survey$Origin.State))
map1 <- map1 + geom_map(map=US,aes(fill=survey$Likelihood.to.recommend))
map1 <- map1 +expand_limits(x=US$long,y=US$lat)
map1 <- map1 + coord_map() + ggtitle("Likelihood to recommend based on Origin state")
map1

#origin states of interest low NPS-OHIO,CONN,MARYLAND, South Dakota, Nevada
#origin states of interest high NPS-Cal,PA,Vermnont,MiSS



############ CORRELATION ##############

NumSurvey = select_if(survey,is.numeric)
round(cor(NumSurvey,use= "complete.obs"),2)
NumSurvey$Likelihood.to.recommend

library(ggplot2)
ggplot(NumSurvey) +geom_
  aes(x =Likelihood.to.recommend, y =Loyalty ) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()



# multiple scatterplots
pairs(dat[, c(1, 4, 6)])

# improved correlation matrix

corrplot(cor(NumSurvey,use="complete.obs"),
         #method = "number",
         type = "upper" # show only upper side
)

corrplot(
  data = NumSurvey,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

################# destination state #########################

survey$Destination.State <- tolower(survey$Destination.State)
US <- map_data("state")
US$region=tolower(US$region)
map2 <- ggplot(survey,aes(map_id=survey$Destination.State))
map2 <- map2 + geom_map(map=US,aes(fill=survey$Likelihood.to.recommend))
map2 <- map2 + expand_limits(x=US$long,y=US$lat)
map2 <- map2 +coord_map() ggtitle("Likelihood to recommend based on Destination State")
map2=map2+geom_label(aes(x=survey$dlong,y=survey$dlat),label=survey$Destination.State)
#coord_map()
snames <- aggregate(cbind(survey$dlong,survey$dlat) ~ survey$Destination.State, data=survey,FUN=function(x)mean(range(x)))


map2<-map2+coord_map()+geom_text(data=snames, aes(US$long, US$lat, label =US$region), size=1)
#destination states of interest low NPS,texas,PA,TENN, South carolina
#destination states of interest high NPS, WASH, Oregon Wisconsion Georgia
map2
######################### weekday analysis
home
#WN MQ EV AS OO B6 OU AA DL HA US
ggplot(survey[survey$Partner.Code=="US",],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot(fill="lightblue")+
  theme(axis.text.x=element_text( angle=60, hjust=1),text = element_text(size = 16))+labs(title="Weekday vs LTR with AIRLINE CODE=US",x="Weekday")

ggplot(survey[survey$Partner.Code=="US",],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot()##same across everyday
ggplot(lowRec,aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday

ggplot(survey[survey$Class=="Business",],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot()##same across everyday
ggplot(lowRec,aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday
ggplot(survey[survey$Price.Sensitivity==4,],aes(x=weekday,y=Likelihood.to.recommend))+geom_bar(fill="lightblue")+labs(title="Weekday vs LTR with Price Sen.=4",x="Weekday")

ggplot(survey[survey$Price.Sensitivity==4,],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot(fill="lightblue")+theme(axis.text.x=element_text( angle=60, hjust=1),text = element_text(size = 16))+labs(title="Weekday vs LTR with Price Sen.=4",x="Weekday")
summary(survey[(survey$Price.Sensitivity==2) &(survey$weekday=="Tuesday"),])
summary(survey[(survey$Price.Sensitivity==2) &(survey$weekday=="Wednesday"),])
##same across everyday
ggplot(survey[survey$Price.Sensitivity==3,],aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday
##when price sensitivity is lower, sunday seems like the day to encourage flying
##Price sensitivity almost a whole point higher on tuesday in terms of likelihood to recommend
ggplot(survey[survey$Price.Sensitivity==3,],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot()##same across everyday
ggplot(survey,aes(x=Loyalty,y=Likelihood.to.recommend))+
  stat_summary(aes(y =survey$Likelihood.to.recommend ,group=1), fun.y=mean, colour="blue", geom="point",group=1)
#show trend in loyalty with likelihood to reccomend
ggplot(survey[survey$Airline.Status=="Platinum",],aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot(fill="lightblue")+labs(title="Weekday vs LTR with Airline Status=Platinum",x="Weekday")
summary(survey[(survey$Airline.Status=="Platinum") &(survey$weekday=="Monday"),])
summary(survey[(survey$Airline.Status=="Platinum") &(survey$weekday=="Wednesday"),])

ggplot(lowRec,aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday

ggplot(medRec,aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot()##same across everyday
ggplot(medRec,aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday

ggplot(highRec,aes(x=weekday,y=Likelihood.to.recommend))+geom_boxplot()##same across everyday
ggplot(highRec,aes(x=weekday,y=Loyalty))+geom_boxplot()##same across everyday


"Southeast Airlines Co."

#Destination.City               Origin.City                    Airline.Status                 Age                            Gender                        
#[6] Price.Sensitivity              Year.of.First.Flight           Flights.Per.Year               Loyalty                        Type.of.Travel                
#[11] Total.Freq.Flyer.Accts         Shopping.Amount.at.Airport     Eating.and.Drinking.at.Airport Class                          Day.of.Month                  
#[16] Flight.date                    Partner.Code                   Partner.Name                   Origin.State                   Destination.State             
#[21] Scheduled.Departure.Hour       Departure.Delay.in.Minutes     Arrival.Delay.in.Minutes       Flight.cancelled               Flight.time.in.minutes        
#[26] Flight.Distance                Likelihood.to.recommend        olong                          olat                           dlong                         
#[31] dlat                           freeText                       weekday                       

survey$Flight.cancelled=as.factor(survey$Flight.cancelled)

####lm modeling p2
train=survey[1:70000,]
test=survey[70001:88100,]






lm6<-lm(survey$Detractor~Flight.time.in.minutes+Flights.Per.Year+Age+Gender+AirportExp+Price.Sensitivity+Airline.Status+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel+Class,data=survey)
summary(lm6)

lm1<-lm(survey$Likelihood.to.recommend~Airline.Status+Price.Sensitivity+weekday+Flight.Distance+Scheduled.Departure.Hour+Class+DelayTotal+Loyalty+Age+Origin.State+Origin.City+Destination.City+Destination.State,data=survey)
lm2<-lm(survey$Likelihood.to.recommend~Age+Airline.Status+Price.Sensitivity+weekday+Flight.Distance+Scheduled.Departure.Hour+DelayTotal+Flights.Per.Year+Type.of.Travel,data=survey)
lm3<-lm(survey$Likelihood.to.recommend~Age+Airline.Status+Price.Sensitivity+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel,data=survey)
lm4<-lm(survey$Likelihood.to.recommend~Flight.time.in.minutes+Flights.Per.Year+Age+Gender+AirportExp+Price.Sensitivity+Airline.Status+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel+Class,data=survey)
#lm4<-lm(survey$DetractorL~Flight.time.in.minutes+Flights.Per.Year+Age+Gender+AirportExp+Price.Sensitivity+Airline.Status+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel+Class,data=survey)

summary(lm1)
lgm<-glm(survey$Detractor~Flight.time.in.minutes+Flights.Per.Year+Age+Gender+AirportExp+Price.Sensitivity+Airline.Status+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel+Class,data=survey,family=binomial(link='logit'))
lgm2<-glm(survey$Likelihood.to.recommend~Flight.time.in.minutes+Flights.Per.Year+Age+Gender+AirportExp+Price.Sensitivity+Airline.Status+Loyalty+Flight.Distance+DelayTotal+Flights.Per.Year+Type.of.Travel+Class,data=survey,family=binomial(link='logit'))

summary(lgm)
confint(lgm)
wald.test(b = coef(lgm), Sigma = vcov(lgm),Terms=1:16)

fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results  0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
"Accuracy 0.842696629213483"
###
###Airline.StatusGold                       1.090e+00  2.595e-02  41.982  < 2e-16 ***
#Airline.StatusPlatinum                   5.786e-01  3.984e-02  14.524  < 2e-16 ***
 # Airline.StatusSilver                     1.706e+00  1.807e-02  94.393  < 2e-16 ***
  #Price.Sensitivity                       -2.776e-01  1.305e-02 -21.265  < 2e-16 ***
####Destination.StatePennsylvania    4.264e-01  9.747e-02   4.374 1.22e-05 ***
#####Destination.StateUtah            5.223e-01  9.736e-02   5.364 8.14e-08 ***



###last thing associative rule mining with predicting detractors 

#this is not a sparse matrix, we see four columns of data, class of passenger,sex, age 
#and whether they survived, there are no empty values throughout
surveyA=survey
surveyA=subset(surveyA,select=-c(Likelihood.to.recommend,DetractorL,Partner.Code,Destination.State,Origin.State))

prop.table(table(survey$Detractor,survey$Airline.Status))
surveyA$Origin.City<-as.factor(survey$Origin.City)
surveyA$Destination.City<-as.factor(survey$Destination.City)
surveyA$Type.of.Travel<-as.factor(survey$Type.of.Travel)
surveyA$DelayGreaterThan5Mins=as.factor(survey$DelayGreaterThan5Mins)
surveyA$weekday<-as.factor(survey$weekday)
surveyA$Gender<-as.factor(survey$Gender)
surveyA$Price.Sensitivity<-as.factor(survey$Price.Sensitivity)
surveyA$Class<-as.factor(survey$Class)
surveyA$Flight.cancelled<-as.factor(survey$Flight.cancelled)
surveyX<-as(surveyA,"transactions")
inspect(surveyX)
itemFrequency(surveyX)
itemFrequencyPlot(surveyX,support=.1)
View(surveyX)
#badboatX takes each variable from badboat and breaks it down into comparative values, such as relative frequency's. It also blows out the options for each factor into their own columns
#and putting in 0s for each null value that would come with this, making this a sparse matrix

rules1 <- apriori(surveyX,parameter=list(supp=0.008,conf=0.55),control=list(verbose=F),appearance=list(default="lhs",rhs=("Detractor=TRUE")))
rules2 <- apriori(surveyX,parameter=list(supp=0.008,conf=0.55),control=list(verbose=F),appearance=list(default="lhs",rhs=("Detractor=FALSE")))

inspectDT(rules1[1:15])
inspectDT(rules1sortedLift[1:25])
inspectDT(rules1sortedConfidence[1:25])
inspectDT(rules1sprtedSupport[1:25])
rules1sortedLift <- sort(rules1, by="lift")
rules1sortedConfidence<-sort(rules1,by="confidence")
rules1sprtedSupport<-sort(rules1,by="support")
inspect(rules2[1:20])
plot(rules1sorted[1:6], method="graph", control=list(type="items"))
plot(rules1[1:20], method="paracoord", control=list(reorder=TRUE))
