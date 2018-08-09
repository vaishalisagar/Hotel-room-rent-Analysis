

  #              Hotel Room Pricing In The Indian Market
  # NAME:     Vaishali Sagar
  # EMAIL:    vaishalisagar08@gmail.com
  # COLLEGE : Bhagwan Parshuram Institute of Technology

##Set Directory
##reading the dataset and creating dataframe

hot2.df <-read.csv(paste("Cities42 (1).csv",sep="" ))
View(hot2.df)
attach(hot2.df)

##########################################################


#describing the variables and identifying catagorical variables

library(psych)
describe(hot2.df)[,c(3,9,10,11)]

#structure of the variables
str(hot2.df)


#summary of data
summary(hot2.df)

###############################

##omiting the null record
hot2.df<-na.omit(hot2.df)


#################################

#boruta command for identifying the important and unimportant variables w.r.t. ROOMRENT
library(Boruta)
boruta<- Boruta(RoomRent~ CityName+ Population+ CityRank+ IsMetroCity+ 
                  IsTouristDestination+ IsWeekend +
                  IsNewYearEve+ Date + HotelName +StarRating 
                +Airport +HotelAddress +HotelPincode+HotelDescription+
                  FreeWifi +FreeBreakfast +HotelCapacity +HasSwimmingPool, data=hot2.df)
                
print(boruta)
TentativeRoughFix(boruta)
### IMPORTANT AND DEPENDABLE VARIABLE ON Y(ROOM RENT) ARE 13 except date, isnewyear eve and is weekend

  ########################## 3 REDUNDANT VARIABLES ARE REMOVED#############################3


 str(hot2.df)
##anova test for finding the most significiant variables

 fit1<- aov(RoomRent~Airport + CityName + CityRank + FreeBreakfast
            + FreeWifi + HasSwimmingPool+
              HotelAddress + HotelCapacity + HotelDescription + HotelName
            + HotelPincode +
              IsMetroCity + IsTouristDestination + Population +
              StarRating,data = hot2.df)
 
 summary(fit1)
 summary(fit1)[[1]][["Pr(>F)"]]
 ##result:
 #Airport
 #cityname
 #cityrank
 #,hasswimming pool,
 #freewifi
 #,hoteladd,
 #hotel capacity,
 #hotel description,
 #hotel name
 
 ######################################################
 
 newhot<- hot2.df[,c(2,4,10,13,16,17,19,20,11)] 
 

 ##finding most significiant variable 
 library(corrgram)
 corrgram(hot2.df,order = TRUE,
          lower.panel = panel.shade,
          upper.panel = panel.pie,
          text.panel = panel.txt,
          main="corgram of Hotel Data")
 
###corrgram shows the most significant variables:
 #swimming pool
 #star rating
 #population
 #####hence they are predictors(x1,x2,x3 respectively.)#######
 
 
####################################################################################
 #############VISUALISATION OF DEPENDENT VARIABLES WRT TO ROOMRENT########################

##swimming pool 
x1<-table(hot2.df$HasSwimmingPool)
 lbls<- c("yes","no")
pie(x1, main = "Hotels that have swimming pools or not", labels = lbls,
    col = c("skyblue","white"))

##star Rating
x2<- table(hot2.df$StarRating, main=" Bar plot for star rating")
barplot(x2)

##population
barplot(hot2.df$Population,main="bar plot for population")

######visualisation of x1,x2,x3 with y###
library(car)
library(ggplot2)

 #star rating vs roomrent

qplot(StarRating,RoomRent, data = hot2.df,color= "red",
      main="Relation Depicted by Room Rent and Hotel Capacity",
      log="xy"
      )

#population vs roomrent
library(car)
x1<-scatterplot(hot2.df$RoomRent,hot2.df$Airport,
            col = c("red","gray"))
jitter(x1,factor=1)

#swimming pool vs roomrent
boxplot(hot2.df$RoomRent,hot2.df$HasSwimmingPool,
        ylab="rent",main="boxplot of room rent and swimming pool")

#corrgram of y,x1,x2 and x3

x4<-data.frame(hot2.df$RoomRent,hot2.df$HasSwimmingPool,
               hot2.df$HotelCapacity,hot2.df$StarRating)
library(corrgram)
corrgram(x4, order=TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.pie,
         text.panel = panel.txt,
         main="corrgram of hotel price in india")

#####variance covariance matrix of y,x1,x2,x3
x5<- hot2.df[,c("HasSwimmingPool","StarRating","HotelCapacity")]
x6<- hot2.df[,c("RoomRent")]
cor(x5,x6)

cov(x5,x6)

#Comparing other factors and their pattern using other trends with roomrent

#Analyzing IsWeekend effect on RoomRent
table(hot2.df$IsWeekend)

table1<-table(hot2.df$IsWeekend)
barplot(table1, main="Distribution of Weekend", xlab="Not weekend(0)         Weekend(1)"
        , col="pink")

#Effect of Isweekend on RoomRent
w= aggregate(RoomRent ~ IsWeekend, data=hot2.df,mean)
w

boxplot(RoomRent~IsWeekend,data=hot2.df,
        main="Room rent vs. IsWeekend", 
        ylab="Not weekend(0)  weekend(1)", 
        xlab="Room Rent in rupees ", col=c("orange","yellow"),horizontal=TRUE)

#RoomRent less than 1 lakh because the outliers effect the average
RoomRent1.df <-hot2.df[which(hot2.df$RoomRent<100000),]
#Without extreme outliers 
boxplot(RoomRent~IsWeekend,data=RoomRent1.df, 
        main="Room rent vs. IsWeekend", 
        ylab="Not weekend(0)  weekend(1)", 
        xlab="Room Rent in rupees ", 
        col=c("red","blue"),horizontal=TRUE)




#Comapring RoomRent on different dates
table(hot2.df$Date)

#Removing the repeated date by gsub command

hot2.df$Date<-gsub("18-Dec-16", "Dec 18 2016", hot2.df$Date)
hot2.df$Date<-gsub("21-Dec-16", "Dec 21 2016", hot2.df$Date)
hot2.df$Date<-gsub("24-Dec-16", "Dec 24 2016", hot2.df$Date)
hot2.df$Date<-gsub("25-Dec-16", "Dec 25 2016", hot2.df$Date)
hot2.df$Date<-gsub("28-Dec-16", "Dec 28 2016", hot2.df$Date)
hot2.df$Date<-gsub("31-Dec-16", "Dec 31 2016", hot2.df$Date)
hot2.df$Date<-gsub("4-Jan-17", "Jan 04 2017", hot2.df$Date)
hot2.df$Date<-gsub("4-Jan-16", "Jan 04 2017", hot2.df$Date)
hot2.df$Date<-gsub("8-Jan-16", "Jan 08 2017", hot2.df$Date)
hot2.df$Date<-gsub("8-Jan-17", "Jan 08 2017", hot2.df$Date)
hot2.df$Date<-gsub("Jan 4 2017", "Jan 04 2017", hot2.df$Date)
hot2.df$Date<-gsub("Jan 8 2017", "Jan 08 2017", hot2.df$Date)
##converting date into numeric
hot2.df$Date<-factor(hot2.df$Date)
is.factor(hot2.df$Date)


#RoomRent on different days

p1 = aggregate(RoomRent ~ Date, data = hot2.df,mean)
p1

scatterplot(p1$Date,p1$RoomRent, main="Scatterplot between Date and RoomRent", 
            xlab="Date", ylab = "Room Rent (Rupees)",
            col = c("red","gray"))


boxplot(RoomRent~Date,data=hot2.df, 
        main="Room rent vs. Date", 
        xlab="Different Dates", 
        ylab="Room Rent in rupees ", 
        col=c("red","blue","green","yellow"))

##Without extreme outliers
boxplot(RoomRent~Date,data=RoomRent1.df,
        main="Room rent vs. Date", xlab="Different Dates", 
        ylab="Room Rent (rupees) " )

#visualising IsMetroCity with respect to RoomRent
table(hot2.df$IsMetroCity)

table1<-table(hot2.df$IsMetroCity)
barplot(table1, main="Distribution of IsMetroCity", 
        xlab="Not a Metro city(0)         Is a Metro City(1)", col="orange")


m = aggregate(RoomRent ~ IsMetroCity, data = hot2.df, mean)
m

boxplot(RoomRent~IsMetroCity,
        data=hot2.df, main="Room rent vs. IsMetroCity",
        ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ",
        col=c("red","blue"),horizontal=TRUE)

##Without extreme outliers
boxplot(RoomRent~IsMetroCity,
        data=RoomRent1.df, 
        main="Room rent vs. IsMetroCity", 
        ylab="Metro city(1) or not(0)", xlab="Room Rent in rupees ", 
        col=c("red","blue","green","yellow"),horizontal=TRUE)


#visualising IsTouristDestination with respect to RoomRent with and without outliers
table(hot2.df$IsTouristDestination)

table1<-table(hot2.df$IsTouristDestination)
barplot(table1, main="Distribution of IsToursitDestination",
        xlab="Not a Tourist Destination(0)         Is a Tourist Destination(1)", 
        col="brown")

t = aggregate(RoomRent ~ IsTouristDestination, data = hot2.df, mean)
t

boxplot(RoomRent~IsTouristDestination,data=hot2.df, 
        main="Room rent vs. IsTouristDestination",
        ylab="IsTouristDestination(1) or not(0)", 
        xlab="Room Rent in rupees ", col=c("red","blue","green","yellow"),
        horizontal=TRUE)
##Without extreme outliers
boxplot(RoomRent~IsTouristDestination,data=RoomRent1.df,
        main="Room rent vs. IsTouristDestination", 
        ylab="IsTouristDestination(1) or not(0)", 
        xlab="Room Rent in rupees ", 
        col=c("red","blue","green","yellow"),horizontal=TRUE)

#visualising freewifi with respect to RoomRent with and without outliers
table(hot2.df$FreeWifi)
fw<-table(hot2.df$FreeWifi)
barplot(fw, main="Borplot of FreeWifi",
        xlab= "FreeWifi" ,col="peachpuff")

w = aggregate(RoomRent ~ FreeWifi, data = hot2.df, mean)
w

##With extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=hot2.df,
        main="Room rent vs. FreeWifi", 
        ylab="Free Wifi available(1)", 
        xlab="Room Rent in rupees ",
        col=c("red","blue","green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeWifi,data=RoomRent1.df,
        main="Room rent vs. FreeWifi",
        ylab="Free Wifi available(1)", 
        xlab="Room Rent in rupees ", 
        col=c("red","blue","green","yellow"),horizontal=TRUE)


#visualising freeBreakfast with respect to RoomRent with and without outliers
table(hot2.df$FreeBreakfast)
table1<-table(hot2.df$FreeBreakfast)
barplot(table1, main="Borplot of FreeBreakfast",xlab= "FreeWifi" ,col="purple")

aggregate(RoomRent ~ FreeBreakfast, data =hot2.df, mean)
aggregate(RoomRent ~ FreeBreakfast, data =RoomRent1.df, mean)
##Aggregate are affected by outliers a lot in the case of FreeBreakfast on RoomRent


##With extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=hot2.df, 
        main="Room rent vs. FreeBreakfast", 
        ylab="Free Breakfast available(1)",
        xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)
##Without extreme outliers of roomrent
boxplot(RoomRent~FreeBreakfast,data=RoomRent1.df, 
        main="Room rent vs. FreeBreakfast",
        ylab="Free Breakfast available(1)", 
        xlab="Room Rent in rupees ", col=c("green","yellow"),horizontal=TRUE)


#visualising Airport with respect to RoomRent with and without outliers

summary(hot2.df$Airport)

boxplot(hot2.df$Airport, main="Boxplot of Airport",
        xlab= "Distance of airport from hotel(Km)" ,
        col="orange",horizontal = TRUE)

#Effect of Airport distance on RoomRent

scatterplot(hot2.df$Airport,hot2.df$RoomRent,
            main="Room rent vs. Airport distance",
            xlab="Airport distance(km)",
            ylab="Room Rent in rupees ",
            col=c("brown"),cex=1.1,log="xy")
###############################################################################

##HYPOTHESIS###

##Hypothesis


#1.Average RoomRent of hotels providing Free Breakfast is more than that which don't provide.
#2.Average RoomRent of hotels that have swimming pool is more than that which don't have.
#3.Average RoomRent of hotels with high star rating is high as compared to one which has less star rating.
#4.Average RoomRent in metro cities hotels is more than that of non metro cities.
#5.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.



t.test(RoomRent~HasSwimmingPool,data = hot2.df, alternative="less")
t.test(hot2.df$RoomRent,hot2.df$StarRating)
t.test(RoomRent~FreeBreakfast, data = hot2.df, alternative="less")
t.test(RoomRent~IsMetroCity, data = hot2.df, alternative="less")
t.test(hot2.df$RoomRent,hot2.df$HotelCapacity)

#############################################################

##FITTIND MULTIPLE LINEAR REGRESSION MODEL


model1<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity, data = hot2.df)
summary(model1)
#result
#StarRating       3597.322   
#HasSwimmingPool  2528.885    
#HotelCapacity     -15.558  

model2<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+
           IsWeekend + Date, data = hot2.df)
summary(model2)
#result
#StarRating       3616.708    
#HasSwimmingPool  2498.877    
#HotelCapacity     -15.559  
#IsWeekend          -97.772  
#date               1854.26

model3<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+Airport, data = hot2.df)
summary(model3)
#result
#StarRating       3522.99
#HasSwimmingPool  2708.400   
#HotelCapacity     -14.776 
#Airport            25.344

model4<-lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity+IsTouristDestination, 
           data = hot2.df)
summary(model4)
#results
#StarRating            3635.967    
#HasSwimmingPool       2285.337    
#HotelCapacity          -13.965    
#IsTouristDestination  1877.481 


str(hot2.df)

x5<- hot2.df$Airport 
x5<-hot2.df$CityRank 
x6<-hot2.df$FreeBreakfast
x7<-hot2.df$FreeWifi 
x8<-hot2.df$HasSwimmingPool 
x9<-hot2.df$HotelCapacity 
x10<-hot2.df$HotelPincode 
x11<-hot2.df$StarRating 
x12<-hot2.df$IsTouristDestination 
x13<-hot2.df$IsMetroCity  
x14<-hot2.df$Population 

library(ggplot2)
library(lattice)
library(caret)
library(mlbench)


y<- hot2.df[,c(2,3,4,5,6,10,11,12,13,14)]
y1<- hot.df[,c(11)]
correlationMatrix<- cor(y,y1)

model <- train(RoomRent~Airport + CityName + CityRank + FreeBreakfast+
               FreeWifi + HasSwimmingPool + HotelCapacity + HotelAddress +
                 HotelDescription + HotelPincode + StarRating + HotelName
               + IsTouristDestination + IsMetroCity  + Population ,
               data=hot2.df)
               
importance<-varImp(model,scale=FALSE)
print(importance)
plot(importance)

######