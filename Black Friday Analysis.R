####Black friday Analysis Project####

#Installing the packages
install.packages("rpart.plot")
install.packages("Amelia")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("caTools")
install.packages("rpart")
install.packages("stringr")
install.packages("stats19")
install.packages("tidyverse")
install.packages("UsingR")
install.packages("prob")
install.packages("tree")

###loading the packages

library(ggplot2) #for plotting
library(corrplot)
library(Amelia)
library(caTools)
library(rpart)
library(rpart.plot)
library(UsingR)
library(stringr)  # for STRING operations
library(tidyverse)  # to work with TIBBLE
library(stats)
library(prob)
library(DataExplorer) #for EDA
library(dtplyr)
library(data.table)
library(MASS)
library(tree)
library(party)
library(rattle)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(caret)


##Loading the dataset
BlackFriday <-read.csv("C:/Users/Admin/Desktop/Black Friday Project/BlackFridayDataset.csv")
attach(BlackFriday)
summary(BlackFriday)
str(BlackFriday)
names(BlackFriday)

##Removing the NA values

BlackFriday<-na.omit(BlackFriday)
View(BlackFriday)
missmap(BlackFriday, col = c('yellow','black'), main = 'check') ##To check if their are any missing values and we had a full black plot so it indicate no missing values

####EDA(Exploratory Data Analysis)#####

#Exploring the basics in data
head(table(User_ID))
table(Gender)
###WE can see the gender have two variable: F and M . F=132197 and M=405380
table(Age)
##Age is divided into 7 ranges, Here Age is Categorical Variable
table(Occupation)
#There are 21 different occupation ranging from 0-21.
table(City_Category)
#Cities in which customers have lived is categorized into three categories: A B C
table(Stay_In_Current_City_Years)
#people lived in current city we can see 0,1,2,3,4+ which indicates the  years
table(Marital_Status)
#People have their marriage status marked as either 0 or 1


###########FOR GENDER###
## keeping copy of data in a temporary variable
temporary <- Gender
Gender <-table(Gender)
# making labels for piechart
gender.labels = c(" Female", " Male")
gender.percent = round(Gender/sum(Gender)*100)
gender.labels = paste(gender.labels, gender.percent)
gender.labels = paste(gender.labels, "%", sep="")
##Plotting a pie chart to see which gender shops more
pie(Gender, labels = gender.labels, col=c("Pink","Blue"), main="BlackFriday | Gender wise shoppers distribution")
#We can conclude that their are Male(75%) buyers and (25%)Female buyers
# making the variable same as before
Gender <- temporary 


by_gend <- BlackFriday %>% group_by(Gender) %>% summarise(Purchase = mean(Purchase))
ggplot(by_gend, aes(Gender, Purchase)) + geom_bar(stat='identity', fill = c('pink', 'black'))
#There are considerably male shoppers and they spend on average 900 units more than the female.


##########For AGE#############
ggplot(BlackFriday,aes(Age)) + geom_bar(fill = c('#c5fafa', '#002ba1',  '#6287ec', '#dd0244',  '#c90000', '#fb0202', 
'#ff9ecb') ) + theme_bw() 
#People withing range of 26-35 shopped most
#While people in age-range 0-17 or 55+ shopped least and almost none compared to 26-35.
#overall people within age range 18-45 are the group which is more active

by_age <- BlackFriday %>% group_by(Age) %>% summarise(Purchase = mean(Purchase))
ggplot(by_age, aes(Age, Purchase)) + geom_bar( stat = 'identity',fill = c('#c5fafa', '#002ba1', '#6287ec', '#dd0244', '#c90000', '#fb0202', '#ff9ecb')) 
# On average, each age group spends about the same. Younger age groups contribute more to the sales as a whole,
#however, the age group that spends the most on average are 51-55.

#######For City Category######
ggplot(BlackFriday,aes(City_Category)) + geom_bar(fill = c('red3','pink2','yellow2'))  

by_city <- BlackFriday %>% group_by(City_Category) %>% summarise(Purchase = mean(Purchase))
ggplot(by_city, aes(City_Category, Purchase)) + geom_bar(
  stat = 'identity',
  fill = c('red3','pink2','yellow2')
)
#Although city B had a lot more customers, the highest average spending occurred at city C on average.
#City B was able to attract the most customers but failed to attract them to spend the most.

#######For Purchase###
# purchase range
range.purchase = range(Purchase)
cat("Range of amount shoppers spent = ", range.purchase)
#Range of amount shoppers spent =  185 23961

#Barplot
barplot(table(Purchase), border = c("darkgreen"), main="Purchase made in $ by shoppers", xlab = "Amount", ylab="Frequency of people")
#Hardly a shopper spend above $19000
#Shoppers mostly spent an amount of approximately 6800 or 8700 as they got highest peak in barplot

#Histogram
#We plot histogram with increasing breaks to analyse data.
# breaks = 10
hist(Purchase,breaks=10, xlim=c(185,25000),col="darkorange", main="Purchase made in $ by shoppers(Breaks=10)", xlab="Amount", ylab="Frequency of people")
#Break=10 We see max data lies between 5000-10000, increase break to 20"
 
#breaks = 20
hist(Purchase,breaks=20, xlim=c(185,25000),col="pink", main="Purchase made in $ by shoppers(Breaks=20)", xlab="Amount", 
ylab="Frequency of people")
#We can see there are some figures which are not at all spent and good amount is spent near 15000 and b/w 5000-10000, increasing break=40"

# breaks = 40
hist(Purchase,breaks=40, xlim=c(185,25000),col="", main="Purchase made in $ by shoppers(Breaks=40)", xlab="Amount", ylab="Frequency of people")
#We can now clearly see how much figures people spent
#If a shopper is coming to black friday sale there are maximum chances, he would be spending on an average at least $5000
#Maximum shoppers population lie across $5000 mark
#We may consider that people didn't spent in $9000 or $17000(avg of 15K & 20K) in sales.

#Boxplot
f = fivenum(Purchase)
oulier = c(f[2]-1.5*(f[4]-f[2]) , f[4]+1.5*(f[4]-f[2]))
boxplot(f,horizontal = TRUE, xaxt="n", xlab="Amount", col="yellow", main="Purchase made in $ by shoppers")
axis(side = 1, at = f, labels=TRUE)
text(f,srt=90, rep(1.2,5), adj=0,labels=c("Min", "Lower Hinge", "Mean","Upper Hinge", "Max"))
#We can consider an average shopper will spend $5866-$12073 in black friday sales

###########The top 5 purchased product.###########
Most_sold <- BlackFriday %>% group_by(Product_ID) %>% summarise(count=n()) %>% arrange(desc(count))
top_5 <- Most_sold[1:5, ]
names(top_5) <- c('Product_ID', 'Quantity')
ggplot(top_5,aes(factor(
  Product_ID,
  level = c('P00110742','P00184942','P00025442','P00112142','P00057642')), Quantity)) + 
  geom_bar(stat = 'identity',fill = c('#c4d8ba', '#d8ceba', '#bac4d8', '#e1daae', '#fa5845'))
+ xlab('Product_ID') + theme_bw()


#########The top 5 products that brought in the most revenue.############

Revenue <- BlackFriday %>% group_by(Product_ID) %>% summarise(revenue = sum(Purchase)) %>% arrange(desc(revenue))
top_5_revenue <- Revenue[1:5, ]
names(top_5_revenue) <- c('Product_ID', 'Revenue')
ggplot(top_5_revenue,aes(factor(Product_ID,level = c('P00110742','P00184942','P00025442','P00112142','P00059442')), Revenue)) + geom_bar(stat = 'identity',fill = c('#c4d8ba', '#d8ceba', '#bac4d8', '#e1daae', '#fa5845')) + xlab('Product_ID') + theme_bw()


####The target variable being the amount of Purchase, the analysis begins with its distribution.#####

ggplot(BlackFriday, aes(Purchase)) + 
  geom_histogram(aes(y=..density..), fill="turquoise") +
  geom_density(alpha=1, color = '#49a4aa', size = 1.2) + theme_bw() + xlab('Amount Spent by each buyer') + ylab('Number of buyers') + ggtitle('Purchase Distribution') +  theme(plot.title = element_text(hjust = 0.5))

#Correlation between numerical predictors and Purchase
data <- BlackFriday[, c(1, 5, 9, 10, 11)]
res <- cor(data, use = "complete.obs")
res <- round(res, 2)
corrplot(res, tl.col = 'black', tl.cex = .7, tl.srt = 45) 
#category 1,2,3 are strong.Hence, product_category 2 and 3 are dropped to avoid multicolinearity.

######### FOR Marital status.#########
ggplot(BlackFriday ,aes(Marital_Status)) + geom_bar(fill= c('pink','cyan')) # 0=single, 1=Married

table(BlackFriday$Marital_Status)#there are 32126 more single customers than couples.

by_mar <- BlackFriday %>% group_by(Marital_Status) %>% summarise(Purchase = mean(Purchase))
ggplot(by_mar, aes(Marital_Status, Purchase)) + geom_bar(stat = 'identity', fill = c('pink', 'cyan'))
#Altough we had more single customers, couples spend as much as single customers on average.

######### FOR occupation#########
ggplot(BlackFriday, aes(Occupation)) + geom_bar(fill= 'light blue') + theme_classic()  #Occupation 0,4,7 are most noticeable among customers.

by_occ <- BlackFriday %>% group_by(Occupation) %>% summarise(Spending = mean(Purchase))
ggplot(by_occ, aes(Occupation, Spending)) + geom_bar(stat='identity', fill= 'light blue')#The difference in average purchase accross occupations is small.

#######For Stay_In_Current_city_Year##
temporary = Stay_In_Current_City_Years # keeping copy of data in a temporary variable
Stay_In_Current_City_Years = table(Stay_In_Current_City_Years)
# GEOMETRIC DITRIBUTION
df.stay.in.current.city = data.frame(Stay_In_Current_City_Years)
df.stay.in.current.city$probability = df.stay.in.current.city$Freq/sum(df.stay.in.current.city$Freq)
options(digits = 2)
#Probability that the person I picked have stayed 5 years in current city
"First person"
dgeom(0, prob=df.stay.in.current.city$probability[5])*100
#15

"Second person"
dgeom(1, prob=df.stay.in.current.city$probability[5])*100
#13

# checking for 10 persons
pmf.10 = dgeom(0:9, prob=df.stay.in.current.city$probability[5])

person.stay.5.year = data.frame(person.count = seq(1:10), probability = pmf.10,percentage = pmf.10 * 100)
person.stay.5.year
#we can see the probability percentage of 10 people
# plotting graph
plot(0:9,pmf.10,type="h",col="green",main="Choosed a person who spent 5 years in current city out of 10 trials",xlab="Person Count", ylab="PMF/ Probability",pch=16)
abline(h=0)

#Barplot
Stay_In_Current_City_Years = round(Stay_In_Current_City_Years/1000)
barplot(Stay_In_Current_City_Years,col="pink3",xlab="Number of Years spent",ylab="Frequency",main="Black friday sales | Years (Value in 1000s)")  
#Most of the people have spent at least 2 years in the city where the survey took place

# PIECHART
Stay_In_Current_City_Years.labels = c("1 Year","2 Years","3 Years","4 Years","5 Years")
Stay_In_Current_City_Years.percent = round(Stay_In_Current_City_Years/sum(Stay_In_Current_City_Years)*100)
Stay_In_Current_City_Years.labels = paste(Stay_In_Current_City_Years.labels, Stay_In_Current_City_Years.percent)
Stay_In_Current_City_Years.labels = paste(Stay_In_Current_City_Years.labels, "%", sep="")
#Ploting a pie chart for years
pie(Stay_In_Current_City_Years, labels = Stay_In_Current_City_Years.labels, col=rainbow(5), main="BlackFriday | stay year wise shoppers distribution")
#From pie chart we can say that 50% of popuation have spend atmost 2 years and 50% more than 2 years  

#purchase of those people
by_stay <- BlackFriday %>% group_by(Stay_In_Current_City_Years) %>% summarise(Purchase = mean(Purchase))
ggplot(by_stay, aes(Stay_In_Current_City_Years, Purchase)) + geom_bar(stat = 'identity',fill = c('#c4d8ba','#d8ceba','#bac4d8','#6f2859','#abe6fa'))
#Same goes for the duration of stay. 
#Regardless of the duration the average spend was more or less the same eventhough new settlers were the group that was most likely to take advantage of Black Friday.




#Spliting data into train(70%) and test(30%)

set.seed(123) # set seed to ensure you always have same random numbers generated
ind <- sample(2,nrow(BlackFriday),replace = TRUE,prob = c(0.7,0.3))
train <- BlackFriday [ind==1,]
test <- BlackFriday [ind==2,]

########Preparing the Multi Regression Model##
options(max.print = 999999)
str(train)
str(test)

#changing the variables to factors

train$Occupation <- factor(train$Occupation)
train$Marital_Status <- factor(train$Marital_Status)
train$Product_Category_1 <- factor(train$Product_Category_1)
train$Product_Category_2<- factor(train$Product_Category_2)
train$Product_Category_3<-factor(train$Product_Category_3)

test$Occupation <- factor(test$Occupation)
test$Marital_Status <- factor(test$Marital_Status)
test$Product_Category_1 <- factor(test$Product_Category_1)
test$Product_Category_2<- factor(test$Product_Category_2)
test$Product_Category_3<-factor(test$Product_Category_3)

#creating a training and validation subsets
set.seed(1)
index <- sample(1:nrow(train), nrow(train)*0.8)
training<- train[index,]
validation<- train[-index,]

#feature engineering

training$Product <- 3 - is.na(training$Product_Category_2)- is.na(training$Product_Category_3)
validation$Product <- 3 - is.na(validation$Product_Category_2)- is.na(validation$Product_Category_3) 
test$Product <- 3 - is.na(test$Product_Category_2)- is.na(test$Product_Category_3)
val_actual<-validation$Purchase
head(val_actual)
length(val_actual)

###Running the model Linear Regression 

model_lm <- lm(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                 Marital_Status+Product_Category_1+Product , data = training)
summary(model_lm)

#r-square = 0.48 RMSE = 3663

plot(model_lm,col= "brown")

#predicting the validation dataset 
pred_lm <- predict(model_lm, validation)
pred_lm
results <- cbind(pred_lm,validation$Purchase)
colnames(results) <- c('pred','real')
head(results)

length(pred_lm)


#rmse value checking for validation dataset
rmse <- sqrt(mean((pred_lm - val_actual)^2))
rmse #3672.93

#predicting on the test dataset 
pred_test_lm <- predict(model_lm, test)
test_sub_lm<- data.frame(test$User_ID,test$Product_ID,pred_test_lm,test$Purchase)
head(test_sub_lm)
str(training)




#######################################################

