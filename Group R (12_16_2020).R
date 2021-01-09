data.df <- read.csv("group.csv")

head(data.df, 3)
#Row.ID      Order.ID Order.Date Ship.Date      Ship.Mode Customer.ID   Customer.Name  Segment        City           State   Country Postal.Code Market
#1  42433  AG-2011-2040   1/1/2011  6/1/2011 Standard Class    TB-11280 Toby Braunhardt Consumer Constantine     Constantine   Algeria          NA Africa
#2  22253 IN-2011-47883   1/1/2011  8/1/2011 Standard Class    JH-15985     Joseph Holt Consumer Wagga Wagga New South Wales Australia          NA   APAC
#3  48883  HU-2011-1220   1/1/2011  5/1/2011   Second Class      AT-735   Annie Thurman Consumer    Budapest        Budapest   Hungary          NA   EMEA

#Region       Product.ID        Category Sub.Category             Product.Name   Sales Quantity Discount  Profit Shipping.Cost Order.Priority
#1  Africa OFF-TEN-10000025 Office Supplies      Storage      Tenex Lockers, Blue 408.300        2      0.0 106.140         35.46         Medium
#2 Oceania  OFF-SU-10000618 Office Supplies     Supplies Acme Trimmer, High Speed 120.366        3      0.1  36.036          9.72         Medium
#3    EMEA OFF-TEN-10001585 Office Supplies      Storage  Tenex Box, Single Width  66.120        4      0.0  29.640          8.17           High

variable.names(data.df)
#[1] "Row.ID"         "Order.ID"       "Order.Date"     "Ship.Date"      "Ship.Mode"      "Customer.ID"    "Customer.Name"  "Segment"       
#[9] "City"           "State"          "Country"        "Postal.Code"    "Market"         "Region"         "Product.ID"     "Category"      
#[17] "Sub.Category"   "Product.Name"   "Sales"          "Quantity"       "Discount"       "Profit"         "Shipping.Cost"  "Order.Priority"

library(dplyr)
library(tidyr)
library(moments)
library(boot)
library(rfm)
library(car)
library(corrplot)# correlation plot
#install.packages('arules')
#install.packages('arulesViz')
library(arules)
library(arulesViz)


##################################################################################################################################
#First block: predictive model
#Separate customers by their values. 
#Who they are? Why are they different?
#Organize questions around story block
#Develop a method to calculate CLV
#Build a linear regression model why some people have high and low CLV
#RFM model, Past value, discount rate, 4 years of data

#CLV
variable.names(data.df)
total_profit <- data.df %>%
  group_by(Customer.Name) %>%
  summarise(total_profit = sum(Profit),
            total_discount = sum(Discount),
            total_sales = sum(Sales))
total_profit

data.df <- total_profit %>% left_join(data.df)
head(data.df,20)
str(data.df)

#changing date from character to date structure
a <- as.Date(data.df$Order.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Order.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
data.df$Order.Date <- a

summary(data.df$Order.Date)
rfm.df<-data.df

#define frequency
frequency <- rfm.df %>%count(Customer.Name) 
frequency
rfm.df <- frequency %>% left_join(rfm.df)

#define recency
recency <- rfm.df %>%
  group_by(Customer.Name) %>%
  summarise(last_date = max(Order.Date))
rfm.df <- recency %>% left_join(rfm.df)
variable.names(rfm.df)
rfm.df %>% mutate(frequency=n,recency=last_date,monetary=total_profit)->rfm.df
head(rfm.df)
summary(rfm.df$frequency)
rfm.df %>% mutate(Findex=cut(frequency,breaks=c(28,58,67,77,109),labels=c("1","2","3","4")))->rfm.df
summary(rfm.df$recency)
date1 <- as.Date("2015-01-01", tz="UTC")

as.numeric(difftime(date1, rfm.df$recency, units="days"))->rfm.df$Rdays
summary(rfm.df$Rdays)
rfm.df %>% mutate(Rindex=cut(frequency,breaks=c(0,7,17,36,430),labels=c("1","2","3","4")))->rfm.df             
summary(rfm.df$total_profit)
rfm.df %>% mutate(Mindex=cut(frequency,breaks=c(-6153,1040,1834,2673,8674),labels=c("1","2","3","4")))->rfm.df
variable.names(rfm.df)

# you can combine these three indices in anyway you want; to segment your market
# for example, have a linear combination
rfm.df %>% mutate(Customer.Lifetime.Value=20*as.numeric(Findex)/5+40*as.numeric(Rindex)/5+40*as.numeric(Mindex)/5)->rfm.df
summary(rfm.df$Customer.Lifetime.Value)
variable.names(rfm.df) #CLV added


#Anova test - CLV between countries has significant difference

model1.aov<-aov(Customer.Lifetime.Value~Country,data=rfm.df)
summary(model1.aov)# P<o.o5
plot(model1.aov,1) # check outliter
library(car)
rfm.df$Country=as.factor(rfm.df$Country)
leveneTest(Customer.Lifetime.Value~Country,data=rfm.df) #p<0.05, equal variance
TukeyHSD(model1.aov)
summary(model1.aov)

#Anova test - CLV between market has no significant difference

model2.aov<-aov(Customer.Lifetime.Value~Market,data=rfm.df)
summary(model2.aov)# P>o.o5 - accept null, there is no difference
library(car)
rfm.df$Market=as.factor(rfm.df$Market)
leveneTest(Customer.Lifetime.Value~Market,data=rfm.df) #p<0.05, variance is not equal
# levene test assumes an equal variance. if p < 0.05, reject.
# thus, oneway test instead of Anova
oneway.test(Customer.Lifetime.Value~Market,data=rfm.df)
summary(model2.aov)
variable.names(rfm.df)


#linear regression
#Predictive model- sales, country, category, subcategory
# sales- product level/ customers

rfm.df %>% group_by(Market)%>% summarise(avg.Shipping.Cost=mean(Shipping.Cost),N=n(),
                                         avg.Discount=mean(Discount),
                                         avg.Sales=mean(Sales),)->data.mo1

cor(data.mo1[c('avg.Shipping.Cost','avg.Discount','avg.Sales')]) 

corrplot.mixed(corr=cor(data.mo1[c('avg.Shipping.Cost','avg.Discount','avg.Sales')], use="complete.obs"),
               upper="ellipse", tl.pos="lt")

#step 3 model building
Sales.m1<-lm(avg.Sales~avg.Shipping.Cost+avg.Discount, data=data.mo1)
summary(Sales.m1)# Adj R-squared: 0.9907
# category variables-as factor to change the data type

# remove avg.Discount -> increased adj R-squared to 0.9925
Sales.m2<-lm(avg.Sales~avg.Shipping.Cost, data=data.mo1)
summary(Sales.m2)

variable.names(rfm.df)
library(corrplot)
cor(rfm.df[c('Customer.Lifetime.Value','Shipping.Cost','Discount')]) 

corrplot.mixed(corr=cor(rfm.df[c('Customer.Lifetime.Value','Shipping.Cost','Discount')], use="complete.obs"),
               upper="ellipse", tl.pos="lt")
#step 3 model building
CLV.m1<-lm(Customer.Lifetime.Value~Discount, data=rfm.df)
summary(CLV.m1)# Adj R-squared:-3.606e-06 -> low quality model
# p>0.05, discount is insignificant. Find better model.

CLV.m2<-lm(Customer.Lifetime.Value~as.factor(Region), data=rfm.df)
summary(CLV.m2)# R-squared: 0.0003186 -> low quality model
# significant variables: Canada,  North Asia 
# there is a significant difference in CLV if customer is from Canada or North Asia

mean(rfm.df$Customer.Lifetime.Value,na.rm=TRUE)


#logistic regression, based on CLV,  >40 high-low  customers 1, and 0
#run when dependent variable (CLV) is binary  
mean(rfm.df$Customer.Lifetime.Value,na.rm=TRUE)# the average of CLV #49.64547
rfm.df$CLVgroup = "NA"
rfm.df$CLVgroup [which(rfm.df$Customer.Lifetime.Value > 50)] = "High"
rfm.df$CLVgroup [which(rfm.df$Customer.Lifetime.Value < 50)] = "Low"
rfm.df$CLVgroup  <- factor(x =rfm.df$CLVgroup, 
                           levels = c("High", "Low"))

rfm.df %>% mutate(prob=ifelse(CLVgroup=="High",1,0))->rfm.df
table(rfm.df$prob)

#   0     1 
# 26654 24636 


# split data into training dataset 70% and validation 30%
# Training = sample used to fit the model
# Validation = provides unbiased evaluation of training model and improves hyperparameters (value used to control mahchine learning process) 
dt=sort(sample(nrow(rfm.df),nrow(rfm.df)*.7))# dt is a list of row number
train<-rfm.df[dt,]
val<-rfm.df[-dt,]#get validation 30%
#check number of rows in training and validation data sets to confirm 30/70 split
nrow(train)
nrow(val)

#Run Logistic Regression with ever variable
CLV.m4<-glm(prob~Discount+as.factor(Region)+as.factor(Order.Priority), 
            data=train,family=binomial)#function code, binomial 
summary(CLV.m4)


#refine your model
# Stepwise Logistic Regression
# Stepwise algorithm to choose a model based on AIC
CLV.m5<-step(CLV.m4)
CLV.m6<-glm(prob~as.factor(Discount), data=train,family=binomial)
#swapped CLV.m6<-glm(prob~as.factor(Region), data=train,family=binomial) as lowest AIC = discount
summary(CLV.m6)
exp(coef(CLV.m6))


#TABLE CELLS

  #linear regression
library(dplyr)
Table.sale <- filter(rfm.df, Sub.Category == 'Tables')

Table.sale %>% group_by(year) %>% summarise(sum.Profit=sum(Profit),N=n())
variable.names(Table.sale)
library(corrplot)
cor(Table.sale[c('Shipping.Cost','Discount','Sales','Profit')]) 

corrplot.mixed(corr=cor(rfm.df[c('Shipping.Cost','Discount','Sales','Profit')], use="complete.obs"),
               upper="ellipse", tl.pos="lt")

  #step 3 model building
tableprofit.m1<-lm(Profit~Discount+Shipping.Cost+Sales, data=Table.sale)
summary(tableprofit.m1) #Adj R-squared: 0.4618

  #Discount is significant variable
tableprofit.m2<-lm(Profit~Discount+as.factor(Country), data=Table.sale)
summary(tableprofit.m2)#R-squared:  0.5328

#category variables-as factor to change the data type
rfm.df$Row.ID<-as.factor(rfm.df$Row.ID)
rfm.df$Order.ID<-as.factor(rfm.df$Order.ID)
rfm.df$Order.Date<-as.factor(rfm.df$Order.Date)
rfm.df$Ship.Date<-as.factor(rfm.df$Ship.Date)
rfm.df$Ship.Mode<-as.factor(rfm.df$Ship.Mode)
rfm.df$Customer.ID<-as.factor(rfm.df$Customer.ID)
rfm.df$Customer.Name<-as.factor(rfm.df$Customer.Name)
rfm.df$Segment<-as.factor(rfm.df$Segment)
rfm.df$City<-as.factor(rfm.df$City)
rfm.df$State<-as.factor(rfm.df$State)
rfm.df$Country<-as.factor(rfm.df$Country)
rfm.df$Postal.Code<-as.factor(rfm.df$Postal.Code)
rfm.df$Market<-as.factor(rfm.df$Market)
rfm.df$Region<-as.factor(rfm.df$Region)
rfm.df$Product.ID<-as.factor(rfm.df$Product.ID)
rfm.df$Category<-as.factor(rfm.df$Category)
rfm.df$Sub.Category<-as.factor(rfm.df$Sub.Category)
rfm.df$Product.Name<-as.factor(rfm.df$Product.Name)
rfm.df$Sales<-as.factor(rfm.df$Sales)
rfm.df$Quantity<-as.factor(rfm.df$Quantity)
rfm.df$Discount<-as.factor(rfm.df$Discount)
rfm.df$Profit<-as.factor(rfm.df$Profit)
rfm.df$Shipping.Cost<-as.factor(rfm.df$Shipping.Cost)
rfm.df$Order.Priority<-as.factor(rfm.df$Order.Priority)

variable.names(rfm.df)
str(rfm.df)

head(rfm.df)

##################################################################################################################################
#Second Block: predictive model
#How product categories are different from each other
#What product categories are profitable? Why?
#Check on the popularity of the product category
#Does the popularity differ by year?
#T-test 

#1. Category$Sales
data.df %>% group_by(Category) %>% summarise(mean.Sales=mean(Sales),N=n())
#1 Furniture             416.  9876
#2 Office Supplies       121. 31273
#3 Technology            468. 10141
#Office Supplies sells more, despite having the lowest sales
#The most profitable is Technology, with decent sales and the highest average sales amount 

#2. Sub.Category$Sales
data.df %>% group_by(Category, Sub.Category) %>% summarise(mean.Sales=mean(Sales),N=n())

#1 Furniture       Bookcases         608.   2411
#2 Furniture       Chairs            437.   3434
#3 Furniture       Furnishings       122.   3170
#4 Furniture       Tables            879.    861
#5 Office Supplies Appliances        576.   1755
#6 Office Supplies Art                76.2  4883
#7 Office Supplies Binders            75.1  6152
#8 Office Supplies Envelopes          70.2  2435
#9 Office Supplies Fasteners          34.4  2420
#10 Office Supplies Labels             28.2  2606
#11 Office Supplies Paper              69.0  3538
#12 Office Supplies Storage           223.   5059
#13 Office Supplies Supplies          100.   2425
#14 Technology      Accessories       244.   3075
#15 Technology      Copiers           679.   2223
#16 Technology      Machines          524.   1486
#17 Technology      Phones            508.   3357


#3. Country$Sales
data.df %>% group_by(Region) %>% summarise(mean.Sales=mean(Sales),N=n())

#1 Africa               171.  4587
#2 Canada               174.   384
#3 Caribbean            192.  1690
#4 Central              254. 11117
#5 Central Asia         368.  2048
#6 East                 238.  2848
#7 EMEA                 160.  5029
#8 North                261.  4785
#9 North Asia           363.  2338
#10 Oceania              316.  3487
#11 South                241.  6645
#12 Southeast Asia       283.  3129
#13 West                 226.  3203


#4. Profit$Year
data.df$Order.Date<-as.character(data.df$Order.Date)
right = function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}
right(data.df$Order.Date,4)->data.df$year

table(data.df$year)
#2011  2012  2013  2014 
#8998 10962 13799 17531 

data.df %>% group_by(year) %>% summarise(mean.Profit=mean(Profit),N=n())

#year   mean.Profit     N
#<chr>          <dbl> <int>
#1 2011         27.7  8998
#2 2012         28.0 10962
#3 2013         29.5 13799
#4 2014         28.8 17531


##################################################################################################################################
#Third Block: Descriptive block
#Use visual to present the ideas
#How customers represent each market
#Data wrangling
#Summarise 

#check NA
which(is.na(data.df))

#duplicates?
which(duplicated(data.df))

#5 Summary #
data.df %>% select(Sales, Quantity, Profit, Shipping.Cost) %>% summary()

#add more existing variables from original data
rfm.df %>% left_join(data.df,by="Customer.ID")->newcustomer.df
####left_join() return all rows from x , and all columns from x and y####
variable.names(newcustomer.df)
variable.names(data.df)

table(newcustomer.df$Market.x)->customer.market.table #how many customers by market
prop.table(customer.market.table)*100
round(prop.table(customer.market.table)*100,2)
pie(customer.market.table)

#how many customers by country
table(newcustomer.df$Country.x)->customer.country.table
prop.table(customer.country.table)*100->prop   #formatting to percentage
prop
round(prop.table(customer.country.table)*100,2)
pie(customer.country.table, main = "Pie chart of country") 

#customers by market-percentage
data.df %>% group_by(Market) %>% summarise(N=n())
table(data.df$Market)->customer.market.table
prop.table(customer.market.table)*100->prop
prop
round(prop.table(customer.market.table)*100,2)
pie(customer.market.table,main="Pie chart of market")

# customers by segment-percentage
data.df %>% group_by(Segment) %>% summarise(N=n())
table(data.df$Segment)->customer.segment.table
prop.table(customer.segment.table)*100->prop
prop
round(prop.table(customer.segment.table)*100,2)
pie(customer.segment.table,main="Pie chart of segment")

#average shipping costs by markets 
data.df %>% group_by(Market) %>% summarise(avg.Shipping.Cost=mean(Shipping.Cost),N=n(),
                                           avg.Discount=mean(Discount),N=n(),
                                           avg.Profit=mean(Profit),N=n())

#average shipping costs by categories and segments 
data.df %>% group_by(Category,Segment) %>% summarise(avg.Shipping.Cost=mean(Shipping.Cost),N=n(),
                                                     avg.Discount=mean(Discount),N=n(),
                                                     avg.Profit=mean(Profit),N=n())

# group countries? subset of your data
newcustomer.df %>% group_by(Country.x) %>% summarise(N=n())    #sample size by each group
newcustomer.df %>% group_by(Country.x) %>% filter(n()>=500)->big.country.df    #filter customers of each country > 500
big.country.df %>% group_by(Country.x) %>% summarise(total=mean(Sales),N=n())   #mean of the sales

#Segment customer numbers?
table(newcustomer.df$Segment.x)
newcustomer.df %>% filter(Segment.x=="Consumer")->consumer.df

summary(data.df$Order.Date)
a <- as.Date(data.df$Order.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Order.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
data.df$Order.Date <- a # Put it back in your dataframe

#double check the format is unified
summary(data.df$Order.Date)
data.df$days<-weekdays(data.df$Order.Date)
table(data.df$days)
data.df$year<-substring(data.df$Order.Date,1,4)
data.df$month<-substring(data.df$Order.Date,6,7)
table(data.df$month)
data.df$dates<-substring(data.df$Order.Date,9,10)
table(data.df$dates)
variable.names(data.df)

# graphically showing sales change by time
library(ggplot2)
ggplot(aes(x=days,y=Sales),data=data.df)+geom_point()
ggplot(aes(x=dates,y=Sales),data=data.df)+geom_point()
ggplot(aes(x=month,y=Sales),data=data.df)+geom_line()
ggplot(aes(x=Market,y=Sales,fill=Market),data=data.df)+geom_bar(stat="identity")

  #EDA
ggplot(data.df,aes(Sub.Category,fill=Sub.Category)) + geom_bar() + labs(title="Sub Category count")
ggplot(data.df,aes(Market,fill=Market)) + geom_bar() + labs(title="Market wise Order count") 
ggplot(data.df,aes(Order.Priority,fill=Order.Priority)) + geom_bar() + labs(title="Orders based on priority") 


##################################################################################################################################
#Fourth Block: Market basket analysis
#How different categories or subcategories are purchased together more often

arm <- read.csv("group.csv")
dim(arm)  #51290 rows, 24 Cols, Row.ID is the rowid column from excel. It is not available in Data Dictionary, so we remove it.
head(arm)
arm <- arm[,-1]
dim(arm)  #51290 rows, 23 Cols, Order.ID is the key column now.
str(arm) #Most of the cols are character/ Numeric data type. We want to focus on sub-category.
summary(arm)
nrow(unique(arm)) #When grouped together, all the records are unique. Number of records - 51290

#Lets find unique order id count
length(unique(arm$Order.ID)) 
length(unique(arm$Sub.Category)) #17 unique sub category
#25035 unique Order.ID. Meaning, multiple products of same/different subcategories were ordered on the same
#day with same order ID.

#Now we convert the above data frame to transactional format. 
#To do this, we use read.transactions. 
#First, we need to store the mini data frame(using only the order ID and sub-category columns) into to a csv file.

arm_mini <- arm[,c("Order.ID","Sub.Category")]
head(arm_mini) 
write.csv(arm_mini,"transdata",row.names = F) #forcing the data frame into a csv, and taking off row number
transdata<-read.transactions(file="transdata",format="single",sep=",",cols=c("Order.ID","Sub.Category"), rm.duplicates = T, header = T)

arm_transactions<-as(transdata,"transactions")
summary(arm_transactions)
  
#transactions as itemMatrix in sparse format with
  #25035 rows (elements/itemsets/transactions) and
  #17 columns (items) and a density of 0.1113805          

#most frequent items (sub-category):
  #Binders Storage     Art   Paper  Chairs (Other) 
    #5392    4534    4366    3234    3187   26690 

#element (itemset/transaction) length distribution:
  #sizes (sub-category)
#    1     2     3     4     5     6     7     8     9    10    11 
#12800  6469  3193  1484   626   304   101    42     9     5     2      # of order
  

  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   1.000   1.893   2.000  11.000      #most of the case, customers will buy 1 or 2 sub-cate per transaction


#visually showing most frequent items
itemFrequencyPlot(arm_transactions,topN=10,type="absolute")

#rule generation
#experimenting with different thresholds
arm.rules <- apriori(arm_transactions, parameter=list(supp=0.0004, conf=0.5, 
                                                      target="rules"))
arm.rules <- apriori(arm_transactions, parameter=list(supp=0.0005, conf=0.5, 
                                                      target="rules"))
arm.rules <- apriori(arm_transactions, parameter=list(supp=0.0006, conf=0.5, 
                                                      target="rules"))
arm.rules <- apriori(arm_transactions, parameter=list(supp=0.00005, conf=0.5, 
                                                      target="rules"))
#too extreme, as we want the occurrence of the set that we hope to see
#only 17 items so many transactions and baskets get involved 
#so average support level is very low
#want to be very confident bc fewer numbers, so we use conf = 0.5

#best result
arm.rules <- apriori(arm_transactions, parameter=list(supp=0.0005, conf=0.5, 
                                                      target="rules"))
inspect(arm.rules)
summary(arm.rules) # 22 rules

#inspecting
top_conf<-head(arm.rules, n=10, by= "confidence")
top_conf<-as(top_conf,"data.frame")   #sort by confidence
top_conf

top_lift<-head(arm.rules, n=10, by= "lift")
top_support<-head(arm.rules, n=10, by= "support")

inspect(top_conf)
inspect(top_lift)
inspect(top_support)

group.hi<-head(sort(arm.rules, by= "lift"),10)
plot(group.hi,method="graph",control=list(type="items"))
#Dark color means stronger relationship, higher lift
#Big circle means higher support level for each rule
