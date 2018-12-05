library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(MASS)
#install.packages('car')
library(car)

#cars <- read.csv('E:/ml/UG/PA/LinearRegression_CaseStudy/CarPrice_Assignment.csv')
cars <- read.csv('CarPrice_Assignment.csv')

# Verify number of rows and structure of the dataset
nrow(cars)
str(cars)

# 205 observations

#Check if there are any duplicate values
n_distinct(cars)
# no duplicate rows

sum(is.na(cars))

# no missing values in the data set


# Data Preperation

# extract the company from the CarName variable
# Let us convert company names into lowercase for consistency 
cars$CarName <- tolower(cars$CarName)
#Extract company names from the CarName variable
cars$Company <- str_extract(cars$CarName,pattern = '\\S+')

table(cars$Company)

# There are some inconsistancy in the names of the company. Lets correct it.
cars$Company <- replace(cars$Company,which(cars$Company == 'vw' | cars$Company == 'vokswagen'),'volkswagen')
cars$Company <- replace(cars$Company,which(cars$Company == 'porcshce'),'porsche')
cars$Company <- replace(cars$Company,which(cars$Company == 'maxda'),'mazda')
cars$Company <- replace(cars$Company,which(cars$Company == 'toyouta'),'toyota')

# Dropping the Original CarName variable and the ID variable which are either a duplicate info or a just a random number

cars <- cars[,!(names(cars) %in% c('CarName','car_ID'))]

# Convert door number variable to numeric type.

cars$doornumber <- gsub('two',2,cars$doornumber)
cars$doornumber <- gsub('four',4,cars$doornumber)
cars$doornumber <- as.numeric(cars$doornumber)

# Convert cylinder number variable  to numeric type.
cars$cylindernumber <- gsub('eight',8,cars$cylindernumber)
cars$cylindernumber <- gsub('five',5,cars$cylindernumber)
cars$cylindernumber <- gsub('four',4,cars$cylindernumber)
cars$cylindernumber <- gsub('six',6,cars$cylindernumber)
cars$cylindernumber <- gsub('three',3,cars$cylindernumber)
cars$cylindernumber <- gsub('twelve',12,cars$cylindernumber)
cars$cylindernumber <- gsub('two',2,cars$cylindernumber)

# convert to numeric values

cars$cylindernumber <- as.numeric(cars$cylindernumber)

# From the business understanding, we know that -2 is a prety good rating compared to 3. 
# Multiplying with -2 will make -ve values as postive and postive as -ve.

cars$symboling <- cars$symboling * -2


# Derive Variable to get POWER TO WEIGHT RATIO 
# POWER TO WEIGHT RATIO is a measurement of actual performance of any engine or power source
# https://en.wikipedia.org/wiki/Power-to-weight_ratio

cars$power_to_weight <- cars$horsepower/cars$curbweight

# Rounding it of to 2 decimal places
cars$power_to_weight <- round(cars$power_to_weight,2)


########################## Exploratory Data Analysis #############################

# symboling

ggplot(cars,aes(x=factor(symboling),fill=factor(symboling)))+
  geom_bar()+
  labs(title='Symboling Frequency', 
       x='Symboling',
       y='Count',
       fill="Symboling")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
  
# Our data has more observations with less symboling rating.

# fuel type
ggplot(cars,aes(x=fueltype,fill=factor(fueltype)))+
  geom_bar()+
labs(title='Fuel Type frequqncy', 
     x='FuelType',
     y='Count',
     fill="FuelType")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")

# our Data has more obervations with gas fuel type.


#aspiration

ggplot(cars,aes(x=aspiration,fill=factor(aspiration)))+
  geom_bar()+
  labs(title='Aspiration type engine frequency', 
       x='aspiration',
       y='Count',
       fill="Aspiration")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# There are more number of "std" type engines.

#doornumber

ggplot(cars,aes(x=factor(doornumber),fill=factor(doornumber)))+
  geom_bar()+
  labs(title='Doornumber frequency', 
       x='Door Numbers',
       y='Count',
       fill="Door Number")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# There are more number of observations with 4 door cars which makes sense in general.

# carbody

ggplot(cars,aes(x=carbody,fill=factor(carbody)))+
  geom_bar()+
  labs(title='Carbody frequency', 
       x='Car body types',
       y='Count',
       fill="Car Body")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")

# Most of the observations are of Sedan and Hatchback type.

# drivewheel		
ggplot(cars,aes(x=drivewheel,fill=factor(drivewheel)))+
  geom_bar()+
  labs(title='Drive Wheel frequency', 
       x='Driver Wheel type',
       y='Count',
       fill="Drive Wheel")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# More number of observations are of forward wheel and rare wheel drive.

# enginelocation

ggplot(cars,aes(x=enginelocation,fill=factor(enginelocation)))+
  geom_bar()+
  labs(title='Engine Location Frequency', 
       x='Engine Location',
       y='Count',
       fill="Engine Location")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# Data with respect to Engine location is not balanced. Only 3 observations of cars with rear engine


# wheelbase

ggplot(cars,aes(x="",y=wheelbase))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Wheel Base distribution ', 
       y='Wheel Base')

# Two outlier observations in the data. Median value around 97. 


# carlength

ggplot(cars,aes(x="",y=carlength))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Car Length distribution', 
       y='Car Length')

# One Outlier below lower whisker. Median ca length around 172

# carwidth	

ggplot(cars,aes(x="",y=carwidth))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Car Width distribution', 
       y='Car Width')
# 4 Outliers above the whisker.



# carheight		

ggplot(cars,aes(x="",y=carheight))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Car Height distribution', 
       y='Car Height')

# No Outliers in the data. Median value around 53

# curbweight

ggplot(cars,aes(x="",y=curbweight))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Car Weight distribution', 
       y='Car Weight')
# No Outliers in the Car Weight

# enginetype		

ggplot(cars,aes(x=enginetype,fill=enginetype))+
  geom_bar()+
  labs(title='Engine Type Frequency', 
       x='Engine Type',
       y='Count',
       fill="Engine Type")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# More number of observations are of "ohc" engine type.

# cylindernumber
ggplot(cars,aes(x=factor(cylindernumber),fill=factor(cylindernumber)))+
  geom_bar()+
  labs(title='Cylinder Number', 
       x='Number of Cylinder',
       y='Count',
       fill="Cylinder Number")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# More number of observation with 4 Cylinder

# enginesize

ggplot(cars,aes(x="",y=enginesize))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Engine Size distribution', 
       y='Engine Size')
# Data has outliers w.r.t to engine size.

# fuelsystem

ggplot(cars,aes(x=fuelsystem,fill=fuelsystem))+
  geom_bar()+
  labs(title='Fuel System Frequency', 
       x='Fuel System Type',
       y='Count',
       fill="Fuel System")+
  geom_text(aes(label=(..count..),vjust=-0.5),stat="count")
# More number of Observations of fuel system type 2bbl and mpfi

# boreratio

ggplot(cars,aes(x="",y=boreratio))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Bore Ration Distribution', 
       y='Bore Ratio')
# No Outlier in the data w.r.t bore ratio

# stroke		

ggplot(cars,aes(x="",y=stroke))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Stroke Distribution', 
       y='Stroke')
# some outliers below and above lower and upper whiskers.

# compressionratio
ggplot(cars,aes(x="",y=compressionratio))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Compression ration Dist', 
       y='Compression Ratio')
# Huge Outliers in the compression ratio.

# horsepower
ggplot(cars,aes(x="",y=horsepower))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Horse Power Dist', 
       y='Horse Power')

# 4 outliers in the horspower distribution


# peakrpm		

ggplot(cars,aes(x="",y=peakrpm))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Peak RPM Distribution', 
       y='RPM')

# 1 outlier in the data.


# citympg	
ggplot(cars,aes(x="",y=citympg))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='City Mileage Distribution', 
       y='City MPG')
#2 Outliers in the data.

# highwaympg
ggplot(cars,aes(x="",y=highwaympg))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='HighWay Mileage Distribution', 
       y='Highway MPG')
# 3 Outliers in the data.


# power to weight ratio

ggplot(cars,aes(x="",y=power_to_weight))+
  geom_boxplot(fill='#FF5733',outlier.colour="blue")+
  labs(title='Power to Weight Distribution', 
       y='P\\W Ratio')
# We have huge outliers in the Power to weight Ratio.

# We observed few outliers in the Univariate analysis. lets fix the outliers

# Created a function to replace the outliers with data points at high and low whiskers
rm_outliers <- function(x){
  
  Intquart <- IQR(x)
  lowerquantile <- unname(quantile(x,0.25))
  upperrquantile <- unname(quantile(x,0.75))
  x <- replace(x,which(x > (upperrquantile + (Intquart * 1.5))),(upperrquantile + (Intquart * 1.5)))
  x <- replace(x,which(x < (lowerquantile - (Intquart * 1.5))),(lowerquantile - (Intquart * 1.5)))
  return(x)
  
}

cars$wheelbase <- rm_outliers(cars$wheelbase)
cars$carlength <- rm_outliers(cars$carlength)
cars$carwidth <- rm_outliers(cars$carwidth)
cars$enginesize <- rm_outliers(cars$enginesize)
cars$stroke <- rm_outliers(cars$stroke)
cars$compressionratio <- rm_outliers(cars$compressionratio)
cars$horsepower <- rm_outliers(cars$horsepower)
cars$peakrpm <- rm_outliers(cars$peakrpm)
cars$citympg <- rm_outliers(cars$citympg)
cars$highwaympg <- rm_outliers(cars$highwaympg)
cars$power_to_weight <- rm_outliers(cars$power_to_weight)


# Bi Variate Analysis

ggplot(cars,aes(x=factor(symboling),y=price,fill=factor(symboling)))+
  geom_boxplot()+
  labs(title='Pricing by Symboling rating ', 
       x='Symboling',
       y='Price',
       fill="Symboling")
  

# There are few cars with symboling rating less than 0. Also from the plot we can see that
# the mean value of price for the cars with symboling rating has higher average pricing.

# carCompany and price

ggplot(cars,aes(x=factor(Company),y=price,fill=factor(Company)))+
  geom_boxplot()+
  labs(title='Pricing by Company', 
       x='Company',
       y='Price',
       fill="Company")

# From the plot it is clear that there are brands with different classes and Jaguar with highest amongst them. This plot will help us 
# to segment the data into buckets based on their avg pricing.
# high Range > 20000, Medium Range between 10000 and 20000, Low Range < 10000
# This also helps generalize the data instead of using specific brand names

cars$range <- replace(cars$Company,which(cars$Company %in% c('bmw','buick','jaguar','porsche')),'HighRange')
cars$range <- replace(cars$range,which(cars$range %in% c('alfa-romero','audi','mercury','peugeot','saab','volvo')),'MidRange')
cars$range <- replace(cars$range,which(cars$range %in% c('chevrolet','dodge','honda','isuzu','mazda','mitsubishi','nissan','plymouth','renault','subaru','toyota','volkswagen')),'LowRange')
# Dropping Company variable after the segmentation
cars <- cars[,!(names(cars) %in% c('Company'))]

# fueltype

ggplot(cars,aes(x=factor(fueltype),y=price,fill=factor(fueltype)))+
  geom_boxplot()+
  labs(title='Pricing by Fuel Type', 
       x='Fuel Type',
       y='Price',
       fill="Fuel Type")
# average Price of Diesel cars are  higher than gas type cars.

# aspiration and price

ggplot(cars,aes(x=aspiration,y=price,fill=aspiration))+
  geom_boxplot()+
  labs(title='Pricing by Aspiration', 
       x='Aspiration',
       y='Price',
       fill="Aspiration")
# Turbo aspirated engines are priced high when compared to std engines

# door number and price

ggplot(cars,aes(x=factor(doornumber),y=price,fill=factor(doornumber)))+
  geom_boxplot()+
  labs(title='Pricing by Number of Doors', 
       x='Number of Doors',
       y='Price',
       fill="DoorNumber")
# The difference between average price of 2 door car and 4 door car is very less, meaning the number of doors 
# is not playing a factor in pricing of the car

# carbody and price

ggplot(cars,aes(x=carbody,y=price,fill=carbody))+
  geom_boxplot()+
  labs(title='Pricing by Car body', 
       x='Car body type',
       y='Price',
       fill="Car Body")

# Hardtop and convertible cars are priced high.

# drivewheel

ggplot(cars,aes(x=drivewheel,y=price,fill=drivewheel))+
  geom_boxplot()+
  labs(title='Pricing by Wheek Drive', 
       x='Wheel Drive',
       y='Price',
       fill="Wheel Drive")


# Data tells a different story is here. Average pricing of a RWD car is higher than 4WD car


# enginelocation and price
ggplot(cars,aes(x=enginelocation,y=price,fill=enginelocation))+
  geom_boxplot()+
  labs(title='Pricing by Engine Location', 
       x='Engine Location',
       y='Price',
       fill="Engine Location")


# Clearly cars with rear engines are much much costlier than the one with front engines

# wheelbase and Price

ggplot(cars,aes(x=wheelbase,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Wheel Base', 
       x='Wheel Base',
       y='Price',
       fill="Wheel Base")

cor(cars$wheelbase,cars$price) # - 0.5 indicates a positive correlation.

# There seems to be a positive corelation between the Wheel base and the price. As the wheel base increases the price increases the 

# carlength and the price

ggplot(cars,aes(x=carlength,y=price))+
  geom_point(alpha=0.4,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car Length', 
       x='Car Length',
       y='Price',
       fill="Car Length")

# As the length increase the price of the car increases

# carwidth and Price
ggplot(cars,aes(x=carwidth,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car Width', 
       x='Car Width',
       y='Price',
       fill="Car Width")

# As the width increase the price of the car increases

# carheight and Price
ggplot(cars,aes(x=carheight,y=price))+
  geom_point(alpha=0.3,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car Height', 
       x='Car Height',
       y='Price',
       fill="Car Height")

cor(cars$carheight,cars$price) # 0.1193362

# From the plot and the correlation number, there seems to be a less corelation of car height and pricing

# curbweight and price
ggplot(cars,aes(x=curbweight,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car Weight', 
       x='Car Weight',
       y='Price',
       fill="Car Weight")
# Car weight is strongly correlated to the pricing. As the weight increases the price of the car also increase

# enginetype
ggplot(cars,aes(x=enginetype,y=price,fill=enginetype))+
  geom_boxplot()+
  labs(title='Pricing by Engine Type', 
       x='Car Engine Type',
       y='Price',
       fill="Engine Type")
# The price varies according to different engine types as per the plot

# cylindernumber
ggplot(cars,aes(x=factor(cylindernumber),y=price,fill=factor(cylindernumber)))+
  geom_boxplot()+
  labs(title='Pricing by No. Of Cylinder', 
       x='No. Of Cylinder',
       y='Price',
       fill="No of Cylinder")

# excluding 2 the price of the car increases as the number of cylinder increases

# enginesize and price

ggplot(cars,aes(x=enginesize,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car engine size', 
       x='Engine Size',
       y='Price',
       fill="Car Height")

# from the plot it is clear that as the enigine size increases the price of the car also increases

# Fuel system and price

ggplot(cars,aes(x=fuelsystem,y=price,fill=fuelsystem))+
  geom_boxplot()+
  labs(title='Pricing by Fuel System', 
       x='Fuel System',
       y='Price',
       fill="Fuel System")

# The average pricing is varying for each fuelsystem type. This give us an idea that pricing is also dependent on the fuelsystem in the car


# boreratio and Price
ggplot(cars,aes(x=boreratio,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by bore ratio', 
       x='Bore Ratio',
       y='Price',
       fill="Bore Ratio")

# There is a positive correlation between the Price and the bore ratio

# Stroke and Car Price

ggplot(cars,aes(x=stroke,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Stroke', 
       x='Stroke',
       y='Price')
# From the scatter plot the stroke  doesnt seem to be deciding factor of pricing

# Compresson ratio
ggplot(cars,aes(x=compressionratio,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Compression Ratio', 
       x='Compression Ratio',
       y='Price')

# From the plot, there seems to be a very low negative correlation between compression ratio and price
cor(cars$compressionratio,cars$price) # -0.08088621

# horsepower and price
ggplot(cars,aes(x=horsepower,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Horse Power', 
       x='Horse Power',
       y='Price')

# As expected there is a strong correlation between Horsepower and the price

# peakrpm and price
ggplot(cars,aes(x=peakrpm,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Car Peak RPM', 
       x='Car Peak RPM',
       y='Price')

# Data tells a different story here. There is a low negative correlation between price and peak rpm

# City Mileage and Price

ggplot(cars,aes(x=citympg,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by City Mileage', 
       x='City Mileage',
       y='Price')

ggplot(cars,aes(x=highwaympg,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Highway Mileage', 
       x='Highway Mileage',
       y='Price')

# The price and Milaege are negatively correlated. Makes sense as the high end cars has a less mileage and vice versa

# power to size ratio and price

ggplot(cars,aes(x=power_to_weight,y=price))+
  geom_point(alpha=0.5,color='#E111BA')+
  geom_smooth(method = lm, se = FALSE)+
  labs(title='Pricing by Power Weight Ratio', 
       x='Power Weight',
       y='Price')

# The price and Power to size ratio are positively correlated and makes sense as it gives us measure of performance of car

#Lets do a Correlation plot amongst all variables. Please zoom the plot for a better View

select_if(cars, is.numeric) -> cars_numeric

ggcorrplot(round(cor(cars_numeric,use = 'pairwise.complete.obs'),2), hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of variables", 
           ggtheme=theme_bw)+
  theme(plot.title = element_text(hjust = 0.5))

# From the correlation plot that there are many variables which are strongly correlated with other variables



# Lets Now create Dummy variables for the categorical columns as regression modeling need Numerical data.


levels(cars$fueltype) <- c(1,0)

cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]

# aspiration column
levels(cars$aspiration) <- c(1,0)

cars$aspiration <- as.numeric(levels(cars$aspiration))[cars$aspiration]

# Car body

dummy <- data.frame(model.matrix(~carbody,data=cars))
dummy <- dummy[,-1]
cars <- cbind(cars[,!(names(cars) %in% c('carbody'))],dummy)

# drive wheel

dummy <- data.frame(model.matrix( ~drivewheel, data=cars))
dummy <- dummy[,-1]
cars <- cbind(cars[,!(names(cars) %in% c('drivewheel'))],dummy)

# enginelocation
levels(cars$enginelocation) <- c(1,0)
cars$enginelocation <- as.numeric(levels(cars$enginelocation))[cars$enginelocation]

# engine type

dummy <- data.frame(model.matrix( ~enginetype, data=cars))
dummy <- dummy[,-1]
cars <- cbind(cars[,!(names(cars) %in% c('enginetype'))],dummy)

# fuelsystem

dummy <- data.frame(model.matrix( ~fuelsystem, data=cars))
dummy <- dummy[,-1]
cars <- cbind(cars[,!(names(cars) %in% c('fuelsystem'))],dummy)

# Range

dummy <- data.frame(model.matrix(~range,data=cars))
dummy <- dummy[,-1]
cars <- cbind(cars[,!(names(cars) %in% c('range'))],dummy)

set.seed(10)
# Lets split the data into train and test

ind <- sample(1:nrow(cars), 0.7*nrow(cars))

traindata <- cars[ind,]
testdata <- cars[-ind,]

# Modeling

model_1 <- lm(price~.,data=traindata)

summary(model_1)
# Multiple R-squared:  0.958,	Adjusted R-squared:  0.9421 

# NA in the model summary denotes that the variable might not be linearly correlated with the dependent
# variable or can happen due to exact collinearity.

# Let us now use stepAIC function to reduce the dimension of varibles and give us significant variables.
step <- stepAIC(model_1, direction = "both")

# Lets now model with the variables that are marked '-' 
step

model_2 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
     wheelbase + curbweight + cylindernumber + compressionratio + 
     horsepower + peakrpm + citympg + power_to_weight + carbodyhardtop + 
     carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
     enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
     rangeLowRange + rangeMidRange, data = traindata)


summary(model_2)
#Multiple R-squared:  0.9561,	Adjusted R-squared:  0.9481

# Check for Multicollinearity
sort(vif(model_2),decreasing = T)

# HP, curbweight, carbodysedan,carbodyhatchback, citympg,wheelbase has VIF but are significant
# Power to weight has high VIF after above variables and also high P value.

# Let us model without the variable  power_to_weight

model_3 <- lm(formula = price ~ symboling + fueltype + enginelocation + 
                wheelbase + curbweight + cylindernumber + compressionratio + 
                horsepower + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)


summary(model_3)
# Multiple R-squared:  0.9547,	Adjusted R-squared:  0.9468

sort(vif(model_3), decreasing = T)
# The R Squared didnot vary after removing the variable and it also reduced VIF value of horsepower
# which is expected as we derived this variable.

# Variables with High VIF values are also significant Let us now experiment 
# removing the variables with high p values and observe its effect on R Squared
# Fuetype variable has hihgh P value (0.16)
# Lets model without variable fueltype

model_4 <- lm(formula = price ~ symboling + enginelocation + 
                wheelbase + curbweight + cylindernumber + compressionratio + 
                horsepower + peakrpm + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_4)
#Multiple R-squared:  0.954,	Adjusted R-squared:  0.9464

sort(vif(model_4),decreasing = T)


# peak RPM has high P value (0.2) among the list of variables
# Lets remove P and see how it effects our model.
model_5 <- lm(formula = price ~ symboling + enginelocation + 
                wheelbase + curbweight + cylindernumber + compressionratio + 
                horsepower + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_5)
#Multiple R-squared:  0.9535,	Adjusted R-squared:  0.9463 
# No change in RSquare and Adj R Squared

# Symboling has P value(0.09) Compared to others. Lets see how it effects our model without this variable

model_6 <- lm(formula = price ~ enginelocation + 
                wheelbase + curbweight + cylindernumber + compressionratio + 
                horsepower + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_6)
#Multiple R-squared:  0.9524,	Adjusted R-squared:  0.9455
# Model score is still good and no effect after removing the symboling variable


# Looking at the summary table of the model Compression ratio has high P value (0.1)
# Lets get rid of this.

model_7 <- lm(formula = price ~ enginelocation + 
                wheelbase + curbweight + cylindernumber + 
                horsepower + citympg + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_7)
#Multiple R-squared:  0.9516,	Adjusted R-squared:  0.945
# No Change in the R Squared and Ad R Sqaured score


# citympg has P value among the available variables (0.2). Lets model without this variable

model_8 <- lm(formula = price ~ enginelocation + 
                wheelbase + curbweight + cylindernumber + 
                horsepower + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + fuelsystemspdi + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_8)
#Multiple R-squared:  0.9511,	Adjusted R-squared:  0.9449 

# fuelsystempdi has high P values (0.172379)
model_9 <- lm(formula = price ~ enginelocation + 
                wheelbase + curbweight + cylindernumber + 
                horsepower + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_9)
#Multiple R-squared:  0.9503,	Adjusted R-squared:  0.9445

# engine type ohcf has P Value(0.053) compared to other variables

model_10 <- lm(formula = price ~ enginelocation + 
                wheelbase + curbweight + cylindernumber + 
                horsepower + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                enginetypeohcv + enginetyperotor + 
                rangeLowRange + rangeMidRange, data = traindata)
summary(model_10)
# Multiple R-squared:  0.9489,	Adjusted R-squared:  0.9433
#R Squared and Adj R Sqaured didnt vary much even after removing the variable

# enginetypeohcv has high Pvalue(0.088234) in the available variable list.
# Lets remove and see how model performs in terms of R Squared and Adj Squared.

model_11 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + enginetypeohc + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_11)
# Multiple R-squared:  0.9477,	Adjusted R-squared:  0.9424 

# Now the model has values with variables P values less than 0.05. To further finetune and
# generalize the model, lets continue removing the variables till the significance level is 0.001

# enginetypeohc has P value of 0.02 which is higher than other variables.
# Lets see how it effects the model R Sqaured and Adj R Squared after removing the variable

model_12 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_12)
# Multiple R-squared:  0.9455,	Adjusted R-squared:  0.9405 

#carbodyhardtop (0.018436 *) has highest P value among the other variables
model_13 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_13)
#Multiple R-squared:  0.9432,	Adjusted R-squared:  0.9384 

#carbodysedan has high P value(0.144006) compared to the rest of variables

model_14 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + 
                 carbodyhatchback + carbodywagon + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_14)
# Multiple R-squared:  0.9422,	Adjusted R-squared:  0.9378


#carbodywagon has highest P value(0.09972 .) compared to the rest of variables
model_15 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + 
                 carbodyhatchback + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_15)
#Multiple R-squared:  0.941,	Adjusted R-squared:  0.937

#carbodyhatchback has high P value (0.120510 )
model_16 <- lm(formula = price ~ enginelocation + 
                 wheelbase + curbweight + cylindernumber + 
                 horsepower + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_16)
# Multiple R-squared:  0.9399,	Adjusted R-squared:  0.9363 

#wheelbase is just above the 0.001 value. Lets see how eliminating this variable effects the score
model_17 <- lm(formula = price ~ enginelocation + 
                 curbweight + cylindernumber + 
                 horsepower + 
                 enginetyperotor + 
                 rangeLowRange + rangeMidRange, data = traindata)
summary(model_17)
#Multiple R-squared:  0.9356,	Adjusted R-squared:  0.9323 


# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.843e+03  2.147e+03   2.255 0.025712 *  
#   enginelocation  -5.447e+03  1.591e+03  -3.424 0.000817 ***
#   curbweight       5.323e+00  7.388e-01   7.205 3.71e-11 ***
#   cylindernumber   1.165e+03  2.657e+02   4.384 2.33e-05 ***
#   horsepower       4.297e+01  9.293e+00   4.624 8.71e-06 ***
#   enginetyperotor  5.292e+03  1.476e+03   3.585 0.000470 ***
#   rangeLowRange   -1.102e+04  8.963e+02 -12.297  < 2e-16 ***
#   rangeMidRange   -8.688e+03  8.420e+02 -10.319  < 2e-16 ***

# What does it mean in terms of business perspective.
# Increase in horspower,Cylinder and weight increases the price of the car.
# Front loaded engines have a negeative effect on Price, which we also observed during EDA. rare engines has a high cost.
# Also the low range vachiles has lesser pricing as observed in variables.

# Lets now use this model to predict the price values on test data

predictedvalue <- predict(model_17,testdata[,-20])

RSQuared <- cor(predictedvalue,testdata$price)^2
# RSquare of Train data = 0.93
# RSquared of Test data = 0.91

error = testdata$price - predictedvalue


ggplot(testdata,aes(x=horsepower,y=price))+
  geom_smooth(se = FALSE,method='loess')+
  geom_smooth(aes(x=horsepower,y=predictedvalue),color='red',se = FALSE,method='loess')+
  labs(title='Actual VS Predicted Price')


# Actual VS Predicted follow a similar pattern.





