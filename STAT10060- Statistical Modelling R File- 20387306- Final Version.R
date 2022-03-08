
## Exploratory Analysis ##

1.
table(data$Group)

2.
hist(data$Age, xlab="Age", main="Distribution of Age Groups")    # Forms Histogram for the distribution of age groups #
min(data$Age)                                                    # Lowest Age #
max(data$Age)                                                    # Highest Age #
obswithin1sd_age<-sum(data$Age>mean(data$Age)-sd(data$Age) &data$Age<mean(data$Age)+sd(data$Age)) # Calculating observations within on SD of the mean #
(obswithin1sd_age/355)*100                                                                        # Calculating proportion of observations within one SD of the mean #   
abline(v=list(mean(data$Age)-sd(data$Age),mean(data$Age)+sd(data$Age)),lwd=2,col="red")           # Adding line one SD above and below the mean to histogram #

3.
bmi<-(data$Weight)/((data$Height)/100)^2                               # Divided by 100 to convert from cm to m #
hist(bmi, main="Distribution of BMI",xlab="BMI")                       # Forms Histogram for the distribution of BMI #
aggregate(bmi~Sex,data=data,FUN="mean")                                # Forms table of genders and their mean BMI #
mean(bmi)                                                              
sd(bmi)                                                                
obswithin1sd_bmi<- sum(bmi>mean(bmi)-sd(bmi) & bmi<mean(bmi)+sd(bmi))  # Calculating observations within on SD of the mean #
(obswithin1sd_bmi/355)*100                                             # Calculating proportion of observations within one SD of the mean #
abline(v=24.27, lwd=2, col="blue")                                     # Adding line for male mean BMI to histogram #
abline(v=27.92,lwd=2,col="purple")                                     # Adding line for female mean BMI to histogram # 

4.
county_weight_mean<-aggregate(Weight~County,data=data,FUN="mean")     # Forms table of counties and their mean weight #
highest_weight<-max(county_weight_mean$Weight)                        # Picks out the highest mean weight #
lowest_weight<-min(county_weight_mean$Weight)                         # Picks out the lowest mean weight #
county_highest<-county_weight_mean$County[county_weight_mean$Weight==highest_weight] # Picks out the county with mean weight equal to the highest weight #
county_lowest<-county_weight_mean$County[county_weight_mean$Weight==lowest_weight]   # Picks out the county with mean weight equal to the lowest weight #
county_highest
county_lowest

5.
tab_sex_bg<-table(data$Sex,data$BloodGroup)           # Table of Sex vs Blood group #
ptab_sex_bg<-(prop.table(tab_sex_bg))*100             # Proportion (percentage) Table of previous table # 
ptab_sex_bg                                           
ptab_sex_bg[2,3]                                      # Picking out % of Men in Blood Group B #

6.
choldiff<-data$Cholesterol2-data$Cholesterol1         # Creating new variable of difference of cholesterol levels # 
mean(choldiff)                                        # Getting the mean difference in cholesterol levels #



## Hypothesis Testing ##
                              
1.
p<-prop.table(table(data$CardioRisk))                  # Forms a proportion table of number of observations for each level of risk for the total sample #
cardiorisk_male<-data$CardioRisk[data$Sex=="Male"]     # Forms factor of cardiovascular disease risk levels for males #
cardiorisk_female<-data$CardioRisk[data$Sex=="Female"] # Forms factor of cardiovascular disease risk levels for females #
chisq.test(cardiorisk_male[1:167],                     # I restricted the size of the first factor from 168 to 167 so both factors were the same length #
           cardiorisk_female,p=p)                      # Performs Chi-Squared test #
qchisq(0.05,df=4,lower.tail=FALSE)                     # For Critical Value #

2.
weight_treatment<-data$Weight[data$Group=="Treatment"]                                                # Weights of people in Treatment Group #
weight_placebo<-data$Weight[data$Group=="Placebo"]                                                    # Weights of people in Placebo Group #
t.test(weight_treatment,weight_placebo,alternative="two.sided",mu=0,var.equal=TRUE, level=0.95)       # Two-sided t-test for difference of means with mu = 0 and equal variance assumed #
qt(0.05/2, df=333,lower.tail = FALSE)                                                                 # For Critical Value #              

3.
choldiff_drug<-data$Cholesterol2[data$Group=="Treatment"]-data$Cholesterol1[data$Group=="Treatment"]   # Difference between cholesterol before and after the experiment for the Treatment group #
choldiff_placebo<-data$Cholesterol2[data$Group=="Placebo"]-data$Cholesterol1[data$Group=="Placebo"]    # Difference between cholesterol before and after the experiment for the Placebo group #
t.test(choldiff_drug,choldiff_placebo,alternative="less",mu=0,var.equal = TRUE)                        # Lower-tailed t-test for difference of means with mu = 0 and equal variance assumed #
qt(0.05, df=333,lower.tail = TRUE)                                                                     # For Critical Value #           


## Model Fitting ##

1.
cor(data$Cholesterol1,bmi)           # Correlation Coefficient #

2.
reg<-lm(Cholesterol1~bmi,data=data)  # Forms Regression line using least squares method #
coefficients(reg)
plot(x=bmi,y=data$Cholesterol1,xlab="BMI",ylab = "Cholesterol Level",main= "Scatter Plot of BMI and Beginning Cholesterol Level for Patients")
abline(reg,lwd=2,col="blue")         # Adding regression line to scatter plot #

3.
summary(reg)                         # Summary about Regression Line #

4.
summary(reg)
qf(0.05/2,df1=1,df2 =333,lower.tail=FALSE)        # For Critical Value #


## Secondary Analysis ##

1.
aggregate(Height~BloodGroup,data=data,FUN="mean") # Forms table of mean height for each blood group #

2.
ANOVA<-aov(Height~BloodGroup,data=data)           # Analysis of the variance for height and blood group #
summary(ANOVA)                                    # Summary of ANOVA #
qf(0.05/2, df1=3, df2=331, lower.tail=FALSE)      # For Critical Value #

3.
summary(ANOVA)

