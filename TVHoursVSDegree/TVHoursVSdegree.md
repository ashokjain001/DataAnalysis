### Does hours spent on TV varies with Education level?

### Introduction:
    In this study we will explore relationship between the education level and no of hours spent on TV. It is an important variable to study, since will help us in targetting the correct audience while producing a TV show and TV commercials.

### Data:


```r
  load("/Users/ashokjain/Desktop/R/statistics-project-gss.RData")
```
    The data used in this project is obtained from General social survey(GSS). GSS has been conducting surveys from 1972-2012 to monitor the American societal change and to capture the trend changes that happen over the years. Survey consists of specific set of questions that has been repeated over the years.
      The survey has been conducted by computer-assisted personal interview (CAPI), face-to-face interview, telephone interview and the respondents for this survey are randomly selected. Since we are merely observing the responses and monitor them over years, it is an observational study. Hence the result of this study is generalizable to US population. 
      Two main variables we will be studying is tvhours and degree. tvhours is a numerical variable and it captures the number of hours spent by the respondent, degree is a categorical variable and it has 5 levels.
      
    summary of the variables

```r
  attach(gss)
  summary(tvhours)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##       0       2       2       3       4      24   23206
```

```r
  summary(degree)
```

```
## Lt High School    High School Junior College       Bachelor       Graduate 
##          11822          29287           3070           8002           3870 
##           NA's 
##           1010
```
    Before we start with the data analysis we will create a data frame with only two variables tvhours and degree.


```r
    degtv<-data.frame(degree,tvhours)
    head(degtv)
```

```
##           degree tvhours
## 1       Bachelor      NA
## 2 Lt High School      NA
## 3    High School      NA
## 4       Bachelor      NA
## 5    High School      NA
## 6    High School      NA
```

```r
    summary(degtv)
```

```
##             degree         tvhours     
##  Lt High School:11822   Min.   : 0     
##  High School   :29287   1st Qu.: 2     
##  Junior College: 3070   Median : 2     
##  Bachelor      : 8002   Mean   : 3     
##  Graduate      : 3870   3rd Qu.: 4     
##  NA's          : 1010   Max.   :24     
##                         NA's   :23206
```
    from the summary, we notice that there are null values in the data frame.

```r
  gdata<-na.omit(degtv)
  head(gdata)
```

```
##              degree tvhours
## 4602    High School       1
## 4603    High School       2
## 4605    High School       2
## 4606       Graduate       1
## 4607       Graduate       3
## 4608 Lt High School       4
```
    Now that we have removed all the null values we will summarise the data once again.

```r
  summary(gdata)  
```

```
##             degree         tvhours     
##  Lt High School: 6807   Min.   : 0.00  
##  High School   :17534   1st Qu.: 2.00  
##  Junior College: 1881   Median : 2.00  
##  Bachelor      : 4771   Mean   : 2.96  
##  Graduate      : 2298   3rd Qu.: 4.00  
##                         Max.   :24.00
```
    visualizing the two variables.

```r
plot(gdata$degree, xlab = "Degree", ylab = "Counts", main = "Degree")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-61.png) 

```r
hist(gdata$tvhours,breaks = 24, col = "blue", main = "Tv hours Distribution")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-62.png) 

```r
summary(gdata)
```

```
##             degree         tvhours     
##  Lt High School: 6807   Min.   : 0.00  
##  High School   :17534   1st Qu.: 2.00  
##  Junior College: 1881   Median : 2.00  
##  Bachelor      : 4771   Mean   : 2.96  
##  Graduate      : 2298   3rd Qu.: 4.00  
##                         Max.   :24.00
```

    To compare different groups in degree variable we are using Box plot.

```r
boxplot(gdata$tvhours~gdata$degree, ylab='TV Hours',xlab='Degree')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
    Here we observe that LtHigh School has more variability, which means that respondents watch more hours of TV compared to any other group. It is right skewed i.e mean is greater than the median. 

    we can observe the same numerically using 'by' function in R.

```r
 by(gdata$tvhours,gdata$degree,summary)
```

```
## gdata$degree: Lt High School
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    2.00    3.00    3.77    5.00   24.00 
## -------------------------------------------------------- 
## gdata$degree: High School
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    2.00    3.00    3.04    4.00   24.00 
## -------------------------------------------------------- 
## gdata$degree: Junior College
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    1.00    2.00    2.54    3.00   20.00 
## -------------------------------------------------------- 
## gdata$degree: Bachelor
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    1.00    2.00    2.19    3.00   24.00 
## -------------------------------------------------------- 
## gdata$degree: Graduate
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     1.0     2.0     1.9     2.0    24.0
```
### Inference:
    Before we go further with our analysis, we set null and alternate hypothesis. 
    H_0 -  all groups watch same amount(hours) of TV
    H_A -  TV watching Hours differs between groups.
    
    since one variable is categorical with 5 levels and other group is numerical we will employ anova function to test our hypothesis.
    
    we will check the condition for anova, 
    1) sampled observations within group are independent.
    2) groups are independent of each other.
    3) distributions with in each group are nearly normal.
    4) each group has roughly equal variability as you can see in the box plot.
  
    since all the condition for anova has been met we will use aov function now.

```r
  fit<-aov(gdata$tvhours~gdata$degree,data = gdata)
  anova(fit) 
```

```
## Analysis of Variance Table
## 
## Response: gdata$tvhours
##                 Df Sum Sq Mean Sq F value Pr(>F)    
## gdata$degree     4  10295    2574     497 <2e-16 ***
## Residuals    33286 172537       5                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
    from the result we reject null hypothesis and conclude that all group means are not equal.
 
    now to check which group means are significantly different from each other we use Tukey test

```r
  TukeyHSD(fit)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = gdata$tvhours ~ gdata$degree, data = gdata)
## 
## $`gdata$degree`
##                                  diff     lwr     upr p adj
## High School-Lt High School    -0.7306 -0.8193 -0.6419     0
## Junior College-Lt High School -1.2279 -1.3897 -1.0661     0
## Bachelor-Lt High School       -1.5760 -1.6933 -1.4588     0
## Graduate-Lt High School       -1.8716 -2.0214 -1.7218     0
## Junior College-High School    -0.4973 -0.6480 -0.3466     0
## Bachelor-High School          -0.8454 -0.9468 -0.7440     0
## Graduate-High School          -1.1410 -1.2788 -1.0032     0
## Bachelor-Junior College       -0.3481 -0.5172 -0.1791     0
## Graduate-Junior College       -0.6437 -0.8368 -0.4506     0
## Graduate-Bachelor             -0.2956 -0.4533 -0.1379     0
```
    from the result we observe that all the group differ significantly from each other. Tukey test also gives the 95% confident interval difference between any two group.
    we will interpret one result from Tukey output.
    The High school group watches tv .81 to .64 hours less to the group Left High School. The p value is less than 5 % significance level and there is siginifant differnce between these two groups.

```r
  plot(TukeyHSD(fit))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
    we can observe mean and confidence interval for each group from the plot.
    
### Conclusion:
    With the conclusion of this research we can say that Tv watching hours differs significantly among the groups. The more a group is educated lesser they watch TV and vice versa. Since it is an observational studies and observations are randomly sampled, we can generalize this findings to the population of USA.

### References:
    Data Citation:                                                                                                       Smith, Tom W., Michael Hout, and Peter V. Marsden. General Social Survey, 1972-2012 [Cumulative File]. ICPSR34802-v1. Storrs, CT: Roper Center for Public Opinion Research, University of Connecticut /Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributors], 2013-09-11. doi:10.3886/ICPSR34802.v1 
    
    Links:

    Research Home page - http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34802/version/1
    
    Variable Description - https://d396qusza40orc.cloudfront.net/statistics%2Fproject%2Fgss1.html

    Data - http://bit.ly/dasi_gss_data
### Appendix:

```r
    gdata[1:50,]
```

```
##              degree tvhours
## 4602    High School       1
## 4603    High School       2
## 4605    High School       2
## 4606       Graduate       1
## 4607       Graduate       3
## 4608 Lt High School       4
## 4609 Lt High School       4
## 4610 Lt High School       2
## 4611       Graduate       1
## 4612       Bachelor       3
## 4613 Lt High School       3
## 4614 Lt High School       3
## 4615       Bachelor       3
## 4616       Bachelor       3
## 4617 Lt High School       5
## 4618    High School       2
## 4619 Lt High School       1
## 4620    High School       3
## 4621    High School       1
## 4622    High School       2
## 4623    High School       2
## 4624    High School       4
## 4625    High School       3
## 4626 Lt High School       2
## 4627 Lt High School       2
## 4628 Lt High School       2
## 4629       Graduate       1
## 4630    High School       2
## 4631       Graduate       3
## 4632    High School       4
## 4633       Graduate       3
## 4634    High School       2
## 4635 Lt High School       1
## 4636 Lt High School       3
## 4637 Lt High School       5
## 4638    High School       2
## 4639    High School       2
## 4640    High School       1
## 4641    High School       1
## 4642 Lt High School       1
## 4643 Lt High School       4
## 4644    High School       5
## 4645    High School       4
## 4646    High School       3
## 4647    High School       2
## 4648 Lt High School       1
## 4649 Lt High School       3
## 4650 Lt High School       4
## 4651 Lt High School       3
## 4652 Lt High School       6
```

  
