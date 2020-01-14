> library(plyr)
> library(corrplot)
> library(ggplot2)
> library(gridExtra)
> library(ggthemes) 
> library(caret) 
> library(MASS) 
> library(randomForest)
> library(party)
> library(caTools)
> 
  > rm(list = ls())
> ls()
character(0)
> gc()
used  (Mb) gc trigger  (Mb) max used  (Mb)
Ncells 2298161 122.8    3941810 210.6  3941810 210.6
Vcells 3760245  28.7    8388608  64.0  8388520  64.0
> 
  > cchurn <- read.csv("C:/Users/jayan/OneDrive/Desktop/Extra/Jiju Laptop/Hard Disk/Laptop Files/Dell Backup/Premier/Projects/Customer Churn/Final Churn Data_FY19_Offline.csv")
> 
  > str(cchurn)
'data.frame':	60235 obs. of  13 variables:
  $ EU.Account.ID                     : num  1.00e+09 1.02e+09 1.02e+09 1.02e+09 1.02e+09 ...
$ EU.Account.Name                   : Factor w/ 59942 levels "@ LEGAL DISCOVERY LLC",..: 55928 25548 48445 39351 48398 28488 39385 15631 9387 18978 ...
$ Total.Offline.Rev.3.years         : Factor w/ 59985 levels "$1,000,112.20",..: 29225 16279 22223 58276 47015 35902 7853 14070 7444 44521 ...
$ Total.Offline.Margin.3.years      : Factor w/ 59760 levels "$0.00 ","$0.83 ",..: 58910 53134 21494 52915 6568 6694 40258 28784 36673 17626 ...
$ Total.Offline.Transactions.3.years: int  184 40 1 13 29 6 23 22 13 2 ...
$ Bid.RR                            : Factor w/ 2 levels "Bid","RR": 1 2 2 1 2 2 2 2 2 2 ...
$ Product.Type                      : Factor w/ 3 levels "Client Solutions PBU",..: 1 2 3 3 1 1 2 1 1 3 ...
$ Seasonality                       : Factor w/ 2 levels "No","Yes": 2 2 1 2 1 2 1 1 1 2 ...
$ Nature.of.Order                   : Factor w/ 3 levels "Big","Medium",..: 1 1 1 1 1 1 1 3 2 1 ...
$ Spike.before.churn                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 1 1 1 1 ...
$ Recency.of.Purchase               : Factor w/ 4 levels "High","Low","Medium",..: 4 4 2 1 4 1 1 3 3 1 ...
$ Frequency.of.Purchase             : Factor w/ 3 levels "High","Low","Medium": 1 1 2 3 3 2 3 2 3 2 ...
$ Churn                             : int  0 0 1 0 0 1 0 1 1 1 ...
> 
  > sapply(cchurn, function(x) sum(is.na(x)))
EU.Account.ID                    EU.Account.Name 
0                                  0 
Total.Offline.Rev.3.years       Total.Offline.Margin.3.years 
0                                  0 
Total.Offline.Transactions.3.years                             Bid.RR 
0                                  0 
Product.Type                        Seasonality 
0                                  0 
Nature.of.Order                 Spike.before.churn 
0                                  0 
Recency.of.Purchase              Frequency.of.Purchase 
0                                  0 
Churn 
0 
> cchurn$EU.Account.ID = as.factor(cchurn$EU.Account.ID)
> cchurn$EU.Account.Name = as.factor(cchurn$EU.Account.Name)
> cchurn[,3:5] <- lapply(cchurn[,3:5], as.numeric)
> cchurn$Nature.of.Order <- as.ordered(cchurn$Nature.of.Order)
> cchurn$Recency.of.Purchase <- as.ordered(cchurn$Recency.of.Purchase)
> cchurn$Frequency.of.Purchase <- as.ordered(cchurn$Frequency.of.Purchase)
> cchurn$Churn <- as.factor(cchurn$Churn)
> str(cchurn)
'data.frame':	60235 obs. of  13 variables:
  $ EU.Account.ID                     : Factor w/ 60235 levels "595660671","595660674",..: 15802 15803 15804 15805 15806 15807 15808 15809 15810 15811 ...
$ EU.Account.Name                   : Factor w/ 59942 levels "@ LEGAL DISCOVERY LLC",..: 55928 25548 48445 39351 48398 28488 39385 15631 9387 18978 ...
$ Total.Offline.Rev.3.years         : num  29225 16279 22223 58276 47015 ...
$ Total.Offline.Margin.3.years      : num  58910 53134 21494 52915 6568 ...
$ Total.Offline.Transactions.3.years: num  184 40 1 13 29 6 23 22 13 2 ...
$ Bid.RR                            : Factor w/ 2 levels "Bid","RR": 1 2 2 1 2 2 2 2 2 2 ...
$ Product.Type                      : Factor w/ 3 levels "Client Solutions PBU",..: 1 2 3 3 1 1 2 1 1 3 ...
$ Seasonality                       : Factor w/ 2 levels "No","Yes": 2 2 1 2 1 2 1 1 1 2 ...
$ Nature.of.Order                   : Ord.factor w/ 3 levels "Big"<"Medium"<..: 1 1 1 1 1 1 1 3 2 1 ...
$ Spike.before.churn                : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 1 1 1 1 ...
$ Recency.of.Purchase               : Ord.factor w/ 4 levels "High"<"Low"<"Medium"<..: 4 4 2 1 4 1 1 3 3 1 ...
$ Frequency.of.Purchase             : Ord.factor w/ 3 levels "High"<"Low"<"Medium": 1 1 2 3 3 2 3 2 3 2 ...
$ Churn                             : Factor w/ 2 levels "0","1": 1 1 2 1 1 2 1 2 2 2 ...
> 
  > p1 <- ggplot(cchurn, aes(x=Bid.RR)) + ggtitle("Bid.RR") + xlab("Bid.RR") +
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
> 
  > p2 <- ggplot(cchurn, aes(x=Product.Type)) + ggtitle("Product.Type") + xlab("Product.Type") + 
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
> 
  > p3 <- ggplot(cchurn, aes(x=Seasonality)) + ggtitle("Seasonality") + xlab("Seasonality") +
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
> p4 <- ggplot(cchurn, aes(x=Frequency.of.Purchase)) + ggtitle("Frequency.of.Purchase") + xlab("Frequency.of.Purchase") +
  +     geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
> grid.arrange(p1, p2, p3, p4, ncol=3)
> 
  > numeric.var <- sapply(cchurn, is.numeric)
> corr.matrix <- cor(cchurn[,numeric.var])
> corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")
> 
  > intrain<- createDataPartition(cchurn$Churn,p=0.7,list=FALSE)
> set.seed(2017)
> 
  > training<- cchurn[intrain,]
> testing<- cchurn[-intrain,]
> dim(training); dim(testing)
[1] 42166    13
[1] 18069    13
> 
  > 
  > LogModel <- glm(Churn ~ Total.Offline.Margin.3.years+Total.Offline.Transactions.3.years+Product.Type+Seasonality+Frequency.of.Purchase+Recency.of.Purchase+Nature.of.Order+Spike.before.churn+Bid.RR
                    +                 ,family=binomial(link="logit"),data=training)
Warning message:
  glm.fit: fitted probabilities numerically 0 or 1 occurred 
> print(summary(LogModel))

Call:
  glm(formula = Churn ~ Total.Offline.Margin.3.years + Total.Offline.Transactions.3.years + 
        Product.Type + Seasonality + Frequency.of.Purchase + Recency.of.Purchase + 
        Nature.of.Order + Spike.before.churn + Bid.RR, family = binomial(link = "logit"), 
      data = training)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.0736  -0.5004  -0.1891   0.6686   3.9841  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                                1.513e-01  5.453e-02   2.776  0.00551 ** 
  Total.Offline.Margin.3.years              -1.313e-06  8.119e-07  -1.617  0.10578    
Total.Offline.Transactions.3.years        -3.011e-03  3.336e-04  -9.026  < 2e-16 ***
  Product.TypeEnterprise Solution Group PBU  3.627e-02  3.699e-02   0.981  0.32680    
Product.TypeOthers                         2.712e-01  4.176e-02   6.493 8.43e-11 ***
  SeasonalityYes                            -2.072e-01  3.067e-02  -6.755 1.43e-11 ***
  Frequency.of.Purchase.L                    8.319e-01  3.301e-02  25.204  < 2e-16 ***
  Frequency.of.Purchase.Q                   -9.533e-01  3.204e-02 -29.753  < 2e-16 ***
  Recency.of.Purchase.L                     -8.922e-01  2.867e-02 -31.120  < 2e-16 ***
  Recency.of.Purchase.Q                     -1.139e+00  4.298e-02 -26.496  < 2e-16 ***
  Recency.of.Purchase.C                     -2.534e-01  4.703e-02  -5.389 7.10e-08 ***
  Nature.of.Order.L                          3.105e-01  3.637e-02   8.538  < 2e-16 ***
  Nature.of.Order.Q                          3.908e-03  3.049e-02   0.128  0.89802    
Spike.before.churnYes                     -1.765e+01  6.989e+01  -0.253  0.80065    
Bid.RRRR                                   1.125e-01  3.834e-02   2.935  0.00334 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 51868  on 42165  degrees of freedom
Residual deviance: 31825  on 42151  degrees of freedom
AIC: 31855

Number of Fisher Scoring iterations: 17

> 
  > anova(LogModel, test="Chisq")
Analysis of Deviance Table

Model: binomial, link: logit

Response: Churn

Terms added sequentially (first to last)


Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                                               42165      51868              
Total.Offline.Margin.3.years        1     20.2     42164      51848 6.981e-06 ***
  Total.Offline.Transactions.3.years  1   5265.0     42163      46583 < 2.2e-16 ***
  Product.Type                        2    288.4     42161      46295 < 2.2e-16 ***
  Seasonality                         1   1066.7     42160      45228 < 2.2e-16 ***
  Frequency.of.Purchase               2   2999.7     42158      42228 < 2.2e-16 ***
  Recency.of.Purchase                 3   6390.1     42155      35838 < 2.2e-16 ***
  Nature.of.Order                     2    156.6     42153      35682 < 2.2e-16 ***
  Spike.before.churn                  1   3848.1     42152      31834 < 2.2e-16 ***
  Bid.RR                              1      8.6     42151      31825  0.003302 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Warning messages:
  1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
4: glm.fit: fitted probabilities numerically 0 or 1 occurred 
5: glm.fit: fitted probabilities numerically 0 or 1 occurred 
6: glm.fit: fitted probabilities numerically 0 or 1 occurred 
7: glm.fit: fitted probabilities numerically 0 or 1 occurred 
> 
  > step(LogModel)
Start:  AIC=31854.87
Churn ~ Total.Offline.Margin.3.years + Total.Offline.Transactions.3.years + 
  Product.Type + Seasonality + Frequency.of.Purchase + Recency.of.Purchase + 
  Nature.of.Order + Spike.before.churn + Bid.RR

Df Deviance   AIC
<none>                                     31825 31855
- Total.Offline.Margin.3.years        1    31827 31855
- Bid.RR                              1    31834 31862
- Product.Type                        2    31868 31894
- Seasonality                         1    31870 31898
- Nature.of.Order                     2    31913 31939
- Total.Offline.Transactions.3.years  1    31978 32006
- Frequency.of.Purchase               2    32906 32932
- Recency.of.Purchase                 3    33991 34015
- Spike.before.churn                  1    35591 35619

Call:  glm(formula = Churn ~ Total.Offline.Margin.3.years + Total.Offline.Transactions.3.years + 
             Product.Type + Seasonality + Frequency.of.Purchase + Recency.of.Purchase + 
             Nature.of.Order + Spike.before.churn + Bid.RR, family = binomial(link = "logit"), 
           data = training)

Coefficients:
  (Intercept)               Total.Offline.Margin.3.years  
1.513e-01                                 -1.313e-06  
Total.Offline.Transactions.3.years  Product.TypeEnterprise Solution Group PBU  
-3.011e-03                                  3.627e-02  
Product.TypeOthers                             SeasonalityYes  
2.712e-01                                 -2.072e-01  
Frequency.of.Purchase.L                    Frequency.of.Purchase.Q  
8.319e-01                                 -9.533e-01  
Recency.of.Purchase.L                      Recency.of.Purchase.Q  
-8.922e-01                                 -1.139e+00  
Recency.of.Purchase.C                          Nature.of.Order.L  
-2.534e-01                                  3.105e-01  
Nature.of.Order.Q                      Spike.before.churnYes  
3.908e-03                                 -1.765e+01  
Bid.RRRR  
1.125e-01  

Degrees of Freedom: 42165 Total (i.e. Null);  42151 Residual
Null Deviance:	    51870 
Residual Deviance: 31820 	AIC: 31850
Warning messages:
  1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
4: glm.fit: fitted probabilities numerically 0 or 1 occurred 
5: glm.fit: fitted probabilities numerically 0 or 1 occurred 
6: glm.fit: fitted probabilities numerically 0 or 1 occurred 
7: glm.fit: fitted probabilities numerically 0 or 1 occurred 
8: glm.fit: fitted probabilities numerically 0 or 1 occurred 
> 
  > fitted.results <- predict(LogModel,newdata=testing,type='response')
> res <- predict(LogModel,newdata=training,type='response')
> 
  > library(ROCR)
> ROCRPred = prediction(res,training$Churn)
> ROCRPref = performance(ROCRPred,"tpr","fpr")
> plot(ROCRPref, colorize = TRUE, print.cutoffs.at=seq(0.1,by=0.1))
> 
  > testing$fitted.results_1 <- ifelse(fitted.results > 0.45,1,0)
> confusionMatrix(as.factor(testing$Churn),as.factor(testing$fitted.results_1))
Confusion Matrix and Statistics

Reference
Prediction     0     1
0 10957  1601
1  1487  4024

Accuracy : 0.8291          
95% CI : (0.8235, 0.8346)
No Information Rate : 0.6887          
P-Value [Acc > NIR] : <2e-16          

Kappa : 0.5992          

Mcnemar's Test P-Value : 0.042           
                                          
            Sensitivity : 0.8805          
            Specificity : 0.7154          
         Pos Pred Value : 0.8725          
         Neg Pred Value : 0.7302          
             Prevalence : 0.6887          
         Detection Rate : 0.6064          
   Detection Prevalence : 0.6950          
      Balanced Accuracy : 0.7979          
                                          
       'Positive' Class : 0               
                                          
> exp(cbind(OR=coef(LogModel), confint(LogModel)))
Waiting for profiling to be done...
                                                    OR        2.5 %       97.5 %
(Intercept)                               1.163401e+00 1.045548e+00 1.294718e+00
Total.Offline.Margin.3.years              9.999987e-01 9.999971e-01 1.000000e+00
Total.Offline.Transactions.3.years        9.969934e-01 9.963197e-01 9.976227e-01
Product.TypeEnterprise Solution Group PBU 1.036940e+00 9.643765e-01 1.114873e+00
Product.TypeOthers                        1.311490e+00 1.208461e+00 1.423431e+00
SeasonalityYes                            8.128713e-01 7.654902e-01 8.632970e-01
Frequency.of.Purchase.L                   2.297683e+00 2.154142e+00 2.451719e+00
Frequency.of.Purchase.Q                   3.854844e-01 3.620290e-01 4.104781e-01
Recency.of.Purchase.L                     4.097540e-01 3.873301e-01 4.334030e-01
Recency.of.Purchase.Q                     3.202119e-01 2.942401e-01 3.482398e-01
Recency.of.Purchase.C                     7.761200e-01 7.081328e-01 8.515404e-01
Nature.of.Order.L                         1.364092e+00 1.270209e+00 1.464835e+00
Nature.of.Order.Q                         1.003916e+00 9.456646e-01 1.065735e+00
Spike.before.churnYes                     2.167822e-08 4.933772e-41 6.913370e-52
Bid.RRRR                                  1.119091e+00 1.038141e+00 1.206494e+00
There were 50 or more warnings (use warnings() to see the first 50)
> 
> tree <- ctree(Churn ~ Total.Offline.Margin.3.years+Total.Offline.Transactions.3.years+Product.Type+Seasonality+Frequency.of.Purchase+Recency.of.Purchase+Nature.of.Order+Spike.before.churn+Bid.RR, training)
> plot(tree, type='simple')
> 
> pred_tree <- predict(tree, testing)
> print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)
[1] "Confusion Matrix for Decision Tree"
         Actual
Predicted     0     1
        0 11191  1738
        1  1367  3773
> 
> p1 <- predict(tree, training)
> tab1 <- table(Predicted = p1, Actual = training$Churn)
> tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
> print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
[1] "Decision Tree Accuracy 0.82815872488793"
> 
> gc()
           used  (Mb) gc trigger  (Mb) max used  (Mb)
Ncells  4446788 237.5   12614960 673.8 11842460 632.5
Vcells 15904492 121.4   26335157 201.0 26335157 201.0
> 
> rfModel_new <- randomForest(Churn ~ Total.Offline.Margin.3.years+Total.Offline.Transactions.3.years+Product.Type+Seasonality+Frequency.of.Purchase+Recency.of.Purchase+Nature.of.Order+Spike.before.churn+Bid.RR, data = training, ntree = 200, mtry = 3)
> print(rfModel_new)

Call:
 randomForest(formula = Churn ~ Total.Offline.Margin.3.years +      Total.Offline.Transactions.3.years + Product.Type + Seasonality +      Frequency.of.Purchase + Recency.of.Purchase + Nature.of.Order +      Spike.before.churn + Bid.RR, data = training, ntree = 200,      mtry = 3) 
               Type of random forest: classification
                     Number of trees: 200
No. of variables tried at each split: 3

        OOB estimate of  error rate: 17.36%
Confusion matrix:
      0    1 class.error
0 25823 3482   0.1188193
1  3839 9022   0.2984993
> 
> pred_rf_new <- predict(rfModel_new, testing)
> caret::confusionMatrix(pred_rf_new, testing$Churn)
Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 11111  1636
         1  1447  3875
                                          
               Accuracy : 0.8294          
                 95% CI : (0.8238, 0.8348)
    No Information Rate : 0.695           
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.5936          
                                          
 Mcnemar's Test P-Value : 0.0007095       

Sensitivity : 0.8848          
Specificity : 0.7031          
Pos Pred Value : 0.8717          
Neg Pred Value : 0.7281          
Prevalence : 0.6950          
Detection Rate : 0.6149          
Detection Prevalence : 0.7055          
Balanced Accuracy : 0.7940          

'Positive' Class : 0               

> 
  > varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')