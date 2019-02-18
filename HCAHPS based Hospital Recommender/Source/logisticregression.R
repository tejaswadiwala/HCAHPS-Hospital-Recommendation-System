install.packages("caTools")
install.packages("ROCR")
library(caTools)
library(ROCR)

newdata=read.csv(file="HCAHPS_Data.CSV",header = TRUE)
attach(newdata)

newdata$RCMND <-as.factor(newdata$RCMND)

set.seed(88)
split=sample.split(newdata$RCMND, SplitRatio = 0.50)

train_data=subset(newdata,split==TRUE)
test_data=subset(newdata,split==FALSE)

myformula=RCMND~H_CLEAN_HSP_A_P	+H_CLEAN_HSP_SN_P +	H_CLEAN_HSP_U_P	+H_COMP_1_A_P	+H_COMP_1_SN_P	+H_COMP_1_U_P	+H_COMP_2_A_P	+H_COMP_2_SN_P	+H_COMP_2_U_P	+H_COMP_3_A_P	+H_COMP_3_SN_P+  H_COMP_3_U_P+ H_COMP_5_A_P+	H_COMP_5_SN_P	+H_COMP_5_U_P	+H_COMP_6_N_P	+H_COMP_6_Y_P	+H_COMP_7_A	+H_COMP_7_D_SD+H_COMP_7_SA+  H_QUIET_HSP_A_P	+H_QUIET_HSP_SN_P+	H_QUIET_HSP_U_P	+H_CLEAN_LINEAR_SCORE+H_COMP_1_LINEAR_SCORE+	H_COMP_2_LINEAR_SCORE+H_COMP_3_LINEAR_SCORE+H_COMP_5_LINEAR_SCORE+H_COMP_6_LINEAR_SCORE+H_COMP_7_LINEAR_SCORE+H_QUIET_LINEAR_SCORE	+H_CLEAN_STAR_RATING	+H_COMP_1_STAR_RATING+  H_COMP_2_STAR_RATING+ H_COMP_3_STAR_RATING+	H_COMP_5_STAR_RATING+	H_COMP_6_STAR_RATING+H_COMP_7_STAR_RATING+H_QUIET_STAR_RATING

patient_model=glm(myformula,data=train_data, family=binomial)
summary(patient_model)


predict_test_model=predict(patient_model, type="response",newdata = test_data)
summary(predict_test_model)

#Roc Curve (focus mainly predicting the accuracy 1 ie hospital recomented )
ROCRtestpred <- prediction(predict_test_model,test_data$RCMND)
rocurve<-performance(ROCRtestpred,"tpr","fpr")
plot(rocurve,colorize=T, main="ROC Curve" ,ylab="Sensitivity / TRUE Positive",xlab="1-Specificity / False Positive")
# shows that if we say all the hospital not recomende then we will be correct aprox 53 percent
#curve is abov the line so curve is better than the benchmark
abline(a=0,b=1)
#AOC MORE AREA THE BETTER (the bench mark line has 50% area under it but our model excels)

auc<-performance(ROCRtestpred,'auc')
auc<-unlist(slot(auc,"y.values"))
auc


confusion_test<-table(test_data$RCMND, predict_test_model > 0.5)
confusion_test
#accuracy
(confusion_test[1,1]+confusion_test[2,2])/sum(confusion_test)
#inaccuracy
(confusion_test[1,2]+confusion_test[2,1])/sum(confusion_test)


