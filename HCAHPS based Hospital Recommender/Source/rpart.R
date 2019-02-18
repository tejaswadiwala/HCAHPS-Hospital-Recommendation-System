install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
newdata=read.csv(file="HCAHPS_Data.CSV",header = TRUE)
attach(newdata)

set.seed(12)
newdata$RCMND=as.factor(newdata$RCMND)

pd<-sample(2,nrow(newdata),replace=TRUE,prob=c(0.5,0.5))

train_dt_data<-newdata[pd==1,]
validate_dt_data<-newdata[pd==2,]
RCMND_test<-newdata$RCMND[pd==2]

myformula= RCMND~ H_COMP_1_SN_P +  H_COMP_7_D_SD +  H_COMP_3_A_P +  H_COMP_5_LINEAR_SCORE+  H_COMP_7_LINEAR_SCORE

tree_rpart<-rpart(myformula,data= train_dt_data)

rpart.plot(tree_rpart)

predict_rpart<-predict(tree_rpart,validate_dt_data,type="class")

#accuracy
mean(predict_rpart==RCMND_test)

#misclassification or inaccuracy
mean(predict_rpart!=RCMND_test)



