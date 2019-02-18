install.packages("party")
install.packages("woe")
library(party)
library(woe)

newdata=read.csv(file="HCAHPS_Data.CSV",header = TRUE)
attach(newdata)

set.seed(12)
str(newdata)

newdata$RCMND=as.factor(newdata$RCMND)
pd<-sample(2,nrow(newdata),replace=TRUE,prob=c(0.5,0.5))
train_dt_data<-newdata[pd==1,]
validate_dt_data<-newdata[pd==2,]
RCMND_test<-newdata$RCMND[pd==2]

#decision tree with party

myformula=RCMND~H_CLEAN_HSP_A_P	+H_CLEAN_HSP_SN_P +	H_CLEAN_HSP_U_P	+H_COMP_1_A_P	+H_COMP_1_SN_P	+H_COMP_1_U_P	+H_COMP_2_A_P	+H_COMP_2_SN_P	+H_COMP_2_U_P	+H_COMP_3_A_P	+H_COMP_3_SN_P+  H_COMP_3_U_P+ H_COMP_5_A_P+	H_COMP_5_SN_P	+H_COMP_5_U_P	+H_COMP_6_N_P	+H_COMP_6_Y_P	+H_COMP_7_A	+H_COMP_7_D_SD+H_COMP_7_SA+  H_QUIET_HSP_A_P	+H_QUIET_HSP_SN_P+	H_QUIET_HSP_U_P	+H_CLEAN_LINEAR_SCORE+H_COMP_1_LINEAR_SCORE+	H_COMP_2_LINEAR_SCORE+H_COMP_3_LINEAR_SCORE+H_COMP_5_LINEAR_SCORE+H_COMP_6_LINEAR_SCORE+H_COMP_7_LINEAR_SCORE+H_QUIET_LINEAR_SCORE	+H_CLEAN_STAR_RATING	+H_COMP_1_STAR_RATING+  H_COMP_2_STAR_RATING+ H_COMP_3_STAR_RATING+	H_COMP_5_STAR_RATING+	H_COMP_6_STAR_RATING+H_COMP_7_STAR_RATING+H_QUIET_STAR_RATING

#root node has hightst significance
tree<-ctree(myformula,data= train_dt_data)
plot(tree)

testpredict<-predict(tree,validate_dt_data)
tab<-table(testpredict,RCMND_test)
print(tab)


#accuracy
sum(diag(tab))/sum(tab)

#misclassification
1-sum(diag(tab))/sum(tab)
