rm(list=ls())
library(tidyverse)
library(randomForest)
library(psych)

setwd('E:/ALLFED/WheatYield_LOS/')
##1. read in the cleaned file, and select the variables under consideration
df = read.csv('data/dwheat_fit_elim.csv')
df_rf = df %>% dplyr::select(Y, n_total, p_fertilizer, irrigation_tot,
                               mechanized, pesticides_H, thz_class,
                               mst_class, soil_class)
##check the data type of each column, if factor or character, 
##need to be changed to numeric
str(df_rf) 

##2. check  correlations between features
corPlot(df_rf[,-1], cex = 1.2) ##n_total is highly correlated with p_ferilizer

###remove p_fertilizer and NAs 
train_rf = df_rf %>% dplyr::select(-p_fertilizer) %>% filter(complete.cases(.))
dim(train_rf)
##3. random forest on whole training set 'df_rf'
set.seed(1234)

###3.1 run RF with ntree=500, ntree=1000 didnt work as run out of memory
rf_mod = randomForest(Y~.,data=train_rf,importance=TRUE, ntree=500)
#saveRDS(rf_mod,file='output/RandomForestResults.rds')
print(rf_mod)

###3.2 Find the optimal mtry value
mtry <- tuneRF(train_rf[-1],train_rf$Y, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
print(mtry)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)

###3.3 build model with again using best mtry value
best.m=3 ###from previous step
rf_mod2 = randomForest(Y~.,data=train_rf,importance=TRUE, ntree=500, mtry= best.m)
saveRDS(rf_mod2,file='output/RandomForestResults.rds')
print(rf_mod2)

importance(rf_mod2)
varImpPlot(rf_mod2)

##3.4 check the model performance for the training dataset---model calibration
pred_train= predict(rf_mod2,train_rf)
train_both = cbind(data.frame(obs_train=train_rf$Y),data.frame(pred_train=pred_train))
train_both = as.data.frame(train_both)

###3.5 get correlation (R2) between the prediction and the observations
reg_train =lm(pred_train~obs_train,data=train_both)
summary(reg_train)
lm_train =paste0(expression(R^2),'==', round(summary(reg_train)$r.squared,2))
lm_train = data.frame(lm_train)
lm_train$lm_train = as.character(lm_train$lm_train)

ggplot(train_both,aes(obs_train,pred_train))+geom_point()+
geom_abline()+theme_bw()+xlab('Wheat Production_train (Kg/ha)')+
  ylab('Wheat Prediciton (Kg/ha)')+
  theme(axis.text=element_text(size=12,face = 'bold'),
        text = element_text(size=15,face = 'bold'))+
  geom_text(data = lm_train, aes(x = 3000, y = 9000, label = lm_train, family = "serif"), 
            size=6, parse = TRUE) 

ggsave('output/Figure1a-ModelPerformance_train.png')

###3.6 check the model performance with the evaluation dataset---model evaluation
test_y = read.csv('data/dwheat_val_elim.csv')
test_y = test_y %>% select(Y)
test_x = read.csv('data/w_val_elim.csv')
test_rf = test_x %>% select(n_total, irrigation_tot,mechanized,
                          pesticides_H, thz_class, mst_class, soil_class)

pred_test= predict(rf_mod2,test_rf)
test_both= cbind(data.frame(obs_test=test_y$Y),data.frame(pred_test=pred_test))
test_both = as.data.frame(test_both)

###3.7 visualization of model evaluation
reg_test =lm(pred_test~obs_test,data=test_both)
summary(reg_test)
lm_test =paste0(expression(R^2),'==', round(summary(reg_test)$r.squared,2))
lm_test = data.frame(lm_test)
lm_test$lm_test= as.character(lm_test$lm_test)

ggplot(test_both,aes(obs_test,pred_test))+geom_point()+
  geom_abline()+theme_bw()+xlab('Wheat Production_test (Kg/ha)')+
  ylab('Wheat Prediciton (Kg/ha)')+
  theme(axis.text=element_text(size=12,face = 'bold'),
        text = element_text(size=15,face = 'bold'))+
  geom_text(data = lm_test, aes(x = 3000, y = 9000, label = lm_test, family = "serif"), 
            size=6, parse = TRUE) 

ggsave('output/Figure1b-ModelPerformance_test.png')



##check the variable importance 
varImpPlot(rf_mod2,main="Variable Importance")

###type 1 --- using %IncMSE
var.imp = data.frame(importance(rf_mod2,type=1))
var.imp$Variables = row.names(var.imp)
VIPVar = var.imp[order(var.imp$X.IncMSE,decreasing = T),]

ggplot(VIPVar, aes(x=reorder(Variables, X.IncMSE), weight=X.IncMSE)) +geom_bar() +coord_flip()+
  xlab("") + ylab("Variable Importance (%IncMSE)")+ theme_bw()+
  theme(axis.text.y=element_text(size=12,face = 'bold'),
        axis.title=element_text(size=16,face = 'bold'),
        text = element_text(size=15,face = 'bold'))

ggsave('output/Figure2-VariableImportance_IncMSE.png')

##########predicting if loss of industry (los) with training dataset

print(names(train_rf))

pred_los= vector('list', 0)
for (i in 2:length(names(train_rf))){
  ###change each column into zero but name them the same 
  train_pre = train_rf %>% select(-all_of(i)) %>% mutate('los'=0)
  colnames(train_pre)[ncol(train_pre)] = names(train_rf)[i]
  train_pre = train_pre %>% select(names(train_rf)[-1])
  
  ###predict with los using model 
  tmp = as.data.frame( predict(rf_mod2,train_pre))
  colnames(tmp) = paste0(names(train_rf)[i],'_los')
  pred_los[[i-1]] = tmp
}

out =bind_cols(pred_los)
out_all = cbind(train_rf$Y, out)
colnames(out_all)[1]='Y'


png(filename="output/Figure3-Boxplots_train_los.png", width = 1000)
boxplot(out_all)
dev.off()



