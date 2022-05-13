rm(list=ls())
library(tidyverse)
library(randomForest)
library(psych)

setwd('E:/ALLFED/WheatYield_ALLFED//')
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
df_rf = df_rf %>% dplyr::select(-p_fertilizer) %>% filter(complete.cases(.))
dim(df_rf)
##3. random forest on whole training set 'df_rf'
set.seed(1234)

###3.1 run RF with ntree=500, ntree=1000 didnt work as run out of memory
rf_mod = randomForest(Y~.,data=df_rf,importance=TRUE, ntree=500)
#saveRDS(rf_mod,file='output/RandomForestResults.rds')
print(rf_mod)

###3.2 Find the optimal mtry value
mtry <- tuneRF(df_rf[-1],df_rf$Y, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
print(mtry)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(best.m)

###3.3 build model with again using best mtry value
best.m=3
rf_mod2 = randomForest(Y~.,data=df_rf,importance=TRUE, ntree=500, mtry= best.m)
saveRDS(rf_mod2,file='output/RandomForestResults.rds')
print(rf_mod2)

importance(rf_mod2)
varImpPlot(rf_mod2)

##get the prediction from the model
pred_Y= predict(rf_mod2,df_rf)
obs_pre = cbind(data.frame(Y=df_rf$Y),data.frame(pred_Y=pred_Y))
obs_pre = as.data.frame(obs_pre)

###get correlation (R2) between the prediction and the observations
reg =lm(pred_Y~Y,data=obs_pre)
summary(reg)
eq_lm =paste0(expression(R^2),'==', round(summary(reg)$r.squared,2))
eq_lm = data.frame(eq_lm)
eq_lm$eq_lm = as.character(eq_lm$eq_lm)

ggplot(obs_pre,aes(Y,pred_Y))+geom_point()+
geom_abline()+theme_bw()+xlab('Wheat Production (Kg/ha)')+
  ylab('Wheat Prediciton (Kg/ha)')+
  theme(axis.text=element_text(size=12,face = 'bold'),
        text = element_text(size=15,face = 'bold'))+
  geom_text(data = eq_lm, aes(x = 3000, y = 9000, label = eq_lm, family = "serif"), 
            size=6, parse = TRUE) 

ggsave('output/Figure1-ModelPerformance.png')

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

##########predicting if loss of industry (los)

print(names(df_rf))

pred_Y_los= vector('list', 0)
for (i in 2:length(names(df_rf))){
  ###change each column into zero but name them the same 
  df_pre = df_rf %>% select(-i) %>% mutate('los'=0)
  colnames(df_pre)[ncol(df_pre)] = names(df_rf)[i]
  
  ###predict with los using model 
  tmp = as.data.frame( predict(rf_mod2,df_pre))
  colnames(tmp) = paste0(names(df_rf)[i],'_los')
  pred_Y_los[[i-1]] = tmp
}

out =bind_cols(pred_Y_los)
out_all = cbind(df_rf$Y, out)
colnames(out_all)[1]='Y'


png(filename="output/Figure5-Boxplots_los.png", width = 1000)
boxplot(out_all)
dev.off()



