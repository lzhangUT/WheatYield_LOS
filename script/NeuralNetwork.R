library(tidyverse)
library(neuralnet)

setwd('E:/ALLFED/WheatYield_LOS/')
##1.1 read in the cleaned file, and the selected the variables 
df = read.csv('data/dwheat_fit_elim.csv')
df_nn = df %>% dplyr::select(Y, n_total,  irrigation_tot,
                             mechanized, pesticides_H, thz_class,
                             mst_class, soil_class)


##1.2 Scale the Data
scale01 = function(x){(x - min(x)) / (max(x) - min(x))}
train_nn = df_nn %>% mutate_all(scale01)

##2.1 Build Neural Network
n = names(train_nn)
f = as.formula(paste("Y ~", paste(n[!n %in% "Y"], collapse = " + ")))
print(f)
nn = neuralnet(f,data=train_nn,hidden=c(3),linear.output=T)
saveRDS(nn,file='output/NeuralNetworkResults.rds')

##2.2 model performance on the training
train.nn = compute(nn,train_nn[,-1])

##2.3 Compute mean squared error on the train dataset
pred_train = train.nn$net.result * (max(train_nn$Y) - min(train_nn$Y)) + min(train_nn$Y)
obs_train = (train_nn$Y) * (max(train_nn$Y) - min(train_nn$Y)) + min(train_nn$Y)
MSE_train = sum((obs_train - pred_train)^2) / nrow(train_nn)
print(MSE_train)

##2.4 Plot the neural network
plot(nn)

##2.5 Plot regression line/Check the model performance---training dataset
reg_train = lm(train_nn$Y~pred_train)
r2 = summary(reg_train)$adj.r.squared

png(filename="output/Figure1a-ModelPerformanceNN_train.png")
plot(train_nn$Y, pred_train, xlab='Wheat Production_train (Kg/ha)',ylab='Wheat Prediction (Kg/ha)',
     xlim = c(0,1), ylim=c(0,1))
abline(0, 1, lwd = 2)

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 0.5, y = 0.8, labels = mylabel)
dev.off()

##3. variable importance from NN
require(devtools)
#import 'gar.fun' from Github
source_gist('6206737')

#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
plotdata = gar.fun('Y',nn)

plotdata + ylim(-1,1)+ylab('Relative Importance')
ggsave(('output/Figure2-VariableImportanceNN.png'))

####4. model performance on the test data
###4.1 read in the rest data and make prediciton using NN
test_y = read.csv('data/dwheat_val_elim.csv')
test_y = test_y %>% select(Y)
test_x = read.csv('data/w_val_elim.csv')
test_x = test_x %>% select(n_total, irrigation_tot,mechanized,
                            pesticides_H, thz_class, mst_class, soil_class)

test_nn = test_y %>% bind_cols(test_x) %>% mutate_all(scale01)

test.nn = compute(nn,test_nn[,-1])

##4.2 Compute mean squared error on the test dataset
pred_test = test.nn$net.result * (max(test_nn$Y) - min(test_nn$Y)) + min(test_nn$Y)
obs_test = (test_nn$Y) * (max(test_nn$Y) - min(test_nn$Y)) + min(test_nn$Y)
MSE_test= sum((obs_test - pred_test)^2) / nrow(test_nn)
print(MSE_test)


##4.3 Plot regression line/Check the model performance---test dataset
reg_test = lm(test_nn$Y~pred_test)
r2 = summary(reg_test)$adj.r.squared

png(filename="output/Figure1b-ModelPerformanceNN_test.png")
plot(test_nn$Y, pred_test, xlab='Wheat Production_test (Kg/ha)',ylab='Wheat Prediction (Kg/ha)',
     xlim = c(0,1), ylim=c(0,1))
abline(0, 1, lwd = 2)

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 0.5, y = 0.8, labels = mylabel)
dev.off()






