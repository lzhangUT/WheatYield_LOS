library(tidyverse)
library(neuralnet)

setwd('E:/ALLFED/WheatYield_ALLFED//')
##1. read in the cleaned file, and the selected the variables 
df = read.csv('data/dwheat_fit_elim.csv')
df_rf = df %>% dplyr::select(Y, n_total,  irrigation_tot,
                             mechanized, pesticides_H, thz_class,
                             mst_class, soil_class)


# Scale the Data
scale01 = function(x){(x - min(x)) / (max(x) - min(x))}
df_rf = df_rf %>% mutate_all(scale01)

# Build Neural Network
n = names(df_rf)
f = as.formula(paste("Y ~", paste(n[!n %in% "Y"], collapse = " + ")))
print(f)
nn = neuralnet(f,data=df_rf,hidden=c(3),linear.output=T)
saveRDS(nn,file='output/NeuralNetworkResults.rds')
# Predict on data
pr.nn <- compute(nn,df_rf[,-1])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(df_rf$Y) - min(df_rf$Y)) + min(df_rf$Y)
test.r <- (df_rf$Y) * (max(df_rf$Y) - min(df_rf$Y)) + min(df_rf$Y)
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(df_rf)
print(MSE.nn)

# Plot the neural network
plot(nn)

# Plot regression line/Check the model performance
reg = lm(df_rf$Y~pr.nn_)
reg_sum = summary(reg)
r2 = reg_sum$adj.r.squared

png(filename="output/Figure3-ModelPerformanceNN.png")
plot(df_rf$Y, pr.nn_, xlab='Observed',ylab='Predicted',
     xlim = c(0,1), ylim=c(0,1))
abline(0, 1, lwd = 2)

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 0.5, y = 0.8, labels = mylabel)
dev.off()

#####variable importance from NN
require(devtools)
#import 'gar.fun' from Github
source_gist('6206737')

#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(7)
#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
plotdata = gar.fun('Y',nn)

plotdata + ylim(-1,1)+ylab('Relative Importance')
ggsave(('output/Figure4-VariableImportanceNN.png'))

