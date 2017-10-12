setwd("~/Documents/JSOM/Fall'18/ML/Project1/Project1")
#library(caret)
library(corrplot)
library(ggplot2)
library(reshape2)

#setting seed to reproduce the results
set.seed(914)

#read the data file
hours<-read.csv("hour.csv",header=TRUE,sep=',')
View(hours)

#checking dimension of the dataset
dim(hours)

#checking NA in any of the columns in the dataset
summary(is.na(hours))

#summary of the data
summary(hours)

#correlation plot
hours_cor<-hours[,c(3:14)]
Correlation<-cor(hours_cor)
corrplot::corrplot(Correlation,method="circle")

#training and test split
sample_size<-floor(0.7*nrow(hours))
index<-sample(seq_len(nrow(hours)),size=sample_size,replace = FALSE)
training<-hours[index,]
test<-hours[-index,]

#removing variables with high correlation like season, as it is highly correlated with month, and temp, as it is highly correlated with atemp.
drops <- c("instant","dteday","season","yr","temp","casual","registered")
training<-training[ , !(names(training) %in% drops)]
test<-test[,!(names(test) %in% drops)]

#extracting the response variable and making a vector
y<-training$cnt
y<-data.matrix(y)

#removing the dependent variable from the training set and making a matrix of it
training<-training[,-10]
#scaling the features
m<-max(training$mnth)
n<-min(training$mnth)
training$mnth<-(training$mnth-n)/(m-n)
m<-max(training$hr)
n<min(training$hr)
training$hr<-(training$hr-n)/(m-n)
m<-max(training$weekday)
n<-min(training$weekday)
training$weekday<-(training$weekday-n)/(m-n)
m<- max(training$weathersit)
n<-min(training$weathersit)
training$weathersit<-(training$weathersit-n)/(m-n)


#making a matrix from the data frame
training_matrix<-data.matrix(training)

# adding 1 for theta0
n_row<-nrow(training_matrix)
training_matrix<-cbind(rep(1,n_row),training_matrix)


#test matrix
#scaling the features
test_matrix<-test
test_y<-test[,10]
test_y<-data.matrix(test_y)
test_matrix<-test_matrix[,-10]
m<-max(test_matrix$mnth)
n<-min(test_matrix$mnth)
test_matrix$mnth<-(test_matrix$mnth-n)/(m-n)
m<-max(test_matrix$hr)
n<-min(test_matrix$hr)
test_matrix$hr<-(test_matrix$hr-n)/(m-n)
m<-max(test_matrix$weekday)
n<-min(test_matrix$weekday)
test_matrix$weekday<-(test_matrix$weekday-n)/(m-n)
m<- max(test_matrix$weathersit)
n<-min(test_matrix$weathersit)
test_matrix$weathersit<-(test_matrix$weathersit-n)/(m-n)
#m<-max(test_matrix$windspeed)
#test_matrix$windspeed<-test_matrix$windspeed/m
test_n_row<-nrow(test_matrix)
test_matrix<-data.matrix(test_matrix)
test_matrix<-cbind(rep(1,test_n_row),test_matrix)

#initializing theta
#theta<-c(30 ,60 , 180,-20,8.5, 5.666, -10,350,-210, 26)
theta<-c(0,0,0,0,0,0,0,0,0,0)

#function to compute cost at each iteration
cost_iter<-function(training_matrix, y, theta){
  m <- length(y)
  J <- sum((training_matrix%*%theta- y)^2)/(2*m)
  return(J)
}


#gradient descent function
gradientDescent<-function(training_matrix, y, theta, alpha, num_iters,threshold,test_matrix,y_test){
  m <- length(y)
  J_iter <- rep(0, num_iters)
  J_test<-rep(0,num_iters)
  iter=0
  for(i in 1:num_iters){
    theta <- theta - alpha*(1/m)*(t(training_matrix)%*%(training_matrix%*%theta - y))
    J_iter[i]  <- cost_iter(training_matrix, y, theta)
    J_test[i] <-cost_iter(test_matrix,y_test,theta)
    print(c(i,J_iter[i]))
    iter=i
   if(i>100){
   if (((J_iter[i-1]-J_iter[i]<= threshold) | (J_iter[i] >=J_iter[i-20]))) break}

  }
  result<-list(J_iter,theta,iter,J_test)
  return(result)
}

alpha<-0.01
threshold<-0.00005
num_iteration=50000
results<-gradientDescent(training_matrix,y,theta,alpha,num_iteration,threshold,test_matrix,test_y)
theta=results[[2]]
cost=results[[1]]
print(theta)
#print(cost)


y_pred<- training_matrix %*% theta
training_matrix_pred<-cbind(training_matrix,y,y_pred)
squared_error_train<- sum((training_matrix_pred[,11]-training_matrix_pred[,12])^2)/(2*n_row)
squared_error_train


test_pred<- test_matrix %*% theta
test_matrix_pred<-cbind(test_matrix,test_pred)
test_matrix_pred<-cbind(test_matrix_pred,test_y)
squared_error_test<- sum((test_matrix_pred[,11]-test_matrix_pred[,12])^2)/(2*test_n_row)
squared_error_test



# training parameters
alpha <- c(0.001,0.003,0.01,0.1)
theta_res<-data.frame(matrix(,nrow=10,ncol=4))
num_iteration <- 100000
threshold=0.0005
cost=list()
cost_test=list()
theta=list()
iter=list()
for(i in 1:4){
theta=rep(0,10)
results <- gradientDescent(training_matrix, y, theta, alpha[i], num_iteration,threshold,test_matrix,test_y)
cost[i]=results[1]
theta_res[,i]=results[[2]]
iter[i]=results[[3]]
cost_test[i]=results[4]
}

#train cost plot
cost_1<-cost[[1]][1:20000]
cost_2<-cost[[2]][1:20000]
cost_3<-cost[[3]][1:20000]
cost_4<-cost[[4]][1:20000]
iter<-1:20000
cost_plot<-data.frame(iteration=iter,alpha0.001=cost_1,alpha0.003=cost_2,alpha0.01=cost_3,alpha0.1=cost_4)
cost_plot<-melt(cost_plot,id.vars='iteration',variable.name = 'cost')
train_cost_plot<-ggplot(cost_plot, aes(iteration,value)) + geom_line(aes(colour = cost))
print(train_cost_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Train Iteration cost as a function of alpha"))
cost[[1]]<-cost[[1]][cost[[1]]>0]
cost[[2]]<-cost[[2]][cost[[2]]>0]
cost[[3]]<-cost[[3]][cost[[3]]>0]
cost[[4]]<-cost[[4]][cost[[4]]>0]


#test cost plot
#train cost plot
test_cost_1<-cost_test[[1]][1:20000]
test_cost_2<-cost_test[[2]][1:20000]
test_cost_3<-cost_test[[3]][1:20000]
test_cost_4<-cost_test[[4]][1:20000]
iter<-1:20000
test_cost_plot<-data.frame(iteration=iter,alpha0.001=test_cost_1,alpha0.003=test_cost_2,alpha0.01=test_cost_3,alpha0.1=test_cost_4)
test_cost_plot<-melt(test_cost_plot,id.vars='iteration',variable.name = 'cost')
test_cost_plot<-ggplot(test_cost_plot, aes(iteration,value)) + geom_line(aes(colour = cost))
print(test_cost_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Test Iteration cost as a function of alpha"))


cost_test[[1]]<-cost_test[[1]][cost_test[[1]]>0]
cost_test[[2]]<-cost_test[[2]][cost_test[[2]]>0]
cost_test[[3]]<-cost_test[[3]][cost_test[[3]]>0]
cost_test[[4]]<-cost_test[[4]][cost_test[[4]]>0]


# print(iter)
# print(cost)
print(theta_res[,4])
theta<-theta_res[,4]
# 
# cost <- results[[1]]
# theta <- results[[2]]
# print(theta)


#predicting the response on the training data
y_pred<- training_matrix %*% theta
training_matrix_pred<-cbind(training_matrix,y,y_pred)
squared_error_train<- sum((training_matrix_pred[,11]-training_matrix_pred[,12])^2)/(2*n_row)
squared_error_train

#test matrix prediction
#scaling the features
# test_matrix<-test
# test_y<-test[,10]
# test_y<-data.matrix(test_y)
# test_matrix<-test_matrix[,-10]
# m<-max(test_matrix$mnth)
# test_matrix$mnth<-test_matrix$mnth/m
# m<-max(test_matrix$hr)
# test_matrix$hr<-test_matrix$hr/m
# m<-max(test_matrix$weekday)
# test_matrix$weekday<-test_matrix$weekday/m
# m<- max(test_matrix$weathersit)
# test_matrix$weathersit<-test_matrix$weathersit/m
# #m<-max(test_matrix$windspeed)
# #test_matrix$windspeed<-test_matrix$windspeed/m
# test_n_row<-nrow(test_matrix)
# test_matrix<-data.matrix(test_matrix)
# test_matrix<-cbind(rep(1,test_n_row),test_matrix)

test_pred<- test_matrix %*% theta
test_matrix_pred<-cbind(test_matrix,test_pred)
test_matrix_pred<-cbind(test_matrix_pred,test_y)
squared_error_test<- sum((test_matrix_pred[,11]-test_matrix_pred[,12])^2)/(2*test_n_row)
squared_error_test

############ function of alpha #########################################

error_function<-function(theta,train_matrix,test_matrix){
  y_pred<- training_matrix %*% theta
  training_matrix_pred<-cbind(training_matrix,y,y_pred)
  squared_error_train<- sum((training_matrix_pred[,11]-training_matrix_pred[,12])^2)/(2*n_row)
  test_pred<- test_matrix %*% theta
  test_matrix<-cbind(test_matrix,test_pred)
  test_matrix<-cbind(test_matrix,test_y)
  squared_error_test<- sum((test_matrix[,11]-test_matrix[,12])^2)/(2*test_n_row)
  return(c(squared_error_train,squared_error_test))
  
}

error_log<-data.frame(matrix(,3,4))
for(i in 1:4){
  results<-error_function(theta_res[,i],training_matrix,test_matrix)
  error_log[1,i]<-alpha[i]
  error_log[2,i]<-results[[1]]
  error_log[3,i]<-results[[2]]
}
print(error_log)


########################## function of threshold############################

alpha <- 0.1
theta_threshold<-data.frame(matrix(,nrow=10,ncol=4))
num_iteration <- 100000
threshold=c(0.0001,0.00001,0.000001,0.0000001)
cost_threshold=list()
cost_test=list()
theta=list()
iter_threshold=list()
for(i in 1:4){
  theta=rep(0,10)
  results <- gradientDescent(training_matrix, y, theta, alpha, num_iteration,threshold[i],test_matrix,test_y)
  cost_threshold[i]=results[1]
  theta_threshold[,i]=results[[2]]
  iter_threshold[i]=results[3]
  cost_test[i]=results[4]
}


#train cost plot
cost_1<-cost[[1]][1:20000]
cost_2<-cost[[2]][1:20000]
cost_3<-cost[[3]][1:20000]
cost_4<-cost[[4]][1:20000]
iter<-1:20000
cost_plot<-data.frame(iteration=iter,t10_4=cost_1,t10_5=cost_2,t10_6=cost_3,t10_7=cost_4)
cost_plot<-melt(cost_plot,id.vars='iteration',variable.name = 'cost')
train_cost_plot<-ggplot(cost_plot, aes(iteration,value)) + geom_line(aes(colour = cost))
print(train_cost_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Train Iteration cost as a function of threshold"))
# cost[[1]]<-cost[[1]][cost[[1]]>0]
# cost[[2]]<-cost[[2]][cost[[2]]>0]
# cost[[3]]<-cost[[3]][cost[[3]]>0]
# cost[[4]]<-cost[[4]][cost[[4]]>0]

#test cost plot
#train cost plot
test_cost_1<-cost_test[[1]][1:20000]
test_cost_2<-cost_test[[2]][1:20000]
test_cost_3<-cost_test[[3]][1:20000]
test_cost_4<-cost_test[[4]][1:20000]
iter<-1:20000
test_cost_plot<-data.frame(iteration=iter,t10_4=test_cost_1,t10_5=test_cost_2,t10_6=test_cost_3,t10_7=test_cost_4)
test_cost_plot<-melt(test_cost_plot,id.vars='iteration',variable.name = 'cost')
test_cost_plot<-ggplot(test_cost_plot, aes(iteration,value)) + geom_line(aes(colour = cost))
print(test_cost_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Test Iteration cost as a function of threshold"))


##single plot for train and test for threshold of 0.0000001
jg<-rep(0,17751)
train_cost_4<-c(cost[[4]],jg)
threshold_cost_df<-data.frame(iteration=iter,train=train_cost_4,test=test_cost_4)
threshold_train_test_plot<-ggplot(threshold_cost_df,aes(iter))+geom_line(aes(y=train), colour="red")+geom_line(aes(y=test), colour="green")+scale_colour_manual(breaks=c("train","test"),values=c("train"="red","test"="green"))
print(threshold_train_test_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Train Iteration cost for thresh 0.0000001"))



## train and test error for different thetas from different thresholds
# print(cost_threshold)
print(theta_threshold)
print(iter_threshold)

error_log<-data.frame(matrix(,3,4))
for(i in 1:4){
  results<-error_function(theta_threshold[,i],training_matrix,test_matrix)
  error_log[1,i]<-threshold[i]
  error_log[2,i]<-results[[1]]
  error_log[3,i]<-results[[2]]
}
print(error_log)


##old code
cost_1<-cost[[1]][1:20000]
cost_2<-cost[[2]][1:20000]
cost_3<-cost[[3]][1:20000]
cost_4<-cost[[4]][1:20000]
iter<-1:20000
cost_plot<-data.frame(iteration=iter,t1e4=cost_1,t1e5=cost_2,t1e6=cost_3,t1e7=cost_4)
cost_plot<-melt(cost_plot,id.vars='iteration',variable.name = 'cost')
threshold_plot<-ggplot(cost_plot, aes(iteration,value)) + geom_line(aes(colour = cost))
print(threshold_plot+labs(y="Gradient Cost",x="Iteration")+ggtitle("Iteration cost as a function of threshold"))

############### Pick 3 random features ####################################

#As the dataset has 9 features after removing the correlated features, generating 3 random numbers to pick the 3 features from the 9 features 
rand_three<-sample(1:9,3,replace=F)
print(rand_three)

#got 9,7 and 8 as the random numbers from the above function
#so picking features atemp,hum and windspeed as the feature for training the data

training_matrix_3F<-training_matrix[,c(1,8:10)]
View(training_matrix_3F)
test_matrix_3F<-test_matrix[,c(1,8:10)]
View(test_matrix_3F)
theta=c(0,0,0,0)


#function to compute cost at each iteration
cost_iter<-function(training_matrix, y, theta){
  m <- length(y)
  J <- sum((training_matrix%*%theta- y)^2)/(2*m)
  return(J)
}


#gradient descent function
gradientDescent<-function(training_matrix, y, theta, alpha, num_iters,threshold){
  m <- length(y)
  J_iter <- rep(0, num_iters)
  for(i in 1:num_iters){
    theta <- theta - alpha*(1/m)*(t(training_matrix)%*%(training_matrix%*%theta - y))
    J_iter[i]  <- cost_iter(training_matrix, y, theta)
    print(c(i,J_iter[i]))
    if(i>10000){
      if (((J_iter[i-1]-J_iter[i]<= threshold) | (J_iter[i] > J_iter[i-1]))) break}
  }
  result<-list(J_iter,theta)
  return(result)
}

# training parameters
alpha <- 0.1
num_iteration <- 150000
threshold=0.0000001
results <- gradientDescent(training_matrix_3F, y, theta, alpha, num_iteration,threshold)

cost <- results[[1]]
theta <- results[[2]]
print(theta)


# calculating train and test error
y_pred<- training_matrix_3F %*% theta
training_matrix_pred_3F<-cbind(training_matrix_3F,y,y_pred)
squared_error_train_3F<- sum((training_matrix_pred_3F[,5]-training_matrix_pred_3F[,6])^2)/(2*n_row)
squared_error_train_3F


test_pred_3F<- test_matrix_3F %*% theta
test_matrix_3F<-cbind(test_matrix_3F,test_pred_3F)
test_matrix_3F<-cbind(test_matrix_3F,test_y)
squared_error_test_3F<- sum((test_matrix_3F[,5]-test_matrix_3F[,6])^2)/(2*test_n_row)
squared_error_test_3F


#################### 3 Features that are best suited to predict the output #####################################

#Picking holiday, working day and weather situation as the 3 features that are best suited to predict the output.
training_matrix_B3F<-training_matrix[,c(1,3,6,7)]#3,6,7
View(training_matrix_B3F)
test_matrix_B3F<-test_matrix[,c(1,3,6,7)]
View(test_matrix_B3F)
theta=c(0,0,0,0)


# training parameters
alpha <- 0.1
num_iteration <- 150000
threshold=0.0005
results <- gradientDescent(training_matrix_B3F, y, theta, alpha, num_iteration,threshold)

cost <- results[[1]]
theta <- results[[2]]
print(theta)


# calculating train and test error
y_pred<- training_matrix_B3F %*% theta
training_matrix_pred_B3F<-cbind(training_matrix_B3F,y,y_pred)
squared_error_train_B3F<- sum((training_matrix_pred_B3F[,5]-training_matrix_pred_B3F[,6])^2)/(2*n_row)
squared_error_train_B3F


test_pred_B3F<- test_matrix_B3F %*% theta
test_matrix_B3F<-cbind(test_matrix_B3F,test_pred_B3F)
test_matrix_B3F<-cbind(test_matrix_B3F,test_y)
squared_error_test_B3F<- sum((test_matrix_B3F[,5]-test_matrix_B3F[,6])^2)/(2*test_n_row)
squared_error_test_B3F
