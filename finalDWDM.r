library(lattice)
setwd("D:\\")

cust_inp = as.data.frame(read.csv("new3.csv"))
location = matrix(0,1,494)
j=1
#result = speedglm(CrashStatus ~ Aboard + Month + Day + as.character(Time)  ,cust_inp,family=binomial(link="logit"))
for(i in 1:dim(cust_inp)[1]){
  lname = as.character(cust_inp[i,8])
  if(!(lname %in% location) ){
    location[1,j] = lname
    j = j+1
  }
  
}


operator = matrix(0,1,2493)
j=1
#result = speedglm(CrashStatus ~ Aboard + Month + Day + as.character(Time)  ,cust_inp,family=binomial(link="logit"))
for(i in 1:dim(cust_inp)[1]){
  opname = as.character(cust_inp[i,9])
  if(!(opname %in% operator) ){
    operator[1,j] = opname
    j = j+1
  }
  
}

type = matrix(0,1,2436)
j=1
#result = speedglm(CrashStatus ~ Aboard + Month + Day + as.character(Time)  ,cust_inp,family=binomial(link="logit"))
for(i in 1:dim(cust_inp)[1]){
  tname = as.character(cust_inp[i,12])
  if(!(tname %in% type) ){
    type[1,j] = tname
    j = j+1
  }
  
}


############
cust_inp = as.data.frame(read.csv("train.csv"))
dim(cust_inp)
preMatrix = matrix(0,nrow = 5236,ncol = 20)
preMatrix[,1] = cust_inp[,1]
preMatrix[,2] = cust_inp[,2]
preMatrix[,3] = cust_inp[,3]
preMatrix[,4] = cust_inp[,4]
preMatrix[,5] = cust_inp[,5]
preMatrix[,6] = cust_inp[,6]
preMatrix[,7] = cust_inp[,7]
preMatrix[,15] = cust_inp[,15]
preMatrix[,16] = cust_inp[,16]
preMatrix[,19] = 0
preMatrix[,8] = 0



for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,9] = match(cust_inp[i,9],location)
}


for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,10] = match(cust_inp[i,10],operator)
}

for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,13] = match(cust_inp[i,13],type)
}


for(i in 1:dim(cust_inp)[1]){
  if(as.character(cust_inp[i,20]) == 'Y'){
    preMatrix[i,20] = 1
    
  }
  else if(as.character(cust_inp[i,20]) == 'N'){
    preMatrix[i,20] = 0
    
  }
  else{
    preMatrix[i,20] = 0.5
  }
}

colnames(preMatrix) = colnames(cust_inp)
preMatrix=as.data.frame(preMatrix) 

results2 <- glm(CrashStatus ~  Month  + Location + Operator + Aboard, preMatrix,family=binomial(link="logit"))
summary(results2)

######################
cust_inp = as.data.frame(read.csv("test.csv"))

preMatrix = matrix(0,nrow = 5236,ncol = 20)
preMatrix[,1] = cust_inp[,1]
preMatrix[,2] = cust_inp[,2]
preMatrix[,3] = cust_inp[,3]
preMatrix[,4] = cust_inp[,4]
preMatrix[,5] = cust_inp[,5]
preMatrix[,6] = cust_inp[,6]
preMatrix[,7] = cust_inp[,7]
preMatrix[,15] = cust_inp[,15]
preMatrix[,16] = cust_inp[,16]
preMatrix[,19] = 0
preMatrix[,8] = 0



for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,9] = match(cust_inp[i,9],location)
}


for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,10] = match(cust_inp[i,10],operator)
}

for(i in 1:dim(cust_inp)[1]){
  preMatrix[i,13] = match(cust_inp[i,13],type)
}


for(i in 1:dim(cust_inp)[1]){
  if(as.character(cust_inp[i,20]) == 'Y'){
    preMatrix[i,20] = 1
    
  }
  else if(as.character(cust_inp[i,20]) == 'N'){
    preMatrix[i,20] = 0
    
  }
  else{
    preMatrix[i,20] = 0.5
  }
}

colnames(preMatrix) = colnames(cust_inp)
preMatrix=as.data.frame(preMatrix)
answ=0
matchValues=0

for(i in 1:dim(cust_inp)[1]){
  
  Month = as.numeric(preMatrix[i,5])
  Location = as.numeric(preMatrix[i,9])
  Operator = as.numeric(preMatrix[i,10])
  Aboard = as.numeric(preMatrix[i,16])
  
  y = 1.013e+00 + -1.674e-04*Month  + 2.007e-03*Location+ -1.025e-03*Operator + 5.544e-04 *Aboard
  prob = exp(y)/(1+exp(y))
  #print(prob)
  if(prob > 0.4){
    if(preMatrix[i,20] == 1){
      matchValues = matchValues + 1
    }
    
  }
  else {
    if(preMatrix[i,20] == 0){
      matchValues = matchValues + 1
    }
  }
  
  
}

print("Accuracy in percent")
print(matchValues*100/dim(cust_inp)[1])