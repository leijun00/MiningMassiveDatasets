#Question 1
M <- matrix(data=c(0,1/2,1/2,0,0,1,0,0,1),nrow=3, ncol=3)
R <- matrix(data=c(1/3,1/3,1/3),nrow=3, ncol=1)
v <- matrix(data=c(1/3,1/3,1/3),nrow=3, ncol=1)
beta <- 0.7
esp <- 0.001
last_v <- t(t(c(0,0,0)))
while(sum((last_v-v)^2) > esp){
    last_v <- v
    v <- (beta*M) %*% v + (1-beta)*R
}  
print(3*v)

#Question 3
M <- matrix(data=c(0,1/2,1/2,0,0,1,1,0,0),nrow=3, ncol=3)
v <- matrix(data=c(1,1,1),nrow=3, ncol=1)
for(i in 1:5){
    v <- M %*% v
    print(v)
}
esp <- 0.001
last_v <- t(t(c(0,0,0)))
while(sum((last_v-v)^2) > esp){
    last_v <- v
    v <- M %*% v
}
print(v)

