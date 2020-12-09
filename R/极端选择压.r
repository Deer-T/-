#function
numb<-function(A,a,G,times,N){
  M<-data.frame()
  for(m in 1:times){
    n=N
    p1=A
    p2=a
    for(i in 1:G){
      
        p1<-rbinom(1,2*n,p1)/(n*2) #A
        p2<-rbinom(1,2*n,p2)/(n*2)#a
        n<-(p1*p1*0.006+0.0012*2*p1*p2)*n*n/4
        M[m,i]<-n
        p1<-(p1*p1*0.006*2+0.0012*2*p1*p2)/(p1*p1*0.006*2+0.0012*2*p1*p2*2)
        p2<-(0.0012*2*p1*p2)/(p1*p1*0.006*2+0.0012*2*p1*p2*2)
        n<-ceiling(n)
      
  }
  return(p1)
}}


#dataframe
library(ggplot2)
y=c()
for(i in 1:10){y[i]=mean(num2[,i])}
df<-data.frame(x=seq(1:10),y)

#plot
ggplot(df)+
  geom_point(aes(x,y),col="blue",alpha=0.5)+geom_smooth(aes(x,y),method = "gam",col="pink",alpha=0.5) 
 
