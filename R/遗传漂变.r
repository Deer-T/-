drift<-function(N,p0,G,times){
  M<-data.frame()
  for(m in 1:times){
    A<-2*N*p0
    for(i in 1:G){
      p<-A/(2*N)  #杂合子比例
      A<-rbinom(1,2*N,p)
      M[m,i]<-p
    }}
    return(M)
}
value<-drift(300,0.5,5000,50)
value2<-drift(3000,0.5,5000,50)
A<-value[,5000]
B<-value2[,5000]

df<-data.frame(A,B)  

library("ggplot2")

ggplot(df)+geom_histogram(aes(A),fill="red",alpha=0.5)+
  geom_histogram(aes(B),fill="green",alpha=0.5)+xlab("hetero")
 
