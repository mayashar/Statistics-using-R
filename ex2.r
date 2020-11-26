red_P_J<-0.50
red_P_B<-0.20
red_P_both<-red_P_J*red_P_B  # both are independent
paste(red_P_both*100, "%")

#t-test=M-mu/(s/root n)
M=68.5
mu=68
s=3
a=sqrt(25)
t<-(M-mu)/(3/5)
t

pnorm(68,68.5,3)
