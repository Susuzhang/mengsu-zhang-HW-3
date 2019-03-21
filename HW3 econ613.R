library(bayesm)
data(margarine)
choiceprice<-margarine$choicePrice
demos<-margarine$demos
#******************************************************************************************************************
#exercise 1
#------------------------------------------------------------------------------------------------------------------
#average and distributions in product characteristics
apply(choiceprice[,3:12],2, mean)
apply(choiceprice[,3:12],2,mean)
apply(choiceprice[,3:12], 2, var)
#------------------------------------------------------------------------------------------------------------------
#market share by product characteristics
dec<-as.data.frame(matrix(rep(0,4470*10),nrow = 4470,ncol = 10))
for (i in 1:4470) {
  for (j in 1:10) {
    if(choiceprice[i,2]==j)
      dec[i,j]=1
  }
}#generate the decision choice matrix
prodt<-dec*choiceprice[,3:12]
proshare<-matrix(apply(prodt, 2, sum),nrow = 1,ncol = 10)
proshare<-proshare/sum(proshare) #market share by products
stk<-sum(prodt[1:6])
stkshare<-stk/sum(prodt)
tub<-sum(prodt[7:10])
tubshare<-tub/sum(prodt)#market share by forms
pk<-sum(prodt[1],prodt[8])/sum(prodt)
bb<-sum(prodt[2])/sum(prodt)
fi<-sum(prodt[3],prodt[9])/sum(prodt)
hse<-sum(prodt[4],prodt[10])/sum(prodt)
gen<-sum(prodt[5])/sum(prodt)
imp<-sum(prodt[6])/sum(prodt)
ss<-sum(prodt[7])/sum(prodt)
brandshare<-data.frame(c(pk,bb,fi,hse,gen,imp,ss))
View(brandshare)#market share by brands
#------------------------------------------------------------------------------------------------------------------
#mapping between attributes and choices
choice<-as.matrix(choiceprice[order(choiceprice[,2]),])
incomechoice<-merge(choiceprice,demos,by="hhid",all.x = TRUE)
incomechoice<-cbind.data.frame(incomechoice$Income,incomechoice$choice)
colnames(incomechoice)<-c("income","choice")
incomeodr<-as.data.frame(incomechoice[order(incomechoice[,1]),])
incomelvl<-as.data.frame(table(incomeodr$income))
colnames(incomelvl)<-c("income","choice")
lvl<-split(incomeodr,incomeodr$income,drop = TRUE)
a<-as.data.frame(matrix(nrow =0,ncol = 10))
colnames(a)<-c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
for (j in 1:14) {
 level<-as.data.frame(lvl[j])
 colnames(level)<-c("income","choice")
 b<-data.frame(matrix(nrow = 1,ncol = 0))
 for (i in 1:10) {
    num<-sum(level$choice==i)
  b<-cbind.data.frame(b,num)
 }
 colnames(b)<-c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
 a<-rbind(a,b)
}
map<-cbind(incomelvl[,1],a)
colnames(map)<-c("income","product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
View(map)
#The end of Exercise 1
#********************************************************************************************************************
#Exercise 2
#--------------------------------------------------------------------------------------------------------------------
product<-data.frame(choiceprice[,3:12])
x1<-data.frame(choiceprice[,3])
x1<-data.frame(rep(x1,10))
xtilta<-data.frame(product-x1)
#generate the decision choice matrix
dec<-as.data.frame(matrix(rep(0,4470*10),nrow = 4470,ncol = 10))
for (i in 1:4470) {
  for (j in 1:10) {
    if(choiceprice[i,2]==j)
      dec[i,j]=1
  }
}
zero<-matrix(rep(0,4470),nrow = 4470,ncol = 1)
probfunc<-function(theta){
  alpha<-matrix(rep(theta[2:10],each=4470),nrow = 4470,ncol = 9)
  V<-theta[1]*xtilta+cbind(zero,alpha)
  idprob<-exp(V)
  rprob<-as.data.frame(apply(idprob, 1, sum))
  prob1<-idprob/rprob[,1]
  lprob<-log(prob1)

  dec<-as.matrix(dec)
  lprob<-as.matrix(lprob)
  llike<--sum(dec * lprob)
  return(llike)
}
start<-c(-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505)
probfunc(start)

result1<-optim(par = start,probfunc)$par
beta1<-result1[1]
print(beta1)# the value of beta
#interpret:
#Since beta1<0, 
#we have that an increase in the price of one alternative decreases the probability of choosing of that alternatives
#and increases the choice probability of choosng other probabilities.


#******************************************************************************************************************
#Exercise 3
#------------------------------------------------------------------------------------------------------------------
incomechoice<-merge(choiceprice,demos,by="hhid",all.x = TRUE)
incm<-t(matrix(rep(t(incomechoice[,13]),each=10),nrow = 10,ncol = 4470))
probfunc<-function(theta){
  beta<-matrix(rep(theta[1:9],each=4470),nrow = 4470,ncol = 9)
  beta<-cbind(zero,beta)
  alpha2<-matrix(rep(theta[10:18],each=4470),nrow = 4470,ncol = 9)
  alpha2<-cbind(zero,alpha2)
  V<-beta*incm+alpha2
  idprob<-exp(V)
  rprob<-as.data.frame(apply(idprob, 1, sum))
  prob2<-idprob/rprob[,1]
  lprob<-log(prob2)
  
  dec<-as.matrix(dec)
  lprob<-as.matrix(lprob)
  llike<--sum(dec * lprob)
  return(llike)
}
instrt<-c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)
probfunc(instrt)

result2<-optim(par = instrt,probfunc)$par
beta2<-result2[1:9]
print(beta2)
#------------------------------------------------------------------------------------------------------------------
#interpret:
# Compared with pk_stk (which has been normalized to zero), 
#a higher income all leads to reduced likelihood from pb_stk(beta=-0.048),fi_stk(beta=-0.034),hse_stk(beta=-0.0293),
#gen_stk(-0.024),imp_stk(-0.0775),ss_tub( -0.09709),pk_tub(-0.04920),fi_tub(-0.03791),hse_tub(-0.056).

#******************************************************************************************************************
# Exercise 4 marginal effects
#------------------------------------------------------------------------------------------------------------------
# maginal effects of first model
alpha<-matrix(rep(result1[2:10],each=4470),nrow = 4470,ncol = 9)
V<-result1[1]*xtilta+cbind(zero,alpha)
idprob<-exp(V)
rprob<-as.data.frame(apply(idprob, 1, sum))
prob1<-idprob/rprob[,1]
prob1<-as.matrix(prob1)#generate the probability matrix of price
yita<-diag(1,10,10)
me1<-data.frame(matrix(rep(0,100),10,10))
for (i in 1:4470) {
  use<-matrix(rep(prob1[i,1:10],each=10),nrow = 10,ncol = 10)
  me1<-me1+t(use)*(yita-use)*beta1
}
me1<-me1/4470
View(me1)
#Interpret:
# The increase of price in product1 could make less probability to choose choice 1 and almost no effects on choocing
# other choices.
#------------------------------------------------------------------------------------------------------------------
#marginal effects of second model

beta<-matrix(rep(result2[1:9],each=4470),nrow = 4470,ncol = 9)
beta<-cbind(zero,beta)
alpha2<-matrix(rep(result2[10:18],each=4470),nrow = 4470,ncol = 9)
alpha2<-cbind(zero,alpha2)
V<-beta*incm+alpha2
idprob<-exp(V)
rprob<-as.data.frame(apply(idprob, 1, sum))
prob2<-idprob/rprob[,1]#generate the probability matrix of income
beta2<-c(0,beta2)
use2<-matrix(rep(beta2,each=4470),nrow = 4470,ncol=10)
use2<-prob2*use2
betaba<-as.data.frame(apply(use2, 1, sum))
betabarep<-t(matrix(rep(t(betaba[,1]),each=10),nrow = 10,ncol = 4470))
betarep<-matrix(rep(beta2,each=4470),nrow = 4470,ncol = 10)
me2<-prob2*(betarep-betabarep)
me2<-as.data.frame(apply(me2, 2, sum))
View(me2[,1]/4470)
#Interpret:
#The increase in any level of income  almost has no effects on the probability of choosing products.

#******************************************************************************************************************
#Exercise 5
#------------------------------------------------------------------------------------------------------------------
#5.1
func3<-function(theta){
  alpha<-matrix(rep(theta[2:10],each=4470),nrow = 4470,ncol = 9)
  V1<-theta[1]*xtilta+cbind(zero,alpha)
  incm<-t(matrix(rep(t(incomechoice[,13]),each=10),nrow = 10,ncol = 4470))
  beta<-matrix(rep(theta[11:19],each=4470),nrow = 4470,ncol = 9)
  beta<-cbind(zero,beta)
  alpha2<-matrix(rep(theta[20:28],each=4470),nrow = 4470,ncol = 9)
  alpha2<-cbind(zero,alpha2)
  V2<-beta*incm+alpha2
  V3<-V1+V2
  idprob<-exp(V3)
  rprob<-as.data.frame(apply(idprob, 1, sum))
  prob3<-idprob/rprob[,1]
  lprob<-log(prob3)
  
  dec<-as.matrix(dec)
  lprob<-as.matrix(lprob)
  llike<--sum(dec * lprob)
  return(llike)
}
start3<-c(-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
result3<-optim(par = start3,func3)$par
beta3<-result3[1]

yita3<-result3[11:19]
betaf<-c(beta3,0,result3[2:10],0,yita3,0,result3[20:28])
print(betaf)
#-------------------------------------------------------------------------------------------------------------------
#5.2
decchoice<-cbind(incomechoice,dec)
dropchoice<-decchoice[!decchoice$choice==2,]
dropchoice2<-subset(dropchoice,select =  c("choice","PPk_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPk_Tub","PFl_Tub","PHse_Tub","Income"))
#generate the decision matrix
dec2<-subset(dropchoice,select = c("V1","V3","V4","V5","V6","V7","V8","V9","V10"))
product2<-data.frame(dropchoice2[,2:10])
x2<-data.frame(dropchoice2[,2])
x2<-data.frame(rep(x2,9))
xtilta2<-data.frame(product2-x2)
zero2<-matrix(rep(0,3771),nrow = 3771,ncol = 1)
incm2<-t(matrix(rep(t(dropchoice2$Income),each=9),nrow = 9,ncol = 3771))

func4<-function(theta){
  alpha<-matrix(rep(theta[2:9],each=3771),nrow = 3771,ncol = 8)
  V1<-theta[1]*xtilta2+cbind(zero2,alpha)
  beta<-matrix(rep(theta[10:17],each=3771),nrow = 3771,ncol = 8)
  beta<-cbind(zero2,beta)
  alpha2<-matrix(rep(theta[18:25],each=3771),nrow = 3771,ncol = 8)
  alpha2<-cbind(zero2,alpha2)
  V2<-beta*incm2+alpha2
  V3<-V1+V2
  idprob<-exp(V3)
  rprob<-as.data.frame(apply(idprob, 1, sum))
  prob4<-idprob/rprob[,1]
  lprob<-log(prob4)
  
  dec2<-as.matrix(dec2)
  lprob<-as.matrix(lprob)
  llike<--sum(dec2 * lprob)
  return(llike)
}
start4<-c(-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,-0.505,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
result4<-optim(par = start4,func4)$par
beta4<-result4[1]
yita4<-result3[10:17]

betar<-c(beta4,0,result4[2:9],0,yita4,result4[18:25])
print(betar)
#-------------------------------------------------------------------------------------------------------------------
#5.3
MTT<--2*(func3(start3)-func4(start4))
chisq.test(abs(betar))
#Interpret:
#Since MTT<chisquare and p-value equals to 1, we fail to reject that MTT=0, 
#which means there is no significant difference and IIA is not violated,
#the choice probabilities are unaffected by the removal of one alternative.



