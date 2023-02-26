attach(mtcars)
mtcars
n=nrow(mtcars)
colnames(mtcars)
y=mtcars[,"mpg"]
predictors_nm=c("hp","wt","disp")
x=mtcars[,predictors_nm]

Int=rep(1,n)
Int
X=as.matrix(cbind(Int,x))
X
##calculate X'X
Xt_X_x=t(X)%*%X
###########calulculate inverse of X'X##
inv_Xt_x_X=solve(Xt_X_x)
inv_Xt_x_X
###calculate beta
beta_hat=inv_Xt_x_X%*%t(X)%*%y
beta_hat
##Fit linear regression model using lm
####mpg=b0+b1*hp+b2*wt+b3*disp+e
fit=lm(mpg~hp+wt+disp,data=mtcars)
cbind(coef(fit),beta_hat)
###########3
plot(mtcars$hp,mtcars$mpg,pch=20,xlab="horse power",ylab="miles per gallon")
grid(col="grey")
abline(lm(mpg~hp,data=mtcars),col="red",lwd=2,lty=2)
####################
head(mtcars)
str(mtcars)
mtcars$cyl=as.factor(mtcars$cyl)
str(mtcars)
fit2=lm(mpg~cyl,data=mtcars)
fit2
summary(fit2)
