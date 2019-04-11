load('~/dropbox/GoingPublic/Analyses/GrimmerMIP_Overall.RData')
pub_data<- na.omit(master_collect_mip)

gop<- ifelse(pub_data[,3]==1, 1, 0)

store<- table(pub_data[,1], pub_data[,2])

store[,1]/sum(store[,1])
store[,2]/sum(store[,2])

x_vals<- seq(-5, 5, len = 1000)
norms<- pnorm(x_vals)

plot(norms~x_vals, xlab = 'Y.tilde', ylab = 'Probability', type='l', lwd = 3, col='cornflowerblue')
abline(v = 0)
dev.copy(device=pdf, file='~/dropbox/teaching/pol350c/class4/NormCDF.pdf', height = 6, width = 6)
dev.off()


seq_p<- seq(0.0001, 0.9999, len = 10000)
logits<- log(seq_p/(1- seq_p))
plot(logits~seq_p, xlab = 'Probability', ylab = 'Log Odds', type='l', col='red', lwd = 3)
title(main = 'Logit')

dev.copy(device=pdf, file='~/dropbox/teaching/pol350c/class4/LogitFunc.pdf', height = 6, width = 6)
dev.off()


logistic<- function(a){
	ee<- 1/(1 + exp(-a))
	return(ee)
	}
	
logistics<- logistic(x_vals)
plot(logistics~x_vals, xlab = 'Y.tilde', ylab = 'Probability', type='l', lwd = 3, col='red')
abline(v = 0)
dev.copy(device=pdf, file='~/dropbox/teaching/pol350c/class4/LogisticCDF.pdf', height = 6, width = 6)
dev.off()

lines(norms~x_vals, type='l', col='cornflowerblue', lwd = 3)
dev.copy(device=pdf, file='~/dropbox/teaching/pol350c/class4/LogistvsNormCDF.pdf', height = 6, width = 6)
dev.off()


##############
##############
##############
##############
##############
##############

##data generating process for probit. 

library(MASS)
set.seed(8675309)
betas<- rnorm(4, sd = 1)
X<- cbind(1, mvrnorm(1000, mu = rep(0, 3), Sigma = diag(1, 3) ))
Y.tilde<- X %*% betas + rnorm(1000)

Y<- ifelse(Y.tilde>0, 1, 0)

plot(Y~X[,2])

abline(lm(Y~X[,2]))

probs<- glm(Y~X[,2], family = binomial(link = 'probit'))

vals<- seq(min(X[,2]), max(X[,2]), len = 1000)

points(pnorm(cbind(1, vals)%*%probs$coef )~vals, type='l', col='cornflowerblue', lwd = 2)





##############
##############
###Probit likelihood






##programming the likelihood function for a probit

prob_lik<- function(params, X, Y){
	beta<- params
	y.tilde<- X%*%beta
	y.prob<- pnorm(y.tilde)
	out<- Y%*%log(y.prob) + (1- Y)%*%log((1- y.prob))
	return(out)
	}
	
	
X<- cbind(1, pub_data[,2], gop)	
Y<- pub_data[,1]	
	
	
out<- optim(rnorm(3), prob_lik, method= 'BFGS', hessian=T, control = list(fnscale = -1), X = X,Y  = Y)
##optim inputs:
##starting values
##function
##method
##that you want the hessian
##control = list(fnscale = -1) make it a maximization problem
##then other inputs to your function

coef<- out$par	##extracting coefficients
ses<- sqrt(diag(solve(-out$hessian))) ##getting the hessian

##that provides the coefficients for the model
##and the standard errors.  


probit_glm<- glm(Y~X[,-1], family=binomial(link='probit')) ##comparing our results to the glm results





####quickly examining properties of probit estimator 

sims<- function(sims, N, betas, X, K, files, titles){
	beta_store<- matrix(NA, nrow = sims, ncol = K)
	for(z in 1:sims){
		y.tild<- X[1:N, ] %*% betas + rnorm(nrow(X[1:N,]))
		obs<- ifelse(y.tild>0, 1, 0)
		beta_store[z,]<- glm(obs~X[1:N,-1], family = binomial(link='probit'))$coef
	}
	par(mfrow=c(1, K))
	for(x in 1:K){
		plot(density(beta_store[,x]), main=titles)
		abline(v = betas[x], col='red', lwd = 2)
	}
	dev.copy(device = pdf, file=files, height = 6, width = 6)
	dev.off()


}

sims(1000, 10, betas, X, 4, '~/dropbox/teaching/pol350c/class4/probit10.pdf', '10 obs')
sims(1000, 100, betas, X, 4, '~/dropbox/teaching/pol350c/class4/probit100.pdf', '100 obs')
sims(1000, 250, betas, X, 4, '~/dropbox/teaching/pol350c/class4/probit250.pdf', '250 obs')
sims(1000, 500, betas, X, 4, '~/dropbox/teaching/pol350c/class4/probit500.pdf', '500 obs')
sims(1000, 1000, betas, X, 4, '~/dropbox/teaching/pol350c/class4/probit1000.pdf', '1000 obs')



###fitting the logit model.  
	
glm(Y~X[,-1], family=binomial(link = 'logit'))




	