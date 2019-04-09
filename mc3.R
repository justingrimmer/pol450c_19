#####
#####
###Grimmer Code for Lecture 3, POL 350C
#####
#####
#####


mle_linear<- function(x, y){
	out<- solve(t(x)%*%x)%*%t(x)%*%y
	return(out)
	}


mle_sigma<- function(x, y){
	coef<- mle_linear(x, y)
	preds<- x%*%coef
	diff<- y - preds
	ssr<- sum( (diff)^2)/length(y)
	return(ssr)
	}

var_cov<- function(x, y){
	sigs<- mle_sigma(x, y)
	coef_var<- sigs*solve(t(x)%*%x)
	sig_var<- (2 * (sigs)^2)/(length(y))
	out<- matrix(0, nrow = ncol(x) + 1, ncol = ncol(x) + 1)
	out[1:ncol(x), 1:ncol(x)]<- coef_var
	out[ncol(x) + 1, ncol(x) + 1]<- sig_var
	return(out)
	}
	
	
x<- rnorm(1000)
y<- 0.5 + 0.25*x + rnorm(1000)	

coef_test<- mle_linear(cbind(1, x), y)	
sig_test<- mle_sigma(cbind(1, x) , y)	
	
var_er<- var_cov(cbind(1, x), y)

##conducting a simulation to examine the properties

n<- c(10, 50, 100, 1000, 10000)
sims<- 1000
store_list<- list()
a<- 0 
for(z in n){
	a<- a + 1
	store_coef<- matrix(NA, nrow = 1000, ncol = 3)
	store_sims<- list()
	count<- 0 
	for(i in 1:sims){
		count<- count + 1
		x<- rnorm(z)
		y<- 0.5 + 0.25*x + rnorm(z)
		store_coef[count,1:2]<- mle_linear(cbind(1, x), y)
		store_coef[count, 3]<- mle_sigma(cbind(1, x), y)
		store_sims[[i]]<- var_cov(cbind(1, x), y)
		}
		store_list[[a]]<- list(store_coef, store_sims)
	}
	
	
##how well does our model perform on average across different sample sizes
actual_ses<- matrix(NA, nrow = 5, ncol = 3)
est_ses<- list()
for(z in 1:5){
	ses_1<- matrix(NA, nrow = 1000, ncol = 3)
	actual_ses[z,]<- apply(store_list[[z]][[1]], 2, sd)
	for(k in 1:1000){
		ses_1[k,]<- sqrt(diag(store_list[[z]][[2]][[k]]))
		}
	est_ses[[z]]<- ses_1
	}

names<- c('beta0', 'beta1', 'sigma2')
truth<- c(0.5, 0.25, 1)
for(z in 1:5){
	par(mfrow = c(1, 3))
	for(j in 1:3){
		plot(density(store_list[[z]][[1]][,j]), main = paste(names[j], n[z], sep=' ') , xlim=c(-0.5, 2.5), xlab ='')
		abline(v = truth[j], lwd = 2)
		abline(v = mean(store_list[[z]][[1]][,j]), col='red', lwd=2)
		}

ee<- '~/dropbox/teaching/pol350c/class3/SimDists'
ff<- paste(paste(ee, z, sep='_'), '.pdf', sep='')
dev.copy(device=pdf, file=ff, height = 6, width = 8)
dev.off()
}	
	
	
	
	
	
	




	
