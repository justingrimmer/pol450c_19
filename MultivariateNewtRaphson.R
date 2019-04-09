##putting together the multivariate newton rapson 


score_func<- function(beta_0,beta_1, sigma, X, Y){
	part1<- (Y - beta_0 - beta_1*X)/sigma
	part2<- ((Y - beta_0 - beta_1*X)/sigma)*X
	part3<- -1/(2*sigma) + ((Y - beta_0 - beta_1*X)^2 )/(2*sigma^2)
	out<- c(sum(part1), sum(part2), sum(part3))
	}
	
hes_func<- function(beta_0, beta_1, sigma, X, Y){
	start<- matrix(0, nrow = 3, ncol = 3)
	start[1,1]<- length(Y)/sigma
	start[1,2]<- start[2,1]<- sum(X)/sigma
	start[2,2]<-  sum(X^2)/sigma
	start[3,3]<- length(Y)/(2 * sigma^2)
	return(- start)
	}	
	
	
X<- rnorm(1000)
Y<- 0.5 + 0.25*X + rnorm(1000)	

library(MASS)
guess<- mvrnorm(1, c(0,0, 0), diag(1,3))
guess[3]<- exp(guess[3])

newt_update<- function(guess, X, Y){
	c<- 0 
	
	count<- 0 
	old<- guess
	while(c==0){
	count<- count + 1

		
		if(count>1){
			old<- new
			
			}
		part1<- score_func(old[1], old[2], old[3], X, Y)
		part2<- hes_func(old[1], old[2], old[3], X, Y)
		
		
		new<- old - solve(part2)%*%part1
		
		
		if(count>2){
			diff<- abs(old - new)
			if(max(diff)<1e-5){
				c<- 1
				}
			}
			
			
			
		}
	return(new)
	
	
	
	}
	
	get_params<- newt_update(rnorm(3, 0.5, sd= 0.1), X, Y)
	
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

	analytic<- c(mle_linear(cbind(1, X), Y), mle_sigma(cbind(1, X), Y))
	

	
	
	
