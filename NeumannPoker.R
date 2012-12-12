npoker <- function(a = 2, b = 1, p = b/(a+b), q = b/(a+b), size=1){
	C <- matrix(c(a,-b,a,b,b,b,a,b,b),nrow=3);
	util <- array(0,c(3,3,3));
	util[,,1] <- C ;
	util[,,2] <- (C-t(C))/2 ;
	util[,,3] <- -t(C);

	sum <- 0;
	profit <- c(1:size);

	k<-0
	repeat{
		i <- 2
		j <- 2
		k <- k + 1
		L <- 1
		A <- floor(runif(1,min=-1,max=2))
		B <- floor(runif(1,min=-1,max=2))
		h <- A-B
		if(h<0){
			L <- 3
		}else if(h==0){
			L <- 2
		}

		if(A>0){
			i<-1
		}
		if(B>0){
			j<-1
		}		

		if(rbinom(1,1,p)==1){
			i<-1
		}

		if(rbinom(1,1,p)==1){
			j<-1
		}

		profit[k] <- util[i,j,L]
		if(k == size){
			break
		}
	}
	return(profit)
}
