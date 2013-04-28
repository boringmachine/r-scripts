
cardNums <- function(A){
	B <- c(1:length(A));
	i <- 1;
	for(x in A){
		B[i] <- x%%13+1;
		i <- i + 1;
	}
	return(B);
}

cardMarks <- function(A){
	B <- c(1:length(A));
	i <- 1;
	for(x in A){
		B[i] <- as.integer(x/13)+1;
		i <- i+1;
	}
	return(B);
}

mysort <- function(A){
	i<-1
	j<-1
	s<-c(1:length(A))
	s[i]=A[j]
	i <- i+1
	j <- j+1
	while(j<length(A)+1){
		k <- 1
		while(k <i){
			if(s[k]==A[j]){
				break
			}
			k <- k+1
		}
		if(k== i){
			s[i] <- A[j]
			i <- i+1
		}
		j <- j+1
	}
	return(sort(s[1:i-1]))
}

checkStraight <- function(A){
	B <- cardNums(A);
	B <- mysort(B);
	if(length(B)<5)return(0);
	if(B[1]==1){
		B[length(B)+1]<-14;
	}
	i <- length(B);
	while(i>4){
		if(B[i-4]+4==A[i])return(B[i]);
		i <- i-1;
	}
	return(0);
}

checkFlush <- function(A){
	i <- 1;
	B <- cardMarks(A);
	while(i < 5){
		if(sum(B==i) > 4)return(i);
		i <- i+1;
	}
	return(0)
}

checkOther <- function(A){
	A <- cardNums(A);
	B <- c(1:13);
	B <- B*0;
	i <- 1;
	for(x in A){
		B[x] <- B[x]+1;
	}
	return(B);
}

