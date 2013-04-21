prob <- function(a1,a2,a3,a4){
    return((a4-a3)/(a1-a2+a4-a3));
}

probgame <- function(x,d,t,a){
    a1 <- x**d+t+a;
    a2 <- a;
    a3 <- -a;
    a4 <- x**d+a;
    a1 <- a1/(2*x-1);
    a4 <- a4/(2*x-1);
    return(prob(a1,a2,a3,a4));
}
