# 混合戦略のMinMax解
prob <- function(a1,a2,a3,a4){
    return((a4-a3)/(a1-a2+a4-a3));
}

# x: 勝率
# a: ポット
# d: 不確実性に対する感度
# t: バンクロールに対するブラフ時の投資額の率
probgame <- function(x,d,t,a){
    a1 <- x**d+t+a;
    a2 <- a;
    a3 <- -a;
    a4 <- x**d+a;
    a1 <- a1/(2*x-1);
    a4 <- a4/(2*x-1);
    return(prob(a1,a2,a3,a4));
}
