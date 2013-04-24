# 混合戦略のMinMax解
prob <- function(a1,a2,a3,a4){
    return((a4-a3)/(a1-a2+a4-a3));
}

# x: 勝率
# a: バンクロールに対するアンティの率
# d: 勝率に対する感度
# t: バンクロールに対するブラフ時の追加投資額の率
# i: 投資関数f(x,d)=x**dに対する補正
probgame <- function(x,d,t,a,i){
    a1 <- (i*(x**d)+t+a)*(2*x-1);
    a2 <- a;
    a3 <- -a;
    a4 <- (i*(x**d)+a)*(2*x-1);
    return(prob(a1,a2,a3,a4));
}
