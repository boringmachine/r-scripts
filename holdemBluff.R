# 混合戦略のMinMax解
prob <- function(a1,a2,a3,a4){
    return((a4-a3)/(a1-a2+a4-a3));
}


# if win y = 1 else y = 0
# X = w*u*y + l*u*(1-y)
# E(X) = w*u*x + l*u*(1-x)
#
# w = (n-1), l = -1, u = v/b, n = 2
#
# define: 
#   u := i*(x^d)
#   bluff is betting u+t
#
# if bluff X' = (u+t+a)*(2*x-1)*j + a*(1-j)
#   if enemy  is fold j=0 else j=1
# else X'' = (u+a)*(2*x-1)*k - a*(1-k)
#   if player is fold k=0 else k=1
#   
# objective: P(j=0) = P(k=0)
# conditions:
#   0 <= u <= 1
#   0 <= x <= 1
#   0 <= a <= 1
#   0 <= t <= 1
#   0 <= u+a <= 1
#
# y: 勝敗の0-1変数
# j: 敵のフォルドの0-1変数
# k: 自分のフォルドの0-1変数
# w: 利益倍率
# l: 損益倍率
# X: ゲームの定義
# E(X): ゲームの期待効用
# n: プレイヤー人数
# b: バンクロール
# v: 投資額
# u: バンクロールに対する投資額の率
# x: 勝率
# a: バンクロールに対するアンティの率
# d: 勝率に対する賭金の依存度
# t: バンクロールに対するブラフ時の追加投資額の率
# i: 投資率x**dに対する補正
probgame <- function(x,d,t,a,i){
    a1 <- (i*(x**d)+t+a)*(2*x-1);
    a2 <- a;
    a3 <- -a;
    a4 <- (i*(x**d)+a)*(2*x-1);
    return(prob(a1,a2,a3,a4));
}
