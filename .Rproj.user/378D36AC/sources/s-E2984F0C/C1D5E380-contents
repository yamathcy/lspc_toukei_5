# ケース：野菜をクラスタリングする
par(family="Hiragino Mincho Pro W6")
# データ読み込み
ysi <- read.csv("yasai_eiyou.csv", row.names = 1)

# 中身を確認
head(ysi)

# 階層クラスタ分析
# 距離をユークリッド距離で算出
D0 <- dist(ysi, method="euclidian")
D <- (1/2)*D0*2

# 確認
D

# クラスタリングする
ysi.out <- hclust(d=D, method="ward.D")

# デンドログラムを描画
plot(as.dendrogram(ysi.out), xlim=c(30000, 0), xlab="非類似度", horiz =T)

# クラスタの特徴を把握
# 3クラスタで分けてみる
(cluster <- factor(cutree(ysi.out, k=3)))

# クラスタごとに栄養の平均をみてみる
by(ysi, INDICES = cluster, FUN=function(x){apply(x, 2, mean)})


# 参考：Z得点による分析
# 変数により分散が異なる場合，標準化してから分析するのがよい
# scale関数により標準化
ysi.stdz <- scale(ysi)
D0.stdz <- dist(ysi.stdz, method = "euclidian")
D.stdz <- (1/2) * D0.stdz^2
ysi.stdz.out <- hclust(d=D.stdz, method = "ward.D")
plot(as.dendrogram(ysi.stdz.out), xlim=c(120, 0), xlab="非類似度", horiz =T)

# 非階層クラスタ分析を行うならPythonの方が良さげなので省きました