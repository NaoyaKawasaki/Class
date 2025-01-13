#packageをロード
library(tidyverse)
library(e1071)

#データのimport
KUZUHA_20053_20234 <- read_csv("./Keihan-Main line_Kuzuha_20053_20234.csv",
                              locale = locale(encoding = "SHIFT-JIS"))

#最初の10行表示
head(KUZUHA_20053_20234, n = 10)

#変数名を変換
colnames(KUZUHA_20053_20234) <-
  c("Type", "PriceType", "CityCode", "Pref", "City", "District", "Station", "Dist_Sta", "Price", "Rooms", "FloorSpace", "Bld_Year", "Bld_Strc", "C_Use", "F_Use", "LandUse", "BCR", "FAR", "Trans_Year", "Remodeling", "Trans_Circum")

#最初の10行表示
head(KUZUHA_20053_20234, n = 10)

#IDの付与 (行をそのまま割り当ててIDとして扱う場合)
KUZUHA_20053_20234$ID <- as.numeric(rownames(KUZUHA_20053_20234))

#付与したIDと住所関連変数のみを取り出す
KUZUHA_20053_20234_r <- as.data.frame(cbind(KUZUHA_20053_20234$ID,KUZUHA_20053_20234$Pref,KUZUHA_20053_20234$City,KUZUHA_20053_20234$District))

#そのデータをexport (これにCSISのアドレスマッチングを用いて緯度経度の情報をつける
#write_csv(KUZUHA_20053_20234_r, "KUZUHA_xy_Pre.csv")

#座標データのimport 
KUZUHA_20053_20234_xy <- read_csv("./KUZUHA_xy_Post.csv", show_col_types = FALSE, locale = locale(encoding = "Shift-JIS"))
head(KUZUHA_20053_20234_xy)

#データの結合 (同じIDをもつマンションに座標を繋げる)
KUZUHA_xy <- merge(KUZUHA_20053_20234, KUZUHA_20053_20234_xy, by.x="ID", by.y="V1")
head(KUZUHA_xy, n = 20)
na_counts <- sapply(KUZUHA_xy, function(x) sum(is.na(x)))
print(na_counts)

#データのexport (現在のworking directoryに保存)
#write_csv(KUZUHA_xy, "KUZUHA_xy.csv")

#対象データの抽出 (dplyr必須)
#df1 <- KUZUHA_xy %>% filter(PriceType == "成約価格情報")

# --- データを代入 ---#

df <- KUZUHA_xy

# ---Priceの処理 ---#

#売買価格のヒストグラム
ggplot(df, aes(x=Price)) + geom_histogram(fill="#108A99") + theme_classic()
#売買価格の箱ひげ図
ggplot(df, aes(x="",y=Price)) + geom_boxplot() + theme_classic()
#売買価格の基礎統計量
summary(df$Price) #最小値・最大値・平均・中央値
sd(df$Price) #標準偏差

#0.01と0.99の場合のみ表示
#quantile(df1$Price, probs=c(0.01, 0.99))

skewness(df$Price, type=2)
kurtosis(df$Price, type=2)

#自然対数の場合
df$Ln_Price <- log(df$Price) #価格の変換 (自然対数)
summary(df$Ln_Price)
ggplot(df, aes(x=Ln_Price)) + geom_histogram(fill="#108A99") + theme_classic()
ggplot(df, aes(x="",y=Ln_Price)) + geom_boxplot() + theme_classic()

summary(df$Ln_Price)
sd(df$Ln_Price)
skewness(df$Ln_Price, type=2)
kurtosis(df$Ln_Price, type=2)

#外れ値の処理

df <- df[!(df$Ln_Price < 14),]

# --- Dist_Staの処理 --- #

df_difficult <- df %>% filter(is.na(Dist_Sta) | Dist_Sta == "30分～60分")

df_difficult$LocName1 <- paste(df_difficult$Pref, df_difficult$City, df_difficult$District,  sep = "")
df$LocName1 <- paste(df$Pref, df$City, df$District,  sep = "")

Loc_Unique <- unique(df_difficult$LocName1)

#print(Loc_Unique)

Town_Dist_Sta<-c("57", "33", "46", "35", "45", "28", "53", "58", "21", "53")# Value
names(Town_Dist_Sta)<-c("京都府八幡市八幡", "京都府八幡市男山", "大阪府枚方市高野道", "大阪府枚方市東山",    
                        "大阪府枚方市招提中町", "大阪府枚方市南船橋", "大阪府枚方市長尾峠町", "大阪府枚方市北山",   
                        "大阪府枚方市楠葉美咲", "大阪府枚方市招提東町"
                        )
#print(Town_Dist_Sta["京都府八幡市男山"])

#df[1, 9] <- Town_Dist_Sta[df[1, 31]]
#print(df[1, 9])

for (i in 1:nrow(df)){
temp <- df[i, "Dist_Sta"]
if(is.na(temp) | temp == "30分～60分"){
  df[i, "Dist_Sta"] <- Town_Dist_Sta[df[i, "LocName1"]]
  }
else{

  }
}

df <- df[!(df$LocName1=="大阪府枚方市東中振"),]

summary(df$Dist_Sta) #文字列の場合、平均等は出力されない
df$Dist_Sta_r <- as.numeric(df$Dist_Sta) #文字列の数字への変換

#--- Roomsの処理 ---#
#欠損値の処理
df <- df[!(is.na(df$Rooms)),]

table(df$Rooms) #多様なカテゴリ変数

library(stringi) #全角半角返還パッケージ

#全角を半角に返還
df <- df %>% mutate(Room = stri_trans_general(Rooms, "Fullwidth-Halfwidth"))

#ダミー変数としての部屋数

df <- df %>% mutate(Room_1 = if_else(
  Rooms=="１ＤＫ"|Rooms=="１ＤＫ＋Ｓ"|Rooms=="１Ｋ"|Rooms=="１Ｋ＋Ｓ"|
    Rooms=="１ＬＤＫ"|Rooms=="１ＬＤＫ＋Ｓ"|Rooms=="１ＬＫ"|Rooms=="１ＬＫ＋Ｓ"|Rooms=="１Ｒ"
  ,1,0)) #部屋数=1
df <- df %>% mutate(Room_2 = if_else(
  Rooms=="２ＤＫ"|Rooms=="２ＤＫ＋Ｓ"|Rooms=="２Ｋ"|Rooms=="２Ｋ＋Ｓ"|
    Rooms=="２ＬＤＫ"|Rooms=="２ＬＤＫ＋Ｓ"|Rooms=="２ＬＫ"|Rooms=="２ＬＫ＋Ｓ"
  ,1,0)) #部屋数=2
df <- df %>% mutate(Room_3 = if_else(
  Rooms=="３ＤＫ"|Rooms=="３ＤＫ＋Ｓ"|Rooms=="３Ｋ"|Rooms=="３ＬＤＫ"|
    Rooms=="３ＬＤＫ＋Ｓ"|Rooms=="３ＬＫ"
  ,1,0)) #部屋数=3
df <- df %>% mutate(Room_Ov4 = if_else(
  Rooms=="４ＤＫ"|Rooms=="４Ｋ"|Rooms=="４ＬＤＫ"|Rooms=="４ＬＤＫ＋Ｓ"|
    Rooms=="５ＤＫ"|Rooms=="５ＬＤＫ"|Rooms=="５ＬＤＫ＋Ｓ"|Rooms=="６ＤＫ＋Ｓ"|
    Rooms=="６Ｋ"|Rooms=="６ＬＤＫ＋Ｓ"
  ,1,0)) #部屋数=4以上

#カテゴリ変数としての部屋数
#後で (1)ダミー変数として、(2)カテゴリ変数としてモデルに投入する時の違いを確認

#部屋数を4カテゴリーに分類
df$Rooms_r <- ifelse(df$Room_1==1,1,ifelse(df$Room_2==1,2,ifelse(df$Room_3==1,3,ifelse(df$Room_Ov4==1,4,999))))

#部屋数を3カテゴリーに分類
df$Rooms_r2 <- ifelse(df$Room_1==1,1,ifelse(df$Room_2==1,2,ifelse(df$Room_3==1|df$Room_Ov4==1,3,999)))

df <- df[!(df$Rooms_r2 == 999),]

#--- LandUseの処理 ---#

#欠損値の処理
df <- df[!(is.na(df$LandUse)),]

df <- df %>% mutate(lu_ex_low_house = if_else(
  LandUse=="１低住専"|LandUse=="２低住専",1,0
)) #低層住居専用地域ダミー
df <- df %>% mutate(lu_ex_high_house = if_else(
  LandUse=="１中住専"|LandUse=="２中住専",1,0
)) #中高層住居専用地域ダミー
df <- df %>% mutate(lu_house = if_else(
  LandUse=="１種住居"|LandUse=="２種住居"|LandUse=="準住居",1,0
)) #住居地域ダミー
df <- df %>% mutate(lu_commercial = if_else(
  LandUse=="商業"|LandUse=="近隣商業",1,0
)) #商業地域ダミー
df <- df %>% mutate(lu_industrial = if_else(
  LandUse=="工業"|LandUse=="工業専用"|LandUse=="準工業",1,0
)) #工業地域ダミー

# --- Floor Spaceの処理 ---#

#専有面積の確認

#専有面積のヒストグラム
ggplot(df, aes(x=FloorSpace)) + geom_histogram(fill="#108A99", binwidth = 10) + theme_classic()
#専有面積の箱ひげ図
ggplot(df, aes(x="",y=FloorSpace)) + geom_boxplot() + theme_classic()
#専有面積の基礎統計量
summary(df$FloorSpace) #最小値・最大値・平均・中央値
sd(df$FloorSpace) #標準偏差

df$Ln_FloorSpace <- log(df$FloorSpace) #面積の変換 (自然対数)

#専有面積のヒストグラム
ggplot(df, aes(x=Ln_FloorSpace)) + geom_histogram(fill="#108A99") + theme_classic()
#専有面積の箱ひげ図
ggplot(df, aes(x="",y=Ln_FloorSpace)) + geom_boxplot() + theme_classic()
#専有面積の基礎統計量
summary(df$Ln_FloorSpace) #最小値・最大値・平均・中央値
sd(df$Ln_FloorSpace) #標準偏差

#--- Bld_Yearの処理---#
#欠損値の処理
df <- df[!(is.na(df$Bld_Year)),]

#〇〇年として収録されているため、"年"を全て削除して数値化
df$Bld_Year_r <- as.numeric(str_replace_all(df$Bld_Year, "年", ""))
summary(df$Bld_Year_r)
table(df$Bld_Year_r)

#全角を半角に返還
df <- df %>% mutate(Trans_Year = stri_trans_general(Trans_Year, "Fullwidth-Halfwidth"))

#2024年建築を処理
df$Bld_Year_r2 <- if_else(df$Bld_Year_r>=2023,2023,df$Bld_Year_r)
table(df$Bld_Year_r2)

# --- Trans_Yearの処理 ---#
#取引年 (文字列のa番目からb番目までを取得して数値化)
df$Trans_Year_r <- as.numeric(str_sub(df$Trans_Year, start=1, end=4))

#取引四半期
df$Trans_Quarter <- as.numeric(str_sub(df$Trans_Year, start=7, end=7))
df$Trans_Quarter_r <- paste("Q", str_sub(df$Trans_Year, start=7, end=7))

#取引年+四半期
df$Trans_YQ <- paste(as.character(df$Trans_Year_r), df$Trans_Quarter_r)

#Age

df$Age <- df$Trans_Year_r - df$Bld_Year_r2
df$Age_r <- if_else(df$Age<0,0,df$Age)

# ---ダミー変数をfactor(binary)に変更--- #
df$Rooms_r <- factor(df$Rooms_r)
df$Room_1 <- factor(df$Room_1)
df$Room_2 <- factor(df$Room_2)
df$Room_3 <- factor(df$Room_3)
df$Room_Ov4 <- factor(df$Room_Ov4)

df$lu_ex_low_house <- factor(df$lu_ex_low_house)
df$lu_ex_high_house <- factor(df$lu_ex_high_house)
df$lu_house <- factor(df$lu_house)
df$lu_industrial <- factor(df$lu_industrial)

df$CityCode <- factor(df$CityCode)

# --- データの特徴を捉える ---#

# --- 全体の特徴を捉える ---#

#データの概要を確認
#install.packages("skimr")
library(skimr)
info <- skim(df)
view(info)

# --- 価格(対数)の特徴を捉える ---#
summary(df$Ln_Price)
sd(df$Ln_Price)
ggplot(df, aes(x=Ln_Price)) + geom_histogram(fill="#108A99") + theme_classic()
ggplot(df, aes(x="",y=Ln_Price)) + geom_boxplot() + theme_classic()
skewness(df$Ln_Price, type=2)
kurtosis(df$Ln_Price, type=2)

# --- 最寄り駅までの距離の特徴を捉える ---#

summary(df$Dist_Sta_r)  
sd(df$Dist_Sta_r)
ggplot(df, aes(x=Dist_Sta_r)) + geom_histogram(fill="#108A99") + theme_classic()
ggplot(df, aes(x="",y=Dist_Sta_r)) + geom_boxplot() + theme_classic()

# --- 間取りの特徴を捉える ---#

#全角として文字が入っている際
ggplot(df, aes(x=reorder(Rooms, Rooms, function(x)-length(x)))) + geom_bar(fill='red') + labs(x='Rooms')

#半角に直したもの
ggplot(df, aes(x=reorder(Room, Room, function(x)-length(x)))) + geom_bar(fill='red') + labs(x='Room')

#部屋数を4カテゴリーに再分類した時の分布
ggplot(df, aes(x=reorder(Rooms_r, Rooms_r, function(x)-length(x)))) + geom_bar(fill='#108A99') + labs(x='Rooms')

#部屋数を3カテゴリーに再分類した時の分布
ggplot(df, aes(x=reorder(Rooms_r2, Rooms_r2, function(x)-length(x)))) + geom_bar(fill='red') + labs(x='Rooms')

#所在地の土地利用別に物件数を確認
ggplot(df, aes(x=reorder(LandUse, LandUse, function(x)-length(x)))) + geom_bar(fill='red') + labs(x='Land Use')

# --- 専有面積の特徴を捉える ---#

#専有面積のサマリーとヒストグラム
summary(df$Ln_FloorSpace)
sd(df$Ln_FloorSpace)
ggplot(df, aes(x=Ln_FloorSpace)) + geom_histogram(fill="#108A99") + theme_classic()
ggplot(df, aes(x="",y=Ln_FloorSpace)) + geom_boxplot() + theme_classic()

# --- 築年数の特徴を捉える ---#

summary(df$Age)
sd(df$Ln_FloorSpace)
ggplot(df, aes(x=Age)) + geom_histogram(fill="#108A99") + theme_classic()
ggplot(df, aes(x="",y=Age)) + geom_boxplot() + theme_classic()

# --- 変数間の関係を捉える ---#

#部屋数別に価格の箱ひげ図を作成
ggplot(df, aes(x=factor(Rooms_r),y=Ln_Price)) + geom_boxplot(fill="#108A99") + theme_classic()

#部屋数別に専有面積の箱ひげ図を作成
ggplot(df, aes(x=factor(Rooms_r),y=Ln_FloorSpace)) + geom_boxplot(fill="#108A99") + theme_classic()

#部屋数別の最寄り駅までの距離の箱ひげ図を作成
ggplot(df, aes(x=factor(Rooms_r),y=Dist_Sta_r)) + geom_boxplot(fill="#108A99") + theme_classic()

#部屋数別に築年数の箱ひげ図を作成
ggplot(df, aes(x=factor(Rooms_r),y=Age_r)) + geom_boxplot(fill="#108A99") + theme_classic()

#install.packages("cowplot")
library(cowplot)

plot_grid(
  ggplot(df, aes(x=Age, y=Ln_Price)) + geom_point() + theme_classic(),
  ggplot(df, aes(x=Ln_FloorSpace, y=Ln_Price)) + geom_point() + theme_classic(),
  ggplot(df, aes(x=Age, y=Ln_FloorSpace)) + geom_point() + theme_classic(),
  nrow=1, ncol=3
)

#変数間の相関を確認
cor.test(df$Age, df$Ln_Price)
cor.test(df$Ln_FloorSpace, df$Ln_Price)
cor.test(df$Dist_Sta_r, df$Ln_Price)

#売買価格(自然対数)、最寄り駅までの距離、専有面積(自然対数)、築年数
df_cor <- na.omit(df[,c("Ln_Price","Dist_Sta_r","Ln_FloorSpace","Age")]) #多分崩れる
round(cor(df_cor),3)

#散布図行列を作成 (変数が多い際には推奨しない)
pairs(df_cor)

#専有面積と売買価格の散布図 + 単回帰の結果を一緒に表示
ggplot(df, aes(x=Ln_FloorSpace, y=Ln_Price)) + geom_point() + theme_classic() + geom_smooth(method=lm, se=FALSE)

#上記の散布図に部屋数(カテゴリー)別に色分け
ggplot(df, aes(x=Ln_FloorSpace, y=Ln_Price, color=factor(Rooms_r2))) + geom_point() + theme_classic() + geom_smooth(method=lm, se=FALSE)

# --- 仮説の検証 --- #
model <- lm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
              Room_2 + Room_3 + Room_Ov4 +
              lu_ex_low_house + lu_ex_high_house + lu_house + lu_industrial +
              Trans_YQ
            , data=df)
summary(model)

#install.packages("car")  
library(car)

vif(model)

df_f <- df %>% dplyr::select(ID, Ln_Price, Age_r, Ln_FloorSpace, Dist_Sta_r,
                          Room_2, Room_3, Room_Ov4,
                          lu_ex_low_house, lu_ex_high_house, lu_house, lu_industrial,
                          Trans_YQ, City, fX, fY)

#Listwise
#install.packages("TestDataImputation")
library(TestDataImputation)

df_f_complete <- Listwise(df_f, Mvalue="NA")
df_f_complete$row_num1 <- as.numeric(row.names(df_f_complete))

#Stepwise (AICを基準に変数選択)
#install.packages("MASS")
library(MASS)

model_null <- lm(Ln_Price ~ 1, data=df_f_complete)
model_full <- lm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
                   Room_2 + Room_3 + Room_Ov4 +
                   lu_ex_low_house + lu_ex_high_house + lu_house + lu_industrial +
                   Trans_YQ
                 , data=df_f_complete)

model_both1 <- stepAIC(model_full, direction="both")
summary(model_both1)
vif(model_both1)

model_best <- lm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
                  Room_2 + Room_3 + Room_Ov4 +
                  lu_industrial + Trans_YQ
                  , data = df_f_complete)
summary(model_best)

#分析結果を表に
#install.packages("modelsummary")
#install.packages("tidyverse")
library(modelsummary)

model_list <- list()
model_list[['Model 1']] <-lm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
                              Room_2 + Room_3 + Room_Ov4 +
                              lu_industrial + Trans_YQ
                              , data = df_f_complete)
msummary(model_list, stars= c("*" = .1, "**" = .05, "***" = .01) , gof_omit='RMSE|AIC|BIC|Log.Lik.')


# ---予測--- #

df_p <- df %>% dplyr::select(Ln_Price, Age_r, Ln_FloorSpace, Dist_Sta_r,
                             Room_2, Room_3, Room_Ov4, lu_industrial, Trans_YQ)

#データの分割
#install.packages("caret")
library(dplyr)
library(caret)

set.seed(123)  # 再現性のためにシードを設定

#データを分割する関数を設定
split_data <- function(data, train_ratio = 0.8) {
  train_index <- createDataPartition(data$Trans_YQ, p = train_ratio, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  return(list(train = train_data, test = test_data))
}

# `Trans_YQ`ごとにデータを分割
train_data_list <- list()
test_data_list <- list()

unique_YQ <- unique(df_p$Trans_YQ)
for (YQ in unique_YQ) {
  subset_data <- df_p %>% filter(Trans_YQ == YQ)
  split_result <- split_data(subset_data)
  train_data_list[[YQ]] <- split_result$train
  test_data_list[[YQ]] <- split_result$test
}

# 訓練データとテストデータをそれぞれ一つのデータフレームに結合
train_data <- bind_rows(train_data_list)
test_data <- bind_rows(test_data_list)

# 結果の確認
#print("Training Data:")
#print(train_data)
#print("Test Data:")
#print(test_data)

#予測モデルの構築
#モデル１：線形回帰モデル
model_LRM <- lm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
              Room_2 + Room_3 + Room_Ov4 +
              lu_industrial + Trans_YQ
            , data = train_data)

prediction_LRM <- predict(model_LRM, test_data)
mse_LRM <- mean((test_data$Ln_Price - prediction_LRM)^2)
print(paste("Mean Squared Error:", mse_LRM))

#モデル２：ランダムフォレスト
#install.packages("randomForest")
library(randomForest)

model_rF <- randomForest(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
                           Room_2 + Room_3 + Room_Ov4 +
                           lu_industrial + Trans_YQ
                         , data = train_data)

prediction_rF <- predict(model_rF, test_data)
mse_rF <- mean((test_data$Ln_Price - prediction_rF)^2)
print(paste("Mean Squared Error:", mse_rF))

#モデル３：サポートベクトル回帰
#install.packages("e1071")
library(e1071)
model_SVR <- svm(Ln_Price ~ Age_r + Ln_FloorSpace + Dist_Sta_r +
                   Room_2 + Room_3 + Room_Ov4 +
                   lu_industrial + Trans_YQ
                 , data = train_data, type = "eps-regression")

prediction_SVR <- predict(model_SVR, test_data)
mse_SVR <- mean((test_data$Ln_Price - prediction_SVR)^2)
print(paste("Mean Squared Error:", mse_SVR))
