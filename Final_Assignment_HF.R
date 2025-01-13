##
##  高頻度資産価格データ分析
##
##  Version: 2024/4/28
##

##--- セットアップ ---##

rm(list=ls())

##--- get working directory ---##

getwd()

##--- read file ---##

SPdata <- read.csv("SP.csv",header=T,skip=0) 
EPUdata <- read.csv("US_Daily_EPU.csv",header=T,skip=0)

##--- S&P500データの加工 ---##

library(dplyr)

# Timeを文字列として読み込む
SPdata$Time <- as.character(SPdata$Time)

# DateをDate型に変換
SPdata$Date <- as.Date(SPdata$Date, format = "%Y/%m/%d")

#08:35以前と15:00以降を削除
SPdata <- SPdata[SPdata$Time >= "08:35" & SPdata$Time <= "15:00", ]

# 5分ごとのRVを計算
df_RV5min <- SPdata %>%
  group_by(Date) %>%
  mutate(Return_squared = ifelse(Time == "08:35", 
                                 (100*(log(Open) - log(lag(Close))))^2 + (100*(log(Close) - log(Open)))^2, 
                                 (100*(log(Close) - log(lag(Close))))^2
                                 )
         ) %>%
  summarise(RV_5min = sum(Return_squared, na.rm = TRUE))

# 30分ごとのRVを計算
df_RV30min <- SPdata %>%
  group_by(Date) %>%
  mutate(Return_squared = case_when(
    Time == "08:35" ~ (100*(log(Open) - log(lag(Close))))^2 + (100*(log(Close) - log(Open)))^2,
    Time == "09:00" ~ (100 * (log(Close) - log(lag(Close, 5))))^2,
    Time >= "09:30" & Time <= "15:00" & row_number() %% 6 == 0 ~ (100 * (log(Close) - log(lag(Close, 6))))^2,  # Assuming 30 minutes intervals
    TRUE ~ NA_real_
  )) %>%
  summarise(RV_30min = sum(Return_squared, na.rm = TRUE))

df_daily <- df_RV5min %>%
  left_join(df_RV30min, by = "Date")

# 各日の最後のTimeのCloseの対数値を計算

df_Return <- SPdata %>%
  group_by(Date) %>%
  filter(Time == max(Time)) %>%
  summarise(
    Price = last(Close),
    Price_log = log(last(Close))
  )

# 日次のリターンを計算
df_daily <- df_daily %>%
  left_join(df_Return, by = "Date") %>%
  mutate(Return = 100*(Price_log - lag(Price_log)))

df_daily <- df_daily %>%
  select(Date, Price, Return, RV_5min, RV_30min)
# 1行目を削除
df_daily <- df_daily[-1, ]

##--- EPU指数データの加工 ---##

library(lubridate)

# 日付をdate型に変換
EPUdata <- EPUdata %>%
  mutate(Date = make_date(year, month, day)) %>%
  rename(EPU = daily_policy_index)


##--- データの結合 ---##
df_daily_selected <- df_daily %>%
  select(Date, Price, Return, RV_5min, RV_30min)
EPUdata_selected <- EPUdata %>%
  select(Date, EPU)

df <- df_daily_selected %>%
  left_join(EPUdata_selected, by = "Date")

##--- 各種処理 ---##

# 対数変換列の追加

df$log_RV_5min <- log(df$RV_5min)
df$log_EPU <- log(df$EPU)

#1日変化していない行を削除
df <- df %>% filter(log_RV_5min != -Inf)

library(zoo)

# RV_5minの移動平均を計算して新しい列RV_5min_weeklyに追加
df <- df %>%
  mutate(RV_5min_weekly = rollmean(RV_5min, k = 5, fill = NA, align = "right"))

# RV_5minの移動平均を計算して新しい列RV_5min_monthlyに追加
df <- df %>%
  mutate(RV_5min_monthly = rollmean(RV_5min, k = 22, fill = NA, align = "right"))

# EPUの移動平均を計算して新しい列EPU_weeklyに追加
df <- df %>%
  mutate(EPU_weekly = rollmean(EPU, k = 5, fill = NA, align = "right"))

# EPUの移動平均を計算して新しい列RV_5min_monthlyに追加
df <- df %>%
  mutate(EPU_monthly = rollmean(EPU, k = 22, fill = NA, align = "right"))


#NAを削除

library(tidyr)

df <- df %>%
  drop_na()

##--- プロット ---##

day <- df$Date 
price <- df$Price
return <- df$Return
RV_5min <- df$RV_5min
RV_30min <- df$RV_30min
log_RV_5min <- df$log_RV_5min
RV_5min_weekly <- df$RV_5min_weekly
RV_5min_monthly <- df$RV_5min_monthly
ar <- abs(return)
dar <- ifelse(return<0,1,0)*ar
EPU <- df$EPU
log_EPU <- df$log_EPU
EPU_weekly <- df$EPU_weekly
EPU_monthly <- df$EPU_monthly
n <- length(day)

dev.new() 
par(mfrow = c(2, 2)) 
plot(day, price,  
     main='S&P500 closing price',
     xlab='day', ylab='S&P500',type='l',lty=1,col='red') 
plot(day, return, 
     main='S&P500 return',
     xlab='day', ylab='return(%)',type='l',lty=1,col='red')
plot(day, RV_5min,  
     main='S&P500 RV' ,
     xlab='day', ylab='RV',type='l',lty=1,col='red') 
plot(day, EPU,  
     main='EPU Index',
     xlab='day', ylab='EPU_Index',type='l',lty=1,col='red') 
 

##--- 要約統計量の計算 ---##

library(psych)

df_ <- df %>%
  select(Return, RV_5min, log_RV_5min, EPU, log_EPU)

describe(df_)

##--- コレログラムをかく ---## 

dev.new() 
par(mfrow = c(2, 2))
acf(RV_5min, main='Correlogram for S&P500 RV')
acf(log_RV_5min, main='Correlogram for S&P500 RV（log）')
acf(EPU, main='Correlogram for EPU Index')
acf(log_EPU, main='Correlogram for EPU Index（log）')

##--- JB統計量 ---##

for (col in names(df_)) {
  
  m <- mean(df_[[col]]) #リターンの平均
  sd <- sd(df_[[col]]) #標準偏差
  se_m <- sd/sqrt(n) #標準誤差
  sk <- mean((df_[[col]]-m)^3/sd^3) #歪度
  se_sk <- sqrt(6/n) #歪度の標準誤差
  ku <- mean((df_[[col]]-m)^4/sd^4) #尖度
  se_ku <- sqrt(24/n) #尖度の標準誤差
  JB <- (sk/se_sk)^2+((ku-3)/se_ku)^2 #Jarque-Bera統計量
  cat(sprintf("JB statistics for %s: %.3f", col, JB),"\n")
}  

##--- LB統計量 ---##

lag <- 10
for (col in names(df_)) {
  ac <- sd <- ac <- ac2 <- LB <- DLB <- 0
  for (i in 1:lag)
  {
    mean <- mean(df_[[col]])
    sd <- sd(df_[[col]])
    ac<- cor(df_[[col]][1:(n-i)], df_[[col]][(i+1):n])
    ac2<- cor((df_[[col]][1:(n-i)]-mean)^2, (df_[[col]][(i+1):n]-mean)^2)
    LB <- LB+n*(n+2)*(ac^2)/(n-i) #Ljung-Box統計量
    DLB <- DLB+n*(n+2)*(sd^4)*(ac^2)/((sd^4+ac2)*(n-i))
  }
  cat(sprintf("LB statistics for %s: %.3f", col, LB),"\n")
  cat(sprintf("Heteroskedasticity adjusted LB statistics for %s: %.3f", col, DLB),"\n")
}  

cat("chisq distribution","\n") #自由度10
chi_var<- qchisq(0.10, lag, lower.tail=FALSE) #左から閾値、自由度、上側を指定
cat("10%:", sprintf('%.3f', chi_var),"\n")
chi_var<- qchisq(0.05, lag, lower.tail=FALSE)
cat("5%:", sprintf('%.3f', chi_var),"\n")
chi_var<- qchisq(0.01, lag, lower.tail=FALSE)
cat("1%:", sprintf('%.3f', chi_var),"\n\n")

Box.test(return, lag =10, type = "Ljung-Box")

c_5min <-(sum((return-mean(return))^2))/sum(RV_5min) 
c_30min <-(sum((return-mean(return))^2))/sum(RV_30min) 
cat('\nThe Hansen and Lunde adjustment coefficient\n') 
print(c_5min)
print(c_30min)

##--- モデルの推定 ---##

library(lmtest)
library(sandwich)

result1 <- lm(log(RV_5min[2:n])~log(RV_5min[1:n-1])+log(RV_5min_weekly[1:n-1]) #1期ずらして回帰
                 +log(RV_5min_monthly[1:n-1])+ar[1:n-1]+dar[1:n-1])
summary(result1)
print(coeftest(result1, vcov = NeweyWest(result1)))

result2 <- lm(log(RV_5min[2:n])~log(RV_5min[1:n-1])+log(RV_5min_weekly[1:n-1]) #1期ずらして回帰
              +log(RV_5min_monthly[1:n-1])+ar[1:n-1]+dar[1:n-1]+EPU[1:n-1])
summary(result2)
print(coeftest(result2, vcov = NeweyWest(result2)))

result3 <- lm(log(RV_5min[2:n])~log(RV_5min[1:n-1])+log(RV_5min_weekly[1:n-1]) #1期ずらして回帰
              +log(RV_5min_monthly[1:n-1])+ar[1:n-1]+dar[1:n-1]+log(EPU[1:n-1]))
summary(result3)
print(coeftest(result3, vcov = NeweyWest(result3)))

result4 <- lm(log(RV_5min[2:n])~log(RV_5min[1:n-1])+log(RV_5min_weekly[1:n-1]) #1期ずらして回帰
              +log(RV_5min_monthly[1:n-1])+ar[1:n-1]+dar[1:n-1]+log(EPU[1:n-1])+log(EPU_weekly[1:n-1])+log(EPU_monthly[1:n-1]))
summary(result4)
print(coeftest(result4, vcov = NeweyWest(result4)))

##--- モデルの予測精度 ---##

#LHARモデル

forecasts_lhar <- numeric()

for (i in 2:(n-3000)){
  result_lhar <- lm(log(RV_5min[i:(i+2999)])~log(RV_5min[(i-1):(i+2998)])+log(RV_5min_weekly[(i-1):(i+2998)]) #1期ずらして回帰
                   +log(RV_5min_monthly[(i-1):(i+2998)])+ar[(i-1):(i+2998)]+dar[(i-1):(i+2998)])
  s.result <-summary(result_lhar)
  coe <- result_lhar$coefficient #係数  
  v_resid <- s.result$sigma^2 #残差分散
  
  # 一期先予測 
  data <-cbind(1,log(RV_5min[i+2999]),log(RV_5min_weekly[i+2999]),log(RV_5min_monthly[i+2999]),ar[i+2999],dar[i+2999])
  forecast<-exp(sum(coe*data)+.5*v_resid)
  forecasts_lhar <- c(forecasts_lhar, forecast)
}

forecasts24_lhar <- c_5min*forecasts_lhar

#LHAR_lnEPUモデル

forecasts_lhar_lnEPU <- numeric()

for (i in 2:(n-3000)){
  result_lhar_lnEPU <- lm(log(RV_5min[i:(i+2999)])~log(RV_5min[(i-1):(i+2998)])+log(RV_5min_weekly[(i-1):(i+2998)]) #1期ずらして回帰
                   +log(RV_5min_monthly[(i-1):(i+2998)])+ar[(i-1):(i+2998)]+dar[(i-1):(i+2998)]+log(EPU[(i-1):(i+2998)]))
  s.result <-summary(result_lhar_lnEPU)
  coe <- result_lhar_lnEPU$coefficient #係数  
  v_resid <- s.result$sigma^2 #残差分散
  
  # 一期先予測 
  data <-cbind(1,log(RV_5min[i+2999]),log(RV_5min_weekly[i+2999]),log(RV_5min_monthly[i+2999]),ar[i+2999],dar[i+2999],log(EPU[i+2999]))
  forecast<-exp(sum(coe*data)+.5*v_resid)
  forecasts_lhar_lnEPU <- c(forecasts_lhar_lnEPU, forecast)
}

forecasts24_lhar_lnEPU <- c_5min*forecasts_lhar_lnEPU

#LHAR_lnEPU_DWMモデル

forecasts_lhar_lnEPU_DWM <- numeric()

for (i in 2:(n-3000)){
  result_lhar_lnEPU_DWM <- lm(log(RV_5min[i:(i+2999)])~log(RV_5min[(i-1):(i+2998)])+log(RV_5min_weekly[(i-1):(i+2998)]) #1期ずらして回帰
                         +log(RV_5min_monthly[(i-1):(i+2998)])+ar[(i-1):(i+2998)]+dar[(i-1):(i+2998)]+log(EPU[(i-1):(i+2998)])+log(EPU_weekly[(i-1):(i+2998)])+log(EPU_monthly[(i-1):(i+2998)]))
  s.result <-summary(result_lhar_lnEPU_DWM)
  coe <- result_lhar_lnEPU_DWM$coefficient #係数  
  v_resid <- s.result$sigma^2 #残差分散
  
  # 一期先予測 
  data <-cbind(1,log(RV_5min[i+2999]),log(RV_5min_weekly[i+2999]),log(RV_5min_monthly[i+2999]),ar[i+2999],dar[i+2999],log(EPU[i+2999]),log(EPU_weekly[i+2999]),log(EPU_monthly[i+2999]))
  forecast<-exp(sum(coe*data)+.5*v_resid)
  forecasts_lhar_lnEPU_DWM <- c(forecasts_lhar_lnEPU_DWM, forecast)
}

forecasts24_lhar_lnEPU_DWM <- c_5min*forecasts_lhar_lnEPU_DWM

##--- 予測精度の検証 ---##

answer <- RV_30min[3002:6013]
answer24 <- c_30min*answer

mse_lhar <- mean((forecasts24_lhar - answer24)^2)
print(mse_lhar)

mse_lhar_lnEPU <- mean((forecasts24_lhar_lnEPU- answer24)^2)
print(mse_lhar_lnEPU)

mse_lhar_lnEPU_DWM <- mean((forecasts24_lhar_lnEPU_DWM- answer24)^2)
print(mse_lhar_lnEPU_DWM)

qlike_lhar <- mean(log(forecasts24_lhar)+(answer24/forecasts24_lhar))
print(qlike_lhar)

qlike_lhar_EPU <- mean(log(forecasts24_lhar_lnEPU)+(answer24/forecasts24_lhar_lnEPU))
print(qlike_lhar_EPU)

qlike_lhar_EPU_DWM <- mean(log(forecasts24_lhar_lnEPU_DWM)+(answer24/forecasts24_lhar_lnEPU_DWM))
print(qlike_lhar_EPU_DWM)

