## 
## ベイズ統計学によるマーケティング分析
##

##--- セットアップ ---##

rm(list = ls())
set.seed(123) # 再現性のためのシード設定

##--- ワーキングディレクトリ ---##

getwd()

##--- データの読み込み ---##

demog <- read.csv("demog.csv", header = TRUE, row.names = 1)
sales_visit <- read.csv("sales_visit.csv", header = TRUE, row.names = 1)

##--- インポート ---##

library(MASS)
library(bayesm)
library(LaplacesDemon)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(psych)
library(reshape2)

##--- 基本統計量とプロット ---##

describe(sales_visit$sales_num)
describe(sales_visit$visit_num)
describe(demog$exp10)
describe(demog$avg_samp)


p1 <- ggplot(sales_visit, aes(x = sales_num)) +
  geom_histogram() +
  labs(title = "売上個数", x = "Sales", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(sales_visit, aes(x = visit_num)) +
  geom_histogram() +
  labs(title = "訪問回数", x = "Visit", y = "Frequency") +
  theme_minimal()

p3 <- ggplot(demog, aes(x = as.factor(exp10))) +
  geom_bar() +
  labs(title = "10年経験ダミー", x = "Exp10", y = "Count") +
  theme_minimal()

p4 <- ggplot(demog, aes(x = avg_samp)) +
  geom_histogram() +
  labs(title = "無料サンプル平均個数", x = "Avg_samp", y = "Frequency") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

# idごと
id_means <- sales_visit %>%
  group_by(id) %>%
  summarise(
    mean_sales_num = mean(sales_num, na.rm = TRUE),
    mean_visit_num = mean(visit_num, na.rm = TRUE)
  )

describe(id_means$mean_sales_num)
describe(id_means$mean_visit_num)

p1 <- ggplot(id_means, aes(x = mean_sales_num)) +
  geom_histogram() +
  labs(title = "IDごとの平均売上個数", x = "Mean Sales", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(id_means, aes(x = mean_visit_num)) +
  geom_histogram() +
  labs(title = "IDごとの訪問回数", x = "Mean Visit", y = "Frequency") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

##--- 準備 ---##

s <-1000
t <-23
p_Z <- 2
p_X <- 2

nvar <- 3 #Xの変数の数

Z <- cbind(c(rep(1,s)), demog[, c("exp10", "avg_samp")])
Z_int <- as.matrix(Z)
nz = ncol(Z)

iota = c(rep(1,t))
regdata = NULL

X_s <- X_s_int <- y_s <- NULL

temp = cbind(iota, sales_visit)

unique_ids <- unique(temp$id)

for(id_val in unique_ids) {
  
  subset_data <- temp[temp$id == id_val, ]
  y <- as.matrix(subset_data$sales_num)
  X <- as.matrix(subset_data[, c("iota", "visit_num", "lagged_sales_num")])
  regdata[[id_val]] = list(y=y, X=X) 
  X_s_int[[id_val]] <- X
  y_s[[id_val]] <- y
  
}

#初期値

V_beta = matrix(1, nrow = nvar, ncol = nvar)
Theta = matrix(1, nrow = p_Z+1, ncol = nvar)
B <- matrix(1, nrow = s, ncol = nvar)
sigma_s <- matrix(1, nrow=s)

# priors
# Theta ~ N(theta0,Theta0)
theta0 <- matrix(0, nrow=p_Z+1, ncol=p_X+1)
Theta0 <- diag(0.01, p_Z+1) 

nu0 <- nvar+3
V0 <- diag(nu0, p_X+1)

nu0i <- 3
s0i <- 50

b0 <- matrix(0, nrow=p_X+1)
B0 <- diag(10000, p_X+1) 

# storage for MCMC draw
Theta_MCMC <- NULL
V_beta_MCMC <- NULL
B_MCMC <- NULL
sigma_s_MCMC <- NULL

## Start MCMC ##
n_MCMC <- 5000 # number of MCMC chain
n_bin <- 1000 #number of burn-in period

for (mc in 1:n_MCMC){
  
  # 1. sampling Theta
  Theta_mean <- as.vector( ( solve( t(Z_int)%*%Z_int+Theta0 ) ) %*% ( t(Z_int)%*%B+Theta0%*%theta0) )
  Theta_cov <- kronecker(V_beta, solve( t(Z_int)%*%Z_int+Theta0 ) ) 
  Theta_vec <- mvrnorm(1, Theta_mean, Theta_cov)
  Theta <- matrix(Theta_vec, nrow=p_Z+1)
  
  # 2. sampling V_beta
  V_beta <- rinvwishart(nu0+s, V0+t(B-Z_int%*%Theta) %*% (B-Z_int%*%Theta))
  V_beta_vec <- as.vector(V_beta)
  
  # 3. sampling B
  B_ite <- NULL
  for (ss in 1:s) { 
    B_s_mean <- solve( t(X_s_int[[ss]])%*%X_s_int[[ss]]+solve(V_beta) ) %*% ( t(X_s_int[[ss]])%*%y_s[[ss]]+t(Theta)%*%Z_int[ss,])
    B_s_cov <- sigma_s[ss]*solve(t(X_s_int[[ss]])%*%X_s_int[[ss]]+solve(V_beta))
    B_s <- mvrnorm(1, B_s_mean, B_s_cov)
    B_ite <- rbind(B_ite,t(B_s))
  }
  B <- B_ite
  
  # 4. sampling sigma_s
  sigma_s_ite <- NULL
  for (ss in 1:s) {
    sigma_s_s <- rinvgamma(1, nu0i+t, s0i+t( y_s[[ss]]-X_s_int[[ss]]%*%(ginv( t(X_s_int[[ss]])%*%X_s_int[[ss]] )%*%t(X_s_int[[ss]])%*%y_s[[ss]]))  %*%  ( y_s[[ss]]-X_s_int[[ss]]%*%(ginv(t(X_s_int[[ss]])%*%X_s_int[[ss]])%*%t(X_s_int[[ss]])%*%y_s[[ss]])) ) 
    sigma_s_ite <- rbind(sigma_s_ite,sigma_s_s)
  }
  sigma_s <- sigma_s_ite
  
  Theta_MCMC <- rbind(Theta_MCMC, c(Theta_vec))
  V_beta_MCMC <- rbind(V_beta_MCMC, c(V_beta))
  B_MCMC <- rbind(B_MCMC, t(c(B)) )
  sigma_s_MCMC <- rbind(sigma_s_MCMC, t(c(sigma_s)) )
  
  print(mc)
}

##--- Theta ---##

#トレースプロット

plots <- lapply(1:ncol(Theta_MCMC), function(i) {
  ggplot(data.frame(index = 1:nrow(Theta_MCMC), value = Theta_MCMC[,i]), aes(x = index, y = value)) +
    geom_line(color = "black") +
    theme_minimal()
})

plots_transposed <- list(
  plots[[1]], plots[[4]], plots[[7]],
  plots[[2]], plots[[5]], plots[[8]],
  plots[[3]], plots[[6]], plots[[9]]
)

# グリッドにプロットを配置
do.call(grid.arrange, c(plots_transposed, ncol = 3))

#事後平均・標準偏差、95％信用区間
poseterior.mean <- apply(Theta_MCMC[(n_bin+1):n_MCMC,], 2, mean)  # posterior mean of betas
poseterior.sd <- apply(Theta_MCMC[(n_bin+1):n_MCMC,], 2, sd) # posterior s.d. of betas
CI.lower <- apply(Theta_MCMC[(n_bin+1):n_MCMC,], 2, quantile, probs=0.025)
CI.upper <- apply(Theta_MCMC[(n_bin+1):n_MCMC,], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

##--- B ---##

#トレースプロット
beta1 <- B_MCMC[, 1:1000]
beta2 <- B_MCMC[, 1001:2000]
beta3 <- B_MCMC[, 2001:3000]

select_random_columns <- function(df, seed = 123) {
  # 再現性のためのシード設定
  set.seed(seed)
  
  # データフレームの列数を取得
  num_cols <- ncol(df)
  
  # ランダムに5列を選択
  selected_cols <- sample(num_cols, 5)
  
  return(selected_cols)
}

plot_data <- function(data, selected_cols, title) {
  
  data_ <- data[, selected_cols]
  
  # データを長い形式に変換
  data_long <- melt(data_, varnames = c("index", "variable"), value.name = "value")
  data_long$index <- rep(1:nrow(data_), times = ncol(data_))
  
  # 折れ線グラフの作成
  p <- ggplot(data_long, aes(x = index, y = value, color = as.factor(variable))) +
    geom_line() +
    labs(title = paste(title, "\nColumns:", paste(selected_cols, collapse = ", ")), 
         x = "Index", y = "Value", color = "Column") +
    theme_minimal()
  
  return(p)
}

# beta1, beta2, beta3のプロットを作成
beta1_p <- plot_data(beta1, select_random_columns(beta1), "beta1 Random 5 Columns")
beta2_p <- plot_data(beta2, select_random_columns(beta2), "beta2 Random 5 Columns")
beta3_p <- plot_data(beta3, select_random_columns(beta3), "beta3 Random 5 Columns")

# 1×3のレイアウトでプロットを配置
grid.arrange(beta1_p, beta2_p, beta3_p, ncol = 1)

#事後平均・標準偏差、95％信用区間
poseterior.mean <- apply(beta1[(n_bin+1):n_MCMC,select_random_columns(beta1)], 2, mean)  
poseterior.sd <- apply(beta1[(n_bin+1):n_MCMC,select_random_columns(beta1)], 2, sd)
CI.lower <- apply(beta1[(n_bin+1):n_MCMC,select_random_columns(beta1)], 2, quantile, probs=0.025)
CI.upper <- apply(beta1[(n_bin+1):n_MCMC,select_random_columns(beta1)], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

poseterior.mean <- apply(beta2[(n_bin+1):n_MCMC,select_random_columns(beta2)], 2, mean)  
poseterior.sd <- apply(beta2[(n_bin+1):n_MCMC,select_random_columns(beta2)], 2, sd) 
CI.lower <- apply(beta2[(n_bin+1):n_MCMC,select_random_columns(beta2)], 2, quantile, probs=0.025)
CI.upper <- apply(beta2[(n_bin+1):n_MCMC,select_random_columns(beta2)], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

poseterior.mean <- apply(beta3[(n_bin+1):n_MCMC,select_random_columns(beta3)], 2, mean) 
poseterior.sd <- apply(beta3[(n_bin+1):n_MCMC,select_random_columns(beta3)], 2, sd) 
CI.lower <- apply(beta3[(n_bin+1):n_MCMC,select_random_columns(beta3)], 2, quantile, probs=0.025)
CI.upper <- apply(beta3[(n_bin+1):n_MCMC,select_random_columns(beta3)], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

#IDごとの平均を出してヒストグラムにしてみる

plot_column_means <- function(data, title) {
  # 列ごとの平均を計算
  column_means <- colMeans(data)
  
  data_ <- data.frame(mean_value = column_means)
  
  # ヒストグラムの作成
  p <- ggplot(data_, aes(x = mean_value)) +
    geom_histogram() +
    labs(title = title, x = "Mean Value", y = "Frequency") +
    theme_minimal()
  
  return(p)
}

# beta1, beta2, beta3のプロットを作成
beta1_avg_p <- plot_column_means(beta1, "beta1 ID Means")
beta2_avg_p <- plot_column_means(beta2, "beta2 ID Means")
beta3_avg_p <- plot_column_means(beta3, "beta3 ID Means")

# 1×3のレイアウトでプロットを配置
grid.arrange(beta1_avg_p, beta2_avg_p, beta3_avg_p, ncol = 3)

##--- V_beta ---##

#トレースプロット

plots <- lapply(1:ncol(V_beta_MCMC), function(i) {
  ggplot(data.frame(index = 1:nrow(V_beta_MCMC), value = V_beta_MCMC[,i]), aes(x = index, y = value)) +
    geom_line(color = "black") +
    theme_minimal()
})

plots_transposed <- list(
  plots[[1]], plots[[4]], plots[[7]],
  plots[[2]], plots[[5]], plots[[8]],
  plots[[3]], plots[[6]], plots[[9]]
)

# グリッドにプロットを配置
do.call(grid.arrange, c(plots_transposed, ncol = 3))

#事後平均・標準偏差、95％信用区間
poseterior.mean <- apply(V_beta_MCMC[(n_bin+1):n_MCMC,], 2, mean)  
poseterior.sd <- apply(V_beta_MCMC[(n_bin+1):n_MCMC,], 2, sd) 
CI.lower <- apply(V_beta_MCMC[(n_bin+1):n_MCMC,], 2, quantile, probs=0.025)
CI.upper <- apply(V_beta_MCMC[(n_bin+1):n_MCMC,], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

##--- sigma_s ---##

plot_data(sigma_s_MCMC, select_random_columns(sigma_s_MCMC), "sigma_s_MCMC Random 5 Columns")

#事後平均・標準偏差、95％信用区間
poseterior.mean <- apply(sigma_s_MCMC[(n_bin+1):n_MCMC,select_random_columns(sigma_s_MCMC)], 2, mean)  
poseterior.sd <- apply(sigma_s_MCMC[(n_bin+1):n_MCMC,select_random_columns(sigma_s_MCMC)], 2, sd) 
CI.lower <- apply(sigma_s_MCMC[(n_bin+1):n_MCMC,select_random_columns(sigma_s_MCMC)], 2, quantile, probs=0.025)
CI.upper <- apply(sigma_s_MCMC[(n_bin+1):n_MCMC,select_random_columns(sigma_s_MCMC)], 2, quantile, probs=0.975)
print(res_table <- as.matrix(t(rbind(poseterior.mean, poseterior.sd, CI.lower, CI.upper))) )

plot_column_means(sigma_s_MCMC, "sigma_s ID Means")
