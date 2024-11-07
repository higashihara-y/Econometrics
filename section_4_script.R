library(tidyverse, readr, readxl)
library(readxl)
library(sandwich)

# 前処理
exc2 <- read_excel("data for chap 4 exercise 2.xlsx")
exc2 <- rename(exc2, "2013_pop" = "2013人口", "ln(2013_pop)" = "ln(2013人口）")

# 均一分散のパッケージ算出
model1 <- lm(exc2$`ln(2013GDP)` ~ exc2$`ln(2013_pop)`)
summary(model1)

# 不均一分散のパッケージ算出
sqrt(vcovHC(model1, type = "HC"))


# 4-2-1
b1_nom <- sum(
  (exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`)) * 
  (exc2$`ln(2013GDP)` - mean(exc2$`ln(2013GDP)`))
  )

b1_denom <- sum(
  (exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`))^2
)

b1 <- b1_nom / bi_denom
b0 <- mean(exc2$`ln(2013GDP)`) - b1 * mean(exc2$`ln(2013_pop)`)
b1
b0


# 4-2-2
# 不均一分散の手動算出
v1_num <- (1 / (nrow(exc2) - 2)) * 
  (sum(
    (exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`))^2
    * (resid(model1)^2)
  ))

v1_denom <- ((1 / nrow(exc2)) *
  (sum(
    (exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`))^2
    )))^2

v1 <- v1_num / v1_denom
se_b1 <- sqrt(v1 / nrow(exc2))
se_b1

# 均一分散の手動算出
v1_2_num <- (1 / (nrow(exc2) - 2)) *
  sum(resid(model1)^2)

v1_2_denom <- (1 / nrow(exc2)) *
  sum((exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`))^2)

v1_2 <- v1_2_num / v1_2_denom
se_b1_2 <- sqrt(v1_2 / nrow(exc2))
se_b1_2

t <- (b1 - 1) / se_b1_2
t

# 4-2-3
confint(model1, '(Intercept)', 0.90)
# b1 - se_b1_2 * qt(0.95, nrow(exc2)) 
# b1 + se_b1_2 * qt(0.95, nrow(exc2)) 



# 4-2-4
# 人口が1%増加すると、GDPは1.08%増加する。
# 95％の信頼水準で、人口の1%増加は、GDPを1%以上増加させる。

# 4-2-5
var_u <- var(resid(model1))
var_lnpop <- var(exc2$`ln(2013_pop)`)


# 4-3
# x,y共にlog化しない場合、xの単位を100倍すると、b0は変わらず、biは1/100になる
# x,y共にlog化する場合、xの単位を100倍すると、b0,b1ともに変わらない
# x,y共にlog化しない場合、yの単位を100倍すると、b0,biともに100倍になる
# x,y共にlog化する場合、yの単位を100倍すると、b0はlog(100)～約4.60大きくなり,b1は変わらない


# 4-5
# 定数項なしの場合のb1の推定公式を使用
b1_5 <- sum(exc2$`ln(2013_pop)` * exc2$`ln(2013GDP)`) / 
  sum(exc2$`ln(2013_pop)`^2)
b1_5


# 4-6
# 均一分散の前提で分散、t値を算出
v1_6_num <- (1 / (nrow(exc2) - 2)) *
  sum((exc2$`ln(2013GDP)` - b1_5 * exc2$`ln(2013_pop)`)^2)

v1_6_denom <- (1 / nrow(exc2)) *
  sum((exc2$`ln(2013_pop)` - mean(exc2$`ln(2013_pop)`))^2)

v1_6 <- v1_6_num / v1_6_denom
se_b1_6 <- sqrt(v1_6 / nrow(exc2))
se_b1_6

t2 <- (b1_5 - 1) / se_b1_6
t2


# 4-9
model2 <- lm(exc2$`ln(2013_pop)` ~ exc2$`ln(2013GDP)`)
summary(model2)
a1 <- 1 / model1$coefficients[2]
a1
a0 <- - (model1$coefficients[1] / model1$coefficients[2])
a0


# 4-10
data410 <- read_excel("data for chap 4 exercise 10.xlsx")

# 4-10-1
model410 <- lm(Y ~ X, data = data410)
summary(model410)
cov(model410$residuals, data410$X)
cov(resid(model410), data410$X)

# u1 <- data410$Y - 1 - data410$X
# cov(u1, data410$X)
# sum((u1 - mean(u1)) * (data410$X - mean(data410$X))) / (nrow(data410) - 1)

# 4-10-2
mean(resid(model410)^2)
plot(data410$X, resid(model410))
abline(h = 0)

model410 |> 
  ggplot(aes(x = X, y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "blue", lty = "dashed")


# 4-10-3
pacman::p_load(estimatr)
model410_rb <- estimatr::lm_robust(Y ~ X, data = data410)
summary(model410_rb)
confint(model410_rb, 'X', level = 0.95)

model410_rb$coefficients[2] - 
  model410_rb$std.error[2] * qt(0.975, nrow(data410) - 2)
model410_rb$coefficients[2] + 
  model410_rb$std.error[2] * qt(0.975, nrow(data410) - 2)

# 4-10-4
summary(model410)
confint(model410, "X", level = 0.95)

# 4-10-5
# 分散が均一ではないデータに均一分散の標準誤差を適用すると、標準誤差が大きく算出され、推定の効率性が低下する可能性がある。



