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
# 均一分散の手動算出
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

# 不均一分散の手動算出
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
b1 - se_b1_2 * qt(0.95, nrow(exc2)) 
b1 + se_b1_2 * qt(0.95, nrow(exc2)) 

# 4-2-4
# 人口が1%増加すると、GDPは1.08%増加する。
# 95％の信頼水準で、人口の1%増加は、GDPを1%以上増加させる。

# 4-2-5
var_u <- var(resid(model1))
var_lnpop <- var(exc2$`ln(2013_pop)`)


