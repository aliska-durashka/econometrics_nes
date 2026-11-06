# 1. Скармливаю проге изначальные файлы и библы
library(sandwich)
library(lmtest)
library(stargazer)

villa <- read.csv("~/Downloads/villa.csv")
str(villa)


# 2. Считаю базовую статистику
stats_table <- function(villa, vars, file) {
    d <- villa[vars]
    tab <- data.frame(
        Среднее = sapply(d, mean),
        Ст.откл = sapply(d, sd),
        Минимум = sapply(d, min),
        Медиана = sapply(d, median),
        Максимум = sapply(d, max)
    )
    tab <- round(tab, 2)
    write.csv(tab, file)
    tab 
}

stats_table(villa, c("price", "house"), "stats_price_house.csv")

# 3. Логарифмы ыы

villa$log_price <- log(villa$price)
villa$log_house <- log(villa$house)

stats_table(villa, c("price", "house", "log_price", "log_house"), "villa_stats_all.csv")

# 4. Диаграммы
save_plots <- function(data, file) {
    png(file, width = 1000, height = 800)
    par(mfrow = c(2, 2))
    plot(data$house, data$price, pch = 16, xlab = "Площадь дома, кв. м", ylab = "Цена, тыс. долл.")
    plot(data$house, data$log_price, pch = 16, xlab = "Площадь дома, кв. м", ylab = "log цены")
    plot(data$log_house, data$price, pch = 16, xlab = "log площади дома",    ylab = "Цена, тыс. долл.")
    plot(data$log_house, data$log_price, pch = 16, xlab = "log площади дома",    ylab = "log цены")
    par(mfrow = c(1, 1))
    dev.off()
}

save_plots(villa, "villa_plots.png")

# Зависимость цены от площади (обычные и робастные ошибки)

m1 <- lm(price ~ house, data = villa)
summary(m1)
coeftest(m1,vcov =vcovHC(m1, type = "HC0"))

# Остальные модели

m2 <- lm(log_price ~ house, data = villa)
coeftest(m2, vcov = vcovHC(m2, type = "HC0"))
m3 <- lm(price     ~ log_house, data = villa)
coeftest(m3, vcov = vcovHC(m3, type = "HC0"))
m4 <- lm(log_price ~ log_house, data = villa)
coeftest(m4, vcov = vcovHC(m4, type = "HC0"))

# Таблица регрессий

se1 <- sqrt(diag(vcovHC(m1, type = "HC0")))
se2 <- sqrt(diag(vcovHC(m2, type = "HC0")))
se3 <- sqrt(diag(vcovHC(m3, type = "HC0")))
se4 <- sqrt(diag(vcovHC(m4, type = "HC0")))

save_reg_table <- function(models, ses, file) {
    out <- capture.output(
        stargazer(models, se = ses, type = "html",
                  dep.var.labels   = c("цена", "log цены"),
                  covariate.labels = c("площадь дома", "log площади дома", "константа"),
                  omit.stat = c("adj.rsq", "ser", "f"),
                  notes = "Робастные стандартные ошибки приведены в скобках.",
                  digits = 3)
    )
    out <- gsub(">-", ">&minus;", out)   # настоящий минус вместо дефиса
    writeLines(out, file)
}

save_reg_table(list(m1, m3, m2, m4), list(se1, se3, se2, se4), "villa_reg.html")
stargazer(m1, m3, m2, m4, se = list(se1, se3, se2, se4), type = "text")

# красивая диаграмма с доверительной полосой
grid <- data.frame(log_house = seq(min(villa$log_house), max(villa$log_house), length.out = 100))

X   <- model.matrix(~ log_house, data = grid)
V   <- vcovHC(m4, type = "HC0")                  
fit <- as.vector(X %*% coef(m4))
se  <- sqrt(rowSums((X %*% V) * X))
z   <- qnorm(0.975)
lwr <- fit - z * se
upr <- fit + z * se

png("villa_best_model.png", width = 800, height = 600)
plot(villa$log_house, villa$log_price, pch = 16, xlab = "log площади дома", ylab = "log цены")
polygon(c(grid$log_house, rev(grid$log_house)), c(lwr, rev(upr)), col = "lightblue", border = NA)
points(villa$log_house, villa$log_price, pch = 16)
lines(grid$log_house, fit, col = "red", lwd = 2)
legend("topleft", legend = c("Наблюдения", "Линия регрессии", "95% доверительная область"), pch = c(16, NA, 15), lty = c(NA, 1, NA), lwd = c(NA, 2, NA), col = c("black", "red", "lightblue"), pt.cex = c(1, 1, 2), bty = "n")
dev.off()