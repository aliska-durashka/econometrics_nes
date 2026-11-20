library(sandwich)
library(lmtest)
library(stargazer)

folder <- "/Users/alisavert/Econometrics_nes/HW2"

# ЗАДАЧА 2.

# 2 Загружаю данные
gpa <- read.table(file.path(folder, "GPA2.txt"), header = TRUE)
gpa$female_black <- gpa$female * gpa$black
cat("Загружено наблюдений:", nrow(gpa), "\n")


# описательная статистика
stats_table <- function(data, vars, file) {
    d <- data[vars]
    tab <- data.frame(
        N        = sapply(d, length),
        Среднее  = sapply(d, mean),
        Ст.откл  = sapply(d, sd),
        Минимум  = sapply(d, min),
        Медиана  = sapply(d, median),
        Максимум = sapply(d, max)
    )
    write.csv(tab, file)
    round(tab, 4)
}

vars <- c("sat", "hsize", "hsizesq", "female", "black", "female_black")
print(stats_table(gpa, vars, file.path(folder, "hw2_stats.csv")))


# Основная модель и проверка значимость

m1 <- lm(sat ~ hsize + hsizesq + female * black, data = gpa)
V1 <- vcovHC(m1, type = "HC0")
print(coeftest(m1, vcov. = V1))


# Сравниваю группы при одинаковом размере выпуска
contrast <- function(terms) {
    estimate <- sum(coef(m1)[terms])
    se <- sqrt(sum(V1[terms, terms, drop = FALSE]))
    c(Оценка = estimate, Ст.ошибка = se,
      p = 2 * pnorm(-abs(estimate / se)),
      Нижняя = estimate - qnorm(0.975) * se,
      Верхняя = estimate + qnorm(0.975) * se)
}

effect_table <- rbind(
    "female при black = 0" = contrast("female"),
    "female при black = 1" = contrast(c("female", "female:black")),
    "black при female = 0" = contrast("black"),
    "black при female = 1" = contrast(c("black", "female:black"))
)
print(effect_table, digits = 4)


# Проверяю, нужен ли квадрат размера выпуска
print(coeftest(m1, vcov. = V1)["hsizesq", , drop = FALSE])

# Наклон кривой при размере выпуска h
a  <- unname(coef(m1)["hsize"])
b2 <- unname(coef(m1)["hsizesq"])


# дельта метод

h_max <- -a / (2 * b2)
size_terms <- c("hsize", "hsizesq")
V_size <- V1[size_terms, size_terms]
gradient <- c(-1 / (2 * b2), a / (2 * b2^2))
se_max <- sqrt(drop(t(gradient) %*% V_size %*% gradient))

maximum <- 100 * c(
    Выпускников = h_max,
    Нижняя = h_max - qnorm(0.975) * se_max,
    Верхняя = h_max + qnorm(0.975) * se_max
)
print(round(maximum, 1))


# Сравниваю три модели в одной таблице
m2 <- update(m1, . ~ . + athlete + hsperc)
m3 <- update(m2, . ~ . + white)

save_reg_table <- function(models, file) {
    ses <- lapply(models, function(m) sqrt(diag(vcovHC(m, type = "HC0"))))
    ps <- lapply(models, function(m) {
        coeftest(m, vcov. = vcovHC(m, type = "HC0"))[, 4]
    })
    stargazer(models, se = ses, p = ps, type = "html", out = file,
              dep.var.labels = "SAT", omit.stat = c("ser", "f"),
              notes = "В скобках приведены робастные ошибки HC0.",
              digits = 3)
}

save_reg_table(list(m1, m2, m3), file.path(folder, "hw2_models.html"))

# Рисунок - прогноз среднего SAT в зависимости от размера выпуска

h <- seq(min(gpa$hsize), max(gpa$hsize), length.out = 300)
grid <- data.frame(hsize = h, hsizesq = h^2, female = 0, black = 0)

# Для каждого размера выпуска считаю прогноз и его робастную ошибку
X <- model.matrix(delete.response(terms(m1)), grid)
fit <- drop(X %*% coef(m1))
se_fit <- sqrt(rowSums((X %*% V1) * X))
lwr <- fit - qnorm(0.975) * se_fit
upr <- fit + qnorm(0.975) * se_fit

# Перевожу сотни человек в число выпускников для горизонтальной оси
x <- 100 * h
peak <- -coef(m1)[["hsize"]] / (2 * coef(m1)[["hsizesq"]])
peak_sat <- predict(m1, data.frame(hsize = peak, hsizesq = peak^2,
                                   female = 0, black = 0))

png(file.path(folder, "2_sat.png"), width = 1600, height = 940, res = 160)

par(mar = c(5, 5, 4, 2))

plot(x, fit, type = "n", ylim = range(lwr, upr),
     xlab = "Number of graduates", ylab = "Predicted mean SAT",
     main = "Graduating class size and SAT", bty = "l")

polygon(c(x, rev(x)), c(lwr, rev(upr)),
        col = "lightblue", border = NA)

lines(x, fit, col = "navy", lwd = 3)
abline(v = 100 * peak, col = "grey40", lty = 2)
points(100 * peak, peak_sat, pch = 19, col = "darkred")
text(100 * peak, peak_sat, sprintf("%.0f graduates", 100 * peak), pos = 3)

mtext("Shading: 95% pointwise CI for mean SAT; female = 0, black = 0.",
      side = 1, line = 3.5, cex = 0.85)

dev.off()