library(sandwich)

# 1. Загружаю данные и оцениваю модель
folder <- "/Users/alisavert/Econometrics_nes/HW2"
if (!dir.exists(folder)) folder <- getwd()

gpa <- read.table(file.path(folder, "GPA2.txt"), header = TRUE)
m1 <- lm(sat ~ hsize + hsizesq + female * black, data = gpa)

# 2. Считаю прогноз SAT для группы female = 0, black = 0
h <- seq(min(gpa$hsize), max(gpa$hsize), length.out = 300)
grid <- data.frame(hsize = h, hsizesq = h^2, female = 0, black = 0)
fit <- predict(m1, newdata = grid)

# 3. Считаю границы 95%-й доверительной полосы
X <- model.matrix(delete.response(terms(m1)), grid)
V <- vcovHC(m1, type = "HC0")
margin <- qnorm(0.975) * sqrt(rowSums((X %*% V) * X))
lo <- fit - margin
hi <- fit + margin

# 4. Нахожу вершину параболы и перевожу сотни человек в число выпускников
x <- 100 * h
b <- coef(m1)
h0 <- -b[["hsize"]] / (2 * b[["hsizesq"]])
y0 <- b[["(Intercept)"]] + b[["hsize"]] * h0 + b[["hsizesq"]] * h0^2

# 5. Рисую и сохраняю график
save_sat <- function(path) {
    png(path, width = 1400, height = 900, res = 150)
    on.exit(dev.off())

    plot(x, fit, type = "n", ylim = range(lo, hi),
         xlab = "Число выпускников", ylab = "Средний SAT, баллы",
         main = "Размер выпуска и прогноз среднего SAT")

    polygon(c(x, rev(x)), c(lo, rev(hi)), col = "lightblue", border = NA)
    lines(x, fit, col = "navy", lwd = 3)
    points(100 * h0, y0, pch = 19, col = "red")
    text(100 * h0, y0, sprintf("%.0f чел.", 100 * h0), pos = 3)
    legend("bottomleft", "95% ДИ среднего SAT (HC0)", fill = "lightblue", bty = "n")
}

out <- file.path(folder, "2_sat_final.png")
save_sat(out)
message("График сохранён: ", out)