library(sandwich)
library(lmtest)
library(stargazer)

folder <- "/Users/alisavert/Econometrics_nes/HW2"


# ЗАДАЧА 1

# 1 Четыре отраслевых индикатора в сумме равны единице.
# База — utility.

# Проверяю значимость коэффициентов из условия
b <- c(const = 4.305, log_sales = 0.257, roe = 0.0112,
       indus = 0.283, finance = 0.441, consprod = 0.464)
se <- c(0.282, 0.032, 0.0043, 0.099, 0.104, 0.109)

t1 <- b / se
p1 <- 2 * pt(-abs(t1), df = 209 - length(b))
print(data.frame(Оценка = b, Ст.ошибка = se, t = t1, p = p1))

# При росте продаж на 1% зарплата выше примерно на 0,257%.
# Перевожу коэффициенты логарифмической модели в проценты.
# Для roe расчет дан на одну единицу шкалы исходной переменной.
sector <- c("indus", "finance", "consprod")
print(100 * expm1(b[c("roe", sector)]))

# Проверяю, положительна ли каждая надбавка к utility
print(pt(t1[sector], df = 203, lower.tail = FALSE))

# Самая большая оценка у consprod.
# Для теста разности consprod и finance нужна ковариация их оценок
# В условии даны только индивидуальные стандартные ошибки

# Рисунок 1. Разница в зарплате относительно utility

beta <- c(0.283, 0.441, 0.464)
err <- c(0.099, 0.104, 0.109)
sector <- c("Промышленность", "Финансы", "Потребительские товары")
y <- 3:1

# интервал коэффициента, затем перевожу обе границы в проценты
crit <- qt(0.975, df = 203)
est <- 100 * expm1(beta)
lo <- 100 * expm1(beta - crit * err)
hi <- 100 * expm1(beta + crit * err)

png(file.path(folder, "1_salary.png"), width = 1600, height = 840, res = 160)
par(mar = c(5, 12, 4, 2))
plot(est, y, type = "n", xlim = c(-5, 110), ylim = c(0.6, 3.5),
     yaxt = "n", ylab = "", xlab = "Разница с utility, %",
     main = "Зарплата управляющего по сравнению с utility", bty = "n")
axis(2, at = y, labels = sector, las = 1, tick = FALSE)
abline(v = 0, lty = 2, col = "grey55")
segments(lo, y, hi, y, col = "#267681", lwd = 3)
points(est, y, pch = 19, col = "#164D57", cex = 1.4)
text(est, y, sprintf("+%.1f%%", est), pos = 3)
mtext("Отрезки показывают отдельные 95%-е доверительные интервалы.",
      side = 1, line = 3.6, cex = 0.85)
dev.off()
