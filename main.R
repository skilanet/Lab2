# Подключние библиотек для работы с временными рядами
library('tseries')
library('forecast')

# Загрузка таблицы и переименование столбцов
data <- read.table('Lab2.csv', header = TRUE, sep = ';')[c(3, 4)]
colnames(data) <- c('t', 'y')

# Выбираем неообходимый столбец
tsData <- data[, 2]
# Преобразуем во временной ряд
tsData <- ts(tsData, frequency = 12, start = c(2007, 1))
# Построение графика временного ряда
plot.ts(tsData, type = 'l', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(2007, 2023, 2))
axis(side = 2, at = seq(0, 200, 10), las = 1)

# Раскладываем временной ряд на основные компоненты
tsDataComponents <- decompose(tsData)
plot(tsDataComponents)

# Функция для расчёта характеристик остатков
mypr <- function(x, y) {
  ME <- mean(x)
  n <- length(x)
  SD <- sd(x)
  Min <- min(x)
  Max <- max(x)
  MAE <- mean(abs(x))
  MPE <- mean(100 * x / y)
  MAPE <- mean(abs(100 * x / y))
  RMSE <- sqrt(mean(x * x))
  DC1 <- 1 - sum(x^2) / sum((y - mean(y))^2)
  DC2 <- sum((y - x - mean(y))^2) / sum((y - mean(y))^2)
  return(data.frame(n = n, sd = SD, Min = Min, Max = Max, ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, RMSE = RMSE, DC1 = DC1, DC2 = DC2))
}

# Подгоняем нелинейную модель
myfit1 <- nls(formula = y ~ k / (1 + a * exp(b * t)), data = data, start = c(k = 170, a = 0.1, b = 0.1))
myfit1
r1 <- residuals(myfit1)
plot(data$y, type = 'l')
lines(data$y - r1, col = 'red', lwd = 2)
sp1 <- spec.pgram(r1, detrend = FALSE, log = 'no', fast = FALSE, pad = FALSE, taper = 0, plot = TRUE)
ac <- Acf(tsData, lag.max = 15, plot = TRUE, na.action = na.contiguous, demean = TRUE)
Pacf(tsData, lag.max = 36, plot = TRUE)
options(digits = 5)
View(mypr(r1, data$y))

r2 <- residuals(myfit1)
ts2 <- ts(r2, frequency = 12, start = c(2007, 1))
plot.ts(ts2)
sp2 <- spec.pgram(ts2, detrend = FALSE, log = 'no', fast = FALSE, pad = FALSE, taper = 0, plot = TRUE)
plot(sp)
Acf(ts2, lag.max = 36)
Pacf(ts2, lag.max = 36)
m2 <- nls(y ~ k / (1 + a * exp(b * t)) +
  a1 * cos(2 * pi * t / 12) +
  b1 * sin(2 * pi * t / 12) +
  a2 * cos(6 * pi * t / 12) +
  b2 * sin(6 * pi * t / 12), data, start = c(k = 180, a = 15, b = -0.1, a1 = 0.1, b1 = 0.1, a2 = 0.1, b2 = 0.1))
m2
r3 <- residuals(m2)
plot(data$y, type = 'l')
lines(data$y - r3, col = 'red', lwd = 2)
View(mypr(r3, data$y))

ts3 <- ts(r3, frequency = 12, start = c(2007, 1))
plot.ts(ts3)
sp3 <- spec.pgram(ts3, detrend = FALSE, log = 'no', fast = FALSE, pad = FALSE, taper = 0, plot = TRUE)
Acf(ts3, lag.max = 15, plot = TRUE, na.action = na.contiguous, demean = TRUE)
Pacf(ts3, lag.max = 15, plot = TRUE)




