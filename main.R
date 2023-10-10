# Подключние библиотек для работы с временными рядами
library('tseries')
library('forecast')

# Загрузка таблицы и переименование столбцов
data <- read.table('Lab2.csv', header = TRUE, sep = ';')[c(1, 2, 3, 4)]
colnames(data) <- c('year', 'month', 't', 'y')

# Выбираем неообходимый столбец
tsData <- data[, 4]
# Преобразуем во временной ряд
tsData <- ts(tsData, frequency = 12, start = c(2007, 1))
# Построение графика временного ряда
plot.ts(tsData, type = 'l', xaxt = 'n', yaxt = 'n')
axis(side = 1, at = seq(2007, 2023, 2))
axis(side = 2, at = seq(0, 200, 10), las = 1)

# Раскладываем временной ряд на основные компоненты
tsDataComponents <- decompose(tsData)
plot(tsDataComponents)

# По графику временного ряда можно понять, что модель нелинейная
myfit1 <- lm(y ~ t, data)
myfit1
plot(data$y, type = 'l')
lines(fitted(myfit1), col = 'red')
r1 <- residuals(myfit1)
plot(r1, type = 'l')

# Подгоняем нелинейную модель
myfit2 <- nls(formula = y ~ (k + a * b^t), data = data, start = c(k = 184.853, a = 214.455, b = 0.965))
myfit2
plot(data$y, type = 'l')
lines(fitted(myfit2), col = 'red')
r2 <- residuals(myfit2)
plot(r2, type = 'l')

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
  return(c(n = n, sd = SD, Min = Min, Max = Max, ME = ME, MAE = MAE, MPE = MPE, MAPE = MAPE, RMSE = RMSE, DC1 = DC1, DC2 = DC2))
}

# Вычисляем характеристики остатков
mypr(r2, data$y)

# Вычисляем значения периодаграммы и строим график
sp <- spec.pgram(tsData, detrend = FALSE, log = 'no', fast = FALSE, pad = FALSE, taper = 0, plot = TRUE)
# Вычисляем и строим график автокорреляционной функции
tsAcf <- Acf(tsData, lag.max = 15, plot = TRUE, na.action = na.contiguous, demean = TRUE)
# Вычисляем и строим график частотной автокорреляционной функции
tsPacf <- pacf(tsData, lag.max = 15, plot = TRUE)