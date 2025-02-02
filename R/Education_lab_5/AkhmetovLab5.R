# Лабораторная работа 5

# Перварительно необходимо открыть файл var3.RData и установить пакеты ниже

# install.packages('fitdistrplus')
library('fitdistrplus')
# install.packages('dgof')
library('dgof')


# Задание 1

# Для выборки отбираем только ненулевые (положительные) значения заработной платы
var3.poswage <- var3[var3[,'wage']>0,]$wage

# Результаты оптимизации по методу максимального правдоподобия сохраняем в списке mle.list:
mle.list <- fitdist(var3.poswage, distr = 'lnorm', method = 'mle')

# Точечные оценки для мат.ожидания и стандартного отклонения mle.list и sigma.hat соответственно равны 9.81 и 0.65:
m.hat <- mle.list$estimate[1] 
sigma.hat <- mle.list$estimate[2]

# Задание 2

# Разобьем на 61 интервал и построим гистограмму:
x.arg <- seq(0, 300000, length =61);
h <- hist (var3.poswage, breaks =x.arg, freq=F, col = 'grey90'); 
lines(x.arg, dlnorm(x.arg, m.hat, sigma.hat), lty =1, col = 'red', lwd = 2)

# Задание 3

# Нулевая гипотеза H0 состоит в том, что выборочное распределение соответствует предполагаемому. 
# Альтернативная гипотеза H1 заключается в том, что такого соответствия нет.

ks.test(var3.poswage, plnorm, m.hat, sigma.hat)

# Задание 4

# Поскольку для значений, начиная с 22-го интервала число значений мало, соответствующие частоты подлежат объединению:
ccc <- h$counts
ccc2 <- vector() 
ccc2[1:18] <- ccc[1:18];
ccc2[19] <- sum(ccc[19:60]);
b <- h$breaks
b <- b[1:18]
sum.n <- sum(ccc2)


E <- vector()
for (i in (1:length(b)-1)) {
  E[i] <- round(plnorm(b[i+1], meanlog = m.hat, sdlog = sigma.hat) - plnorm(b[i], meanlog = m.hat, sdlog = sigma.hat), 5)
}

E[length(b)] <- round(1 - plnorm(b[length(b)], meanlog = m.hat, sdlog = sigma.hat), 3)
E.n <- sum.n*E
(df <- cbind(x = b, n = ccc2, E = E.n))
# В результате набор данных df содержит как фактические, так и теоретические частоты

# Рассчитаем значение тестовой статистики Пирсона: 
chi2 <- sum((df[,2]-df[,3])^2/df[,3]); # оно равно 133.98.

# Критическая точка уровня значимости 0.01 равна 39.99:
alpha <- 0.01; 
chi2cr <- qchisq(1-alpha/2, 20)

# Задание 5
t <- table(var3$sex,var3$city)
t
chisq.test(t)
