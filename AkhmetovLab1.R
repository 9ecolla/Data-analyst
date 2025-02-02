# Лабораторная работа 1 Ахметов Руслан 1ФКц


setwd('C:/Users/9ecol/OneDrive/Рабочий стол/Akhmetov')

# №1

load('C:/Users/9ecol/OneDrive/Рабочий стол/Akhmetov/var3.RData')

# install.packages('moments')                 
library('moments') 
# install.packages('boot')
library('boot')

# №2

# выборочное среднее
mean_var3 <- mean(var3$wage)

# выборочная дисперсия
var_var3 <- var(var3$wage , na.rm = FALSE)

# среднеквадратичное отклонение (СКО)
sd_var3 <- sd(var3$wage , na.rm = FALSE)

# медиана
median_var3 <- median(var3$wage, na.rm = FALSE)

# квартили выборочного распредления
quantile_var3 <- quantile(var3$wage, probs = c(0,1,0,1))
              
# экцесс
kurtosis_var3 <- kurtosis(var3$wage, na.rm = T)
              
# скошенность
skewness_var3 <- skewness(var3$wage, na.rm = T)


# №3
# гистограмма относительных частот.
hist(var3$wage , col = 'blue' , xlab = 'Среднемесячная заработная плата',
ylab = 'Относительная частота', main = 'Гистограмма')


# №4

# проверка на основе qq-графика
qqnorm(var3$wage, pch=1, frame=FALSE, main='Q-Q график',
xlab='Квантили теор. распределения',
ylab='Квантили эмпир. распределения');
qqline(var3$wage, col = "red", lwd = 3)
## отклонения сильные, особенно в правом хвосте, следовательно, соответствия нормальному распределению нет

# проверка на основе эксцессы и скошенности
jarque_test_var3 <- jarque.test(var3$wage)
jarque_test_var3
## т.к. значение p меньше 0,05, следовательно, набор данных имеет скошенность и эксцесс, которые не соответствуют нормальному распределению

# проверка с помощью формального теста Колмогорова-Смирнова
ks_test_var3 <- ks.test(var3$wage, 'pnorm', mean(var3$wage), sd(var3$wage))
ks_test_var3
## т.к. значение p слишком мало, следовательно, гипотеза о нормальности опровергается

# проверка с помощью Шапиро-Уилки
shapiro_test_var3 <- shapiro.test(var3$wage)
shapiro_test_var3
## значение p очень малое, следовательно, гипотеза о нормальности опровергается 

#№5

mu.hat <- mean(var10$wage);
S <- sd(var10$wage);
n <- 2000
alpha <- 0.05
#Нахождение доверительного интервала при неизветсной дисперсии 
I.mu.sigma.hat <- c(left=qnorm(alpha/2, mu.hat, S/sqrt(n)),
                    right=qnorm(1-alpha/2, mu.hat, S/sqrt(n)))
round(rbind(
  'Доверительный интервал при неизвестной дисперсии:' =I.mu.sigma.hat), 2)
