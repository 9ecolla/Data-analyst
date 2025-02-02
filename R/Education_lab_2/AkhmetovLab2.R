# Лабораторная работа 2

# Предварительно необходимо открыть файл var21.Rdata (File -> Open file)

# install.packages("ggplot2")
library('ggplot2')


# №1

# По условию предварительно преобразую тип данных для переменных к типу «double»
var21$wage <- as.double(var21$wage) 
var21$age <- as.double(var21$age)


# Проверьтим гипотезы о равенстве соответствующих выборочных коэффициентов корреляции нулю
r1 <- cor(var21$wage, var21$age, method = 'pearson') 
r1
r2 <- cor(var21$wage, var21$hours, method = 'pearson') 
r2

# Расчет коэффициентов корреляции для выборок
# связь r1 слабая, свзязь r2 слабая, тк приближена к 0


#Тестирование коэффициентов корреляции на значимость.

cor.test(var21$wage, var21$age, alternative = 'two.sided', method = 'pearson', conf.level = 0.95);
cor.test(var21$wage, var21$hours, alternative = 'two.sided', method = 'pearson', conf.level = 0.95);

#выводит те же значения, а также: 
# 1) связь слабая и обратная, гипотеза о равенстве нулю отвергается
# 2) связь слабая, гипотеза о равенстве нулю отвергается



# №2

cor.test(var21$wage, var21$age, alternative = 'two.sided', method = 'spearman', conf.level = 0.95);
cor.test(var21$wage, var21$hours, alternative = 'two.sided', method = 'spearman', conf.level = 0.95);

#1) связь слабая и обратная, гипотеза о равенстве к нулю отвергается
#2) связь слабая, гипотеза о равенстве к нулю отвергается.



# №3

cor.test(var21$wage, var21$age, alternative = 'two.sided', method = 'kendall', conf.level = 0.95);
cor.test(var21$wage, var21$hours, alternative = 'two.sided', method = 'kendall', conf.level = 0.95)

#1) связь слабая и обратная, гипотеза о равенстве нулю отвергается
#2) связь слабая, гипотеза о равенстве нулю не отвергается.



# №4

forlm <- var21[sample(nrow(var21), size = 200, replace=FALSE),] 

# replace – замена
# sample – образец
# nrow в R возвращает количество строк, присутствующих во фрейме данных или матрице.

library('ggplot2')
(qplot(forlm$hours, forlm$wage, xlab = 'Продолжительность раб. недели', 
      ylab='Среднемесячная зп', 
      main = 'Зависимость ЗП от продолжительности раб.недели'))



# №5
# Оценим теперь по выборке forlm линейную модель зависимости заработной платы от продолжительности рабочей недели.

model = lm(wage ~ hours, data = forlm)
summary (model)

# Residuals – остаточное количество
# Estimate – оценка
# Significant codes – значимые коды
# Multiple R-squared - кратный r-квадрат

# Отметим, что значимым является только свободный член (Intercept), переменная hours является незначимой на уровне значимости 5% (0.1638).
# Оцененное уравнение также не является значимым (p-value равно 0.002644)
