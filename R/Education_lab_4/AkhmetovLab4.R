# Лабораторная работа 4 Ахметов Руслан 1ФКц


# Задание 1

library('ggplot2')

df <- var3[,c('wage', 'educ')]
df$educ <- factor(df$educ, levels=c('нет', 'Учитесь', 'Учились'))

# Задание 2

#Построим диаграммы «boxplot» и оцените возможное наличие различий в средних визуально.

ggplot(df, aes(x = educ, y = wage)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("educ") +
  ylab("wage")

# Задание 3

# Построим последовательно линейную модель (что позволяет выявить возможные различий групповых средних)
m1 = lm(wage ~ educ, data = df)
summary(m1)

anova(m1) #Проведем однофакторный дисперсионный анализ.

# 2-ой  метод
m0= aov(data = df, wage ~ educ)
summary(m0)

# Доверительные интервалы для групповых средних
confint(m1) 

# Задание 4

# с учетом поправки Бонферрони
pairwise.t.test(df$wage, df$educ, p.adj = "bonferroni")

# без учета поправки Бонферрони
pairwise.t.test(df$wage, df$educ, p.adj = "none")

# Метод LSD (Фишера)
# install.packages('agricolae')
# install.packages('labelled')
library('agricolae')
library('labelled')


m0 <- aov(data=df, wage~educ)
df <- df.residual(m0)
MS.error <- deviance(m0)/df
(LSD.test(var3$wage, var3$educ, df, MS.error, group=FALSE))


# Метод HSD (Тьюки)
AOV <- aov(var3$wage ~ var3$educ)
hsd <- TukeyHSD(AOV)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(hsd, las=2)




