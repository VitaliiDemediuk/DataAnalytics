# Udemy Courses Datasets
# dataset url - https://www.kaggle.com/andrewmvd/udemy-courses

# library
library(tidyverse)
library(moments)
library(gamlss)
library(goftest)
library(SMPracticals)

## based on Box-and-whisker plot
remove.outlires <- function(x){
  iqr = IQR(x);
  qv = quantile(x, p = c(0.25, 0.75));
  max.x <- qv[["75%"]] + 1.5 * iqr;
  min.x <- qv[["25%"]] - 1.5 * iqr;
  x <- x[x <= max.x];
  x <- x[x >= min.x];
  return(x)
}

# load, prepare and clean datasets
dataset<-read.csv(file="udemy_courses.csv", header = TRUE)

dataset <- subset(dataset, select = -c(`course_title`, `url`, `published_timestamp`))

dataset$is_paid <- as.factor(dataset$is_paid)
dataset$level <- as.factor(dataset$level)
dataset$subject <- as.factor(dataset$subject)

summary(dataset)
view(dataset)

nsub = dataset$num_subscribers

# 1. Підрахувати показники центру: середнє значення, медіану.

# Average
mean(nsub)

#Median
median(nsub)

# 2. Підрахувати показники варіації: дисперсію, стандартне відхилення, коефіцієнт
# варіації, розмах варіації та інтерквартильний розмах.

# Dispersion
var(nsub)

# Standard deviation
st.dev <-sd(nsub)
st.dev

# Coefficient of variation
st.dev/mean(nsub)

# Range of variation
range(nsub)
max(nsub)-min(nsub)

# Interquartile range
IQR(nsub)

# 3. Побудувати ящик з вусами (з підписами).
boxplot(nsub, horizontal=T, xlab='Кількість підписників на курс', outline=F, main='Ящик з вусами, без викидів')
boxplot(nsub, horizontal=T, xlab='Кількість підписників на курс', outline=T, main='Ящик з вусами, з викидами')

# 4. Вивести п’ятиточкову характеристику (екстремальні точки та квартилі). xmin, Q1, Q2, Q3, xmax
quantile(nsub)

# 5. Знайти 1-й та 9-й децилі.
quantile(nsub, p = c(0.1, 0.9))

# 6. Знайти коефіцієнт асиметрії та коефіцієнт ексцесу.

# Коефіцієнт асиметрії
# (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
skewness(nsub)

# Коефіцієнт ексцесу 
# n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
kurtosis(nsub)

# remove outliers, optional
nsub <- remove.outlires(nsub)
summary(nsub)

# 7. Побудувати гістограму, використовуючи різні методи групування (базове правило, правило Скотта,
#    правило Фрідмана-Діаконіса).
#    На тому ж графіку зобразити оцінку щільності та графік щільності гіпотетичного розподілу. 
#    Сформулювати вербально гіпотезу щодо типу розподілу, що спостерігається.

k <- 1+log2(length(nsub))
hist(nsub, xlab='Кількість підписників на курс', ylab='Кількість курсів', main='Базове групування', breaks = k)

# h = 3.5 * sd(x) * length(x)^(-1/3)
hist(nsub, xlab='Кількість підписників на курс', ylab='Кількість курсів', main='Групування Скотта', breaks = "Scott")

# h = 2 * IQR(x) * length(x)^(-1/3)
hist(nsub, xlab='Кількість підписників на курс', ylab='Кількість курсів', main='Групування Фрідмана та Діаконіса', breaks = "FD")

plot(density(nsub), xlab='Кількість підписників на курс', ylab='', main='Оцінка щільності')

hd.values = histDist(nsub, family = "EXP", density = TRUE, xlab='Кількість підписників на курс', ylab='', main='Гістограму з теоретичною щільністю')

# 8. Зобразити Q-Q-діаграму для перевірки узгодженості з гіпотетичним розподілом.
qqexp(nsub, line=T)

# 9. Зобразити P-P-діаграму для перевірки узгодженості з гіпотетичним розподілом.
plot(pexp(sort(nsub), 1/hd.values$mu), (1:length(nsub))/length(nsub), xlab="Теоретична функція розподілу", ylab = "Емпірична функція розподілу", main = "P-P діаграма", asp = 1)
abline(0, 1, col="red")

# 10. За допомогою одного з статистичних критеріїв перевірити згоду з гіпотетичним розподілом.
cvm.test(nsub, "pexp", rate=1/hd.values$mu)
        