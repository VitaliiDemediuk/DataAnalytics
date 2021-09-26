# Udemy Courses Datasets
# dataset url - https://www.kaggle.com/andrewmvd/udemy-courses

# library
library(tidyverse) # general
library(moments)
library(gamlss)


# load, prepare and clean datasets
dataset<-read.csv(file="udemy_courses.csv", header = TRUE)

dataset <- select(dataset, -c(`course_title`, `url`, `published_timestamp`))

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

# коефіцієнт варіації
st.dev/mean(nsub)

# Range of variation
range(nsub)
max(nsub)-min(nsub)

# інтерквартильний розмах
IQR(nsub)

# 3. Побудувати ящик з вусами (з підписами).
boxplot(nsub, horizontal=T, xlab='Кількість підписників на курс', outline=F, main='Ящик з вусами')

# 4. Вивести п’ятиточкову характеристику (екстремальні точки та квартилі). xmin, Q1, Q2, Q3, xmax
quantile(nsub)

# 5. Знайти 1-й та 9-й децилі.
quantile(nsub, p = c(0.1, 0.9))

# 6. Знайти коефіцієнт асиметрії та коефіцієнт ексцесу.

# Асиметрія
# (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
skewness(nsub)

# Ексцес
# n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
kurtosis(nsub)
