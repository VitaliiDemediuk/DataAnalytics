# Udemy Courses Datasets
# dataset url - https://www.kaggle.com/andrewmvd/udemy-courses

# library
library(tidyverse) # general


# load, prepare and clean datasets
dataset<-read.csv(file="udemy_courses.csv", header = TRUE)

dataset <- select(dataset, -c(`course_title`, `url`, `published_timestamp`))

dataset$is_paid <- as.factor(dataset$is_paid)
dataset$level <- as.factor(dataset$level)
dataset$subject <- as.factor(dataset$subject)

summary(dataset)
view(dataset)