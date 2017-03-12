### Reading Guide 1 --- Data Mining --- M.Sc. Data Science


## load library
library(xlsx)
library(dplyr)
library(ggplot2)
library(data.table)
## load data frame
df.students <- read.xlsx(file = "./students.xls", sheetIndex = 1 )
#str(df.ffs2010) ## check column types
df.students$Mark <- as.numeric(df.students$Mark) ## make mark numeric
df.students$Attended <- as.numeric(df.students$Attended) ## make mark numeric

df.ffs2010 <- dplyr::filter(df.students, Semester == "FSS2010") ## filter semester

## plot grades in histogram
ggplot(data=df.ffs2010, aes(Mark))+
  geom_histogram(binwidth = 0.1)+
  ggtitle("Marks in SS2010")+
  xlab("Mark")+
  ylab("Count")
## 2 and 3 are most common marks


ggplot(data=df.ffs2010, aes(x = Attended, y = Mark))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_smooth(method = 'loess')+
  ggtitle("Marks in SS2010")+
  xlab("Attendend Classes")+
  ylab("Mark")
### no clear result. but linear regression shows improved marks with more attended classes


### aggregrate

df.ffs2010 <- as.data.table(df.ffs2010)

df.ffs2010.agg.mark <- as.data.frame(df.ffs2010[, mean(Mark, na.rm = TRUE), by = Name])
df.ffs2010.agg.attended <- as.data.frame(df.ffs2010[, sum(Attended, na.rm = TRUE), by = Name])


df.ffs2010.agg <- dplyr::left_join(df.ffs2010.agg.attended, df.ffs2010.agg.mark, by  = "Name")

ggplot(data=df.ffs2010.agg,aes(x = V1.x, y = V1.y, color = Name))+
  geom_point()+
  geom_smooth(method = 'lm')+
  geom_smooth(method = 'loess')+
  ggtitle("Marks in SS2010")+
  xlab("Attendend Classes")+
  ylab("Mark")
## average attended classes gived a clearer picture than sum of attended classes.
