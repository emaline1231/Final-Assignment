
data <- emily_dataset
bmi <- emily_dataset$bmi
systolic <- emily_dataset$systolic
diastolic <- emily_dataset$diastolic
age <- emily_dataset$age
gender <- emily_dataset$gender
weight <- emily_dataset$weight
height <- emily_dataset$height
plot(bmi,systolic)
plot(bmi,diastolic)
colMeans(data[sapply(data, is.numeric)])
a <- ggplot(data = emily_dataset, aes(bmi,systolic))+geom_point()
b <- a + geom_smooth(method='lm')
b + stat_cor(method = "pearson", alternative = "two.sided", cor.coef.name = "R",label.x.npc = 0.80, label.y.npc = 0.97)
c <- ggplot(data = emily_dataset, aes(bmi,diastolic))+geom_point()
d <- a + geom_smooth(method='lm')
d + stat_cor(method = "pearson", alternative = "two.sided", cor.coef.name = "R",label.x.npc = 0.60, label.y.npc = 0.97)
cor.test(age,systolic)
cor.test(height,systolic)
cor.test(weight,systolic)

Calculate row stats of a table
apply(data,2,sd,is.numeric)
colMeans(data[sapply(data, is.numeric)])

Correlation Matrix
cormat <- round(cor(NumbersOnly),2)
head(cormat)
get_upper_tri <- function(cormat){cormat[lower.tri(cormat)]<-NA
return(cormat)}
TriMap <- melt(upper_tri, na.rm = TRUE)
ColorMap <- ggplot(data = TriMap, aes(Var2, Var1, fill = value))+
geom_tile(color = "white")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white",
midpoint = 0, limit = c(-1,1), space = "Lab",
name="Pearson\nCorrelation") +
theme_minimal()+
theme(axis.text.x = element_text(angle = 45, vjust = 1,
size = 12, hjust = 1))+
coord_fixed()
FinalMap <- ColorMap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
FinalMap


