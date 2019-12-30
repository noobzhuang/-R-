
#####    庄俊伟   #########

setwd("E:\\Desktop\\business analysis\\R learning script")

library("tidyverse")
library("countrycode")
library("ggplot2")
library("VIM")
library("tidyr")
library("nutshell")
library("MASS")

data<-read.csv("E:\\Desktop\\business analysis\\R learning script\\Suicide rate.csv",header = T,sep = ",")
colnames(data)[1]<-"country"##修改第一列变量的名字
#数据初步探索
str(data)
view(data)
########################   tidydata    ###################################

##HDI 缺失比例
aggr(data,prop=T,numbers=T)
HDI_NA_rate<-mean(is.na(data$HDI.for.year))

##删除缺失值最多的一列
data.ty1<-data[,-9]
View(data.ty1)

###查看各个年份的数据比例
table2<-table(data.ty1$year) 
barplot(table2,xlab = "year",ylab="frequence",col="blue",main = "每年数据量")
data.ty2<-subset(data.ty1,data$year!=2016)##剔除2016 年的数据
view(data.ty2)

##查看各个国家的数据量大小
table3<-table(data.ty2$country) %>% sort(table3,decreasing = T)
barplot(table3,xlab ="country",ylab = "frequence",main = "每个国家的数据量",col="blue")
names(table3)
data.ty3<-subset(data.ty2,data.ty2$country %in% names(table3)[1:93])

#将年龄转为分类变量
#data.ty3$age<-factor(data.ty3$age,ordered = T,levels = 
                       #c("5-14","15-24", "25-34", "35-54", "55-74", "75+"))#将年龄转为分类变量

data.ty3$gdp_for_year....<-as.numeric(data.ty3$gdp_for_year....)
data.ty4<-data.ty3[,-8]###剔除第8 列

##新增加一列，将国家转为洲
data.ty4$continent<-countrycode(sourcevar = data.ty4[, "country"],
                                origin = "country.name",
                                destination = "continent")

data.ty4$continent<-as.factor(data.ty4$continent)
data.ty<-data.ty4
##############   data tidy end ###################################



############### data explore ###################
### expx 代表第x次数据探索
exp1<-data.ty %>%
  group_by(year) %>%
  summarize(population = sum(population),suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
ggplot(exp1,aes(x=year,y=suicides_per_100k))+
  geom_line(col="blue",size=1)+
  geom_point(col="blue",size=2)+
  labs(title = "1985-2015 每100k自杀人数",x="Year",y=" suicides_per_100k")+
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))

exp2<-data.ty %>%
  group_by(sex) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp2,aes(x = sex, y = suicides_per_100k, fill = sex)) + geom_bar(stat = "identity") +
  labs(title = "性别与自杀率")

exp3<-data.ty %>%
  group_by(age) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp3,aes(x = age, y = suicides_per_100k, fill = age)) + geom_bar(stat = "identity") +
  labs(title = "年龄与自杀率")

exp4<-data.ty %>%
  group_by(generation) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp4,aes(x = generation, y = suicides_per_100k, fill =generation)) + geom_bar(stat = "identity") +
  labs(title = "generation与自杀率")

exp5<-data.ty %>%
  group_by(continent) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp5,aes(x = continent, y = suicides_per_100k, fill =continent)) + geom_bar(stat = "identity") +
  labs(title = "不同洲与自杀率")


exp6<-data.ty %>%
  group_by(continent, sex) %>%
  summarize(
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000)

ggplot(exp6,aes(x = continent, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge")


exp7<-data.ty %>%
  group_by(year,age) %>%
    summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)
ggplot(exp7,aes(x = year, y = suicides_per_100k,col=age)) + 
  geom_line() + 
  geom_point() +
  facet_grid(age ~ ., scales = "free_y")+
  scale_x_continuous(breaks = seq(1985, 2015, 5))+
  labs(title = "年龄随着时间变化与自杀率")

exp8<-data.ty %>%
  group_by(sex,age) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp8,aes(x = age, y = suicides_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge")+labs(title = "性别、年龄与自杀率")


exp9 <- data.ty %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
ggplot(exp9,aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line(size=1) +
  labs(title = "性别、年份与自杀率", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)


exp10<-data.ty %>%
  group_by(year,continent) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000)

ggplot(exp10,aes(x = year, y = suicides_per_100k,col=continent))+ 
  geom_line() + 
  geom_point() +
  facet_grid(continent ~ ., scales = "free_y")+
  scale_x_continuous(breaks = seq(1985, 2015,5))+
  labs(title = "不同地区随时间的自杀率变化")


####################  data explore end ###################

#####################3   model  ###################

####回归模型1
model1_data<-data.ty %>% 
  group_by(country,year) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000,
            gdp_per_capita=mean(gdp_per_capita....))
model1 <- lm(suicides_per_100k ~ gdp_per_capita, data = model1_data)
summary(model1)
plot(model1)

ggplot(model1_data,aes(x=gdp_per_capita,y=suicides_per_100k))+
  geom_point(shape=19,col="blue",alpha=0.5)+
  geom_smooth(method = lm,col="red",size=1.2)+
  labs(title = "一元回归模型拟合图",size=3)

  
data.ty.lm<-lm(suicides.100k.pop~sex+age+gdp_per_capita....,data=data.ty)
lm_back<-stepAIC(data.ty.lm,direction = "backward")

model2_data<--data.ty %>% 
  group_by(country,year) %>%
  summarise(suicides_per_100k = sum(suicides_no)/sum(population)*100000,
            gdp_per_capita=mean(gdp_per_capita....))



######回归模型2

model3_data<-read.csv("E:/Desktop/model3_data.csv")
colnames(model3_data)[1]<-"country"

model3.lm <- lm(suicide_pop_100k~F_15.24.years_pop+ F_25.34.years_pop+F_35.54.years_pop+F_5.14.years_pop+
                  F_55.74.years_pop+F_75..years_pop+M_15.24.years_pop+ M_25.34.years_pop+ M_35.54.years_pop+
                  M_5.14.years_pop+M_55.74.years_pop+M_75..years_pop+gdp_per_capital,data=model3_data)
lm_back2<-stepAIC(model3.lm,direction = "backward")              
summary(model1)
plot(model1)


data.ty.lm<-lm(suicides.100k.pop~sex+age+gdp_per_capita....,data=data.ty)
lm_back<-stepAIC(data.ty.lm,direction = "backward")

model4.lm <- lm(suicide_pop_100k~ F_25.34.years_pop+F_35.54.years_pop+F_5.14.years_pop+
                  F_55.74.years_pop+F_75..years_pop+ M_25.34.years_pop+ M_35.54.years_pop+
                  M_75..years_pop,data=model3_data)
lm_back4<-stepAIC(model4.lm ,direction = "backward")


#######时间序列分析
library("forecast")
library("tseries")

US_data<-subset(data.ty,data.ty$country %in% c("United States"))

US_data_group<- US_data %>% group_by(year) %>%
  summarise(suicides_year=sum(suicides_no)/sum(population)*100000)

plot(US_data_group)
lines(US_data_group)
tsdisplay(US_data_group)

(mod_auto=auto.arima(US_data_group$suicides_year))
  
qqnorm(mod_auto$residuals)
qqline(mod_auto$residuals)
Box.test(mod_auto$residuals,type='Ljung-Box')

(pre=forecast(mod_auto,5))

par(new=T)
plot(US_data_group$suicides_year,col='pink',type = "o",lwd=2)
plot(pre,col='green',lwd=2,type="o")



 