
#下载打开包----

install.packages("tidyr") 
library(tidyr)            



#读文件----

read.csv("EmpetrumElongation.csv") 
read.csv("EmpetrumElongation.csv", header = TRUE) 
read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
read.csv("EmpetrumTreatments.csv", row.names=1)
read.csv("EmpetrumTreatments.csv", row.names=1, stringsAsFactors=T)



#看数据----

head(elongation)          # 看前面几行
str(elongation)           # 看有几个variance，看数据属于chr还是num
unique(elongation$Indiv)  # 看几种（名字）
length(elongation$Indiv)  # 看数量 （数字）
names(elong2)             # 看所有column的名字
levels(elong2$zone)       # 看有几个levels
dim(soils)                # 看column和row的number



#查看指定位置，数字----

elongation[2,5] 
elongation[6, ] 
elongation[6, ]$Indiv 
longation[elongation$Indiv == 603, ]



#符号意义----

# ==(完全等于)， <, <= ，>, >= , != (不等于),
# %in% (属于以下之一,通常后跟可能值的向量）
# & (and,但是是两个同时满足的条件)
# “|”  (or, 至少满足两个条件中的一个)
# !（not）

elongation[!elongation$Zone >= 5, ] 
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]



#创造数列----

seq(300, 400, 10)        #s equences创造300-400之间的数，增幅为10
rep(c(1,2), 3)           # repeat，重复1和2两个数三次
rep(seq(0, 30, 10), 4)   # 做0-30之间，增幅10的数，重复四次



#改column名字----

names(elong2)[1] <- "zone"                             # 把第1个名字改成zone
names(elong2)[2] <- "ID"                               # 把第2个名字改成ID
rename(elongation_long, zone = Zone, indiv = Indiv)    # 改多个名字,前者新后者旧
names(elongation_long) <- c("zone", "indiv")           # 改多个名字 - 方法2



#改数据----

elong2[1,4] <- 5.7                       #使用坐标位置
elong2[elong2$ID == 373, ]$X2008 <- 5.7  # 使用logical conditions
grepl("Acer", LatinName) ~ "Acer"        # 根据条件



#改数据类别----

as.factor(elong2$zone)                          # 把integer data（A,B,C）,改成数字，默认1,2,3,4,5.
as.numeric(soils$Habitat)
levels(elong2$zone) <- c("A", "B", "C")         # 自定义levels

#自定义levels - 进阶
factor(trees.genus$Height.cat,                  # 给Height.cat创造level
       levels = c('Short', 'Medium', 'Tall'),   # 给他一个level
       labels = c('SHORT', 'MEDIUM', 'TALL') )  # 新名字，新lable




#colnum 合并 - 创造新column----

### gather - (tidyr)   
gather(elongation, Year, Length,                               # 的顺序: data frame, key, value
       c(X2007, X2008, X2009, X2010, X2011, X2012))            # 需要 gather的columns

# 把range里的年份名字全部合起来，变成year，他们的数据然后放进新的length里
gather(elongation, Year, Length, c(3:8))                       # gather

### mutate - (dplyr)
mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)
#mutate 进阶, pipe, mutate, case when, name change

trees %>%                           
  mutate(Genus = case_when(                      # mutate = 创建新coloum，叫做Genus 后面的是条件
    grepl("Acer", LatinName) ~ "Acer",           #grepl指，当LatinName包含Acer时，新的coloum就是Acer
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula"))

#### tidyr::separate - (tidyr)
trees %>%
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  #首先用tidyr::separate，把LatinName 拆成两个coloum，第一个单词为一个coloum（Genus），
  #第二个单词为第二个coloum（Species）
  #remove = FALSE 就是不删除LatinName这个coloum
  
  
  
# 拆开gather----

spread(elongation_long, Year, Length)  



# 合并两个文件----

### left_join - (dplyr)
left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))



#过滤信息----

filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011"))                            #把属于2，3 zone，2009-2011 的数据过滤出来   
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]  # &替换，

# 写法1
soils[soils$Basin=="Manu"|soils$Basin=="Los_Amigos",]   # 指提取basin 为 Manu 和 Los_Amigos的数据
# 写法2
soils[soils$Basin %in% c("Manu","Los_Amigos"),]


#过滤column- 只显示其中的几个coloums----

### dplyr::select - (dplyr)
dplyr::select(elongation_long, indiv, year, length)   #加法
dplyr::select(elongation_long, -zone)                 # 减发
elongation_long[ , -1]                                # 最基本R
dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)  # 边过滤边column名字



#group_by----

group_by(elongation_long, indiv)  # 这个操作是肉眼看不到改变的，但是它内部以indiv分成了几个不同的组，用于简化后期处理



#summary----

summarise(elongation_long, total.growth = sum(length))  # sum
summarise(trees.grouped, count = length(CommonName))    # length 

### tally - (dplyr)使用前必须group by
tally(trees.grouped)                                    # 看以分类的column下，每个不同个体的个数，作用跟上一条summaries length一样
summarise(elong_grouped, total.growth = sum(length),    # 一堆
          mean.growth = mean(length),
          sd.growth = sd(length))
summarise_all(trees, mean)                              # summaries所有column的mean



#排列顺序----

merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  # 以treatments作为优先排列顺序



# pipes %>%----

trees.summary <- trees %>%
  group_by(CommonName) %>%
  tally() 



#ifeles----

ifelse(vector < 10, "A", "B")  # 如果vactor < 10, 就展示A, 不然就B



# case_when----

case_when(vector2 == "What am I?" ~ "I am the walrus",    #如果=What am I， 就I am the walrus
          vector2 %in% c("A", "B") ~ "goo",               #如果是A或者B， 就goo
          vector2 == "C" ~ "ga",                          #如果是C      ，就ga
          vector2 == "D" ~ "joob")                        #如果是D      ，就joob



# boxplot----

boxplot(Length ~ Year, data = elongation_long,              # y-length, x-year
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")



 #scateplot---- 

#总公式前后加()表示直接画图 (ggplot2)
(map.all <- ggplot(trees.five) +                                 # 数据点为trees.five
   geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) + 
   # x轴，y轴，数据点的大小=Height.cat的数值大小，color=不同Genus不同颜色， alpha=数据点的透明度
   theme_bw() +                                                  # 主体为最简单的黑白风格
   theme(panel.grid = element_blank(),                           # 风格中的其他元素  panel.grid删除网格线
         axis.text = element_text(size = 12),                    # 轴上文本=12
         legend.text = element_text(size = 12))                  # legent文本=12
)


# histogram ----

hist(soils$Soil_pH, xlab="Soil pH", main="")
abline(v = median(soils$Soil_pH))               # 加中位线
abline(v = mean(soils$Soil_pH))                 # 加平均线

plot(pH ~ Habitat, data=soils)                   # 普通方法，但是两者都要为numerical
hist(soils$Soil_pH, breaks=10)                   # 设置间隔为10，但只是建议，R不一定会采纳



#画图自定义标题，groupby，pipes----

trees.five%>% 
  group_by(Genus)%>%               # trees.five数据里，只显示根据Genus分组的数据
  do(plots =                       # do允许我们使用任何R指令，在do里面
       ggplot(data = .) +          # 会直接根据下面的指令自动提供数据
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) + # xy轴，大小，透明度
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) + 
       # paste把文本粘在一起：title里，Genus的文字会根据Genus而改变，sep指这三个组成部分之间有空格 
       theme_bw() +                                   # 主体黑白 
       theme(panel.grid = element_blank(),            # 删除网格线
             axis.text = element_text(size = 14),     # 轴上文本=14
             legend.text = element_text(size = 12),   # legent文本=12
             plot.title = element_text(hjust = 0.5),  # title文本位置=0.5
             legend.position = "bottom")              # legend在plot的下面
  )


# 给图中的数据点标识名字 ----

rownames (soils) 



#合并进阶---

#以高度为条件，创造一个新的coloum (pipes. mutate,case when)
trees.genus <- trees.genus %>%                                                         # overwriting our data frame
  mutate(Height.cat =                                                                  # creating our new column   #再创造一个新coloum（Height.cat）
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",      # 当hight ="Up to 5 meters", "5 to 10 meters"， 新coloum为Short
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",   #。。。。
                     Height == "20 to 25 meters" ~ "Tall")                             #。。。。
  )
# (pipes, filter)

trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# 保存多个plot，并且不同自定义名字

tree.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm")) 


# linear model (lm) ----
lm(height~fertiliser, data = alldata)
lm(height~fertiliser, data = alldata[-1,])      # 指数据框中排除第一行
lm(log(height)~fertiliser, data = alldata)      # log数据

anova(height_lm)                                # 主要看F-test, p值<0.05为显著
summary(height_lm)
resid(height_lm)                                # 找residual，检测是否有constant variances，
                                                # 也可以检测dependent variable 和independent variable 是否non-linear manner
shapiro.test(resid(height_lm))                  # 看residual的normality (里面只能放residuals)
                                                # p-value<0.05, model is normal distributed
bartlett.test(height~fertiliser, data=alldata)  # 看equality of variances，也可以看homoscedasticity

kruskal.test(Sodium~ Habitat,data=soils)        # p值<0.05，指可以肯定地说habitat会影响sodium的浓度
                                                # p值>0.05说明它有homogeneous variances
cor.test(soils$Sand, soils$Saturation,data=soils, method="spearman",exact=FALSE)  # correlation test
                                                # p值<0.05， 指两者完全不相关

TukeyHSD(aov(height~fertiliser,data=alldata))   # 线是平的就是有显著，线约平越好


plot(height_lm)                                 # 画四个图 (QQ plot)
                                                # QQ plot - 点符合线，就是model normal distributed, tail出现偏离是small data set 的常见特点
                                                # 第三个图(bartlett.test),线最好是平的，斜的解决方法是log数据 
log(soils$Potassium)                            # log数据

abline(lm(pH~Saturation,data=soils))            # 可以做一个model，然后作为最佳线



# linear model (lm) (plus) ----

lm(Phosphorus~ Habitat + Basin, data=soils_trim)


# linear model (lm) (interaction) ----
# 写法1
lm(pH~Habitat + Saturation + Habitat:Saturation, data=soils)  #就是ph ~（habitat + saturation + habitat：saturation），“：”指interaction
# 写法2
lm(pH ~ Habitat* Saturation, data=soils)


# AIC ----

AIC(lm_Habitat, lm_TBS)    # 比较不同model的AIC

# 手算AIC

logLik (model)             #先找出model的likelihood，结果为 3.59134
2*4 - 2*3.59134            #代入公式得出结果：0.81732



















