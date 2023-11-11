
#���ش򿪰�----

install.packages("tidyr") 
library(tidyr)            



#���ļ�----

read.csv("EmpetrumElongation.csv") 
read.csv("EmpetrumElongation.csv", header = TRUE) 
read.csv("EmpetrumTreatments.csv", header = TRUE, sep = ";")
read.csv("EmpetrumTreatments.csv", row.names=1)
read.csv("EmpetrumTreatments.csv", row.names=1, stringsAsFactors=T)



#������----

head(elongation)          # ��ǰ�漸��
str(elongation)           # ���м���variance������������chr����num
unique(elongation$Indiv)  # �����֣����֣�
length(elongation$Indiv)  # ������ �����֣�
names(elong2)             # ������column������
levels(elong2$zone)       # ���м���levels
dim(soils)                # ��column��row��number



#�鿴ָ��λ�ã�����----

elongation[2,5] 
elongation[6, ] 
elongation[6, ]$Indiv 
longation[elongation$Indiv == 603, ]



#��������----

# ==(��ȫ����)�� <, <= ��>, >= , != (������),
# %in% (��������֮һ,ͨ���������ֵ��������
# & (and,����������ͬʱ���������)
# ��|��  (or, �����������������е�һ��)
# !��not��

elongation[!elongation$Zone >= 5, ] 
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]



#��������----

seq(300, 400, 10)        #s equences����300-400֮�����������Ϊ10
rep(c(1,2), 3)           # repeat���ظ�1��2����������
rep(seq(0, 30, 10), 4)   # ��0-30֮�䣬����10�������ظ��Ĵ�



#��column����----

names(elong2)[1] <- "zone"                             # �ѵ�1�����ָĳ�zone
names(elong2)[2] <- "ID"                               # �ѵ�2�����ָĳ�ID
rename(elongation_long, zone = Zone, indiv = Indiv)    # �Ķ������,ǰ���º��߾�
names(elongation_long) <- c("zone", "indiv")           # �Ķ������ - ����2



#������----

elong2[1,4] <- 5.7                       #ʹ������λ��
elong2[elong2$ID == 373, ]$X2008 <- 5.7  # ʹ��logical conditions
grepl("Acer", LatinName) ~ "Acer"        # ��������



#���������----

as.factor(elong2$zone)                          # ��integer data��A,B,C��,�ĳ����֣�Ĭ��1,2,3,4,5.
as.numeric(soils$Habitat)
levels(elong2$zone) <- c("A", "B", "C")         # �Զ���levels

#�Զ���levels - ����
factor(trees.genus$Height.cat,                  # ��Height.cat����level
       levels = c('Short', 'Medium', 'Tall'),   # ����һ��level
       labels = c('SHORT', 'MEDIUM', 'TALL') )  # �����֣���lable




#colnum �ϲ� - ������column----

### gather - (tidyr)   
gather(elongation, Year, Length,                               # ��˳��: data frame, key, value
       c(X2007, X2008, X2009, X2010, X2011, X2012))            # ��Ҫ gather��columns

# ��range����������ȫ�������������year�����ǵ�����Ȼ��Ž��µ�length��
gather(elongation, Year, Length, c(3:8))                       # gather

### mutate - (dplyr)
mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)
#mutate ����, pipe, mutate, case when, name change

trees %>%                           
  mutate(Genus = case_when(                      # mutate = ������coloum������Genus �����������
    grepl("Acer", LatinName) ~ "Acer",           #greplָ����LatinName����Acerʱ���µ�coloum����Acer
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula"))

#### tidyr::separate - (tidyr)
trees %>%
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  #������tidyr::separate����LatinName �������coloum����һ������Ϊһ��coloum��Genus����
  #�ڶ�������Ϊ�ڶ���coloum��Species��
  #remove = FALSE ���ǲ�ɾ��LatinName���coloum
  
  
  
# ��gather----

spread(elongation_long, Year, Length)  



# �ϲ������ļ�----

### left_join - (dplyr)
left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))



#������Ϣ----

filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011"))                            #������2��3 zone��2009-2011 �����ݹ��˳���   
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]  # &�滻��

# д��1
soils[soils$Basin=="Manu"|soils$Basin=="Los_Amigos",]   # ָ��ȡbasin Ϊ Manu �� Los_Amigos������
# д��2
soils[soils$Basin %in% c("Manu","Los_Amigos"),]


#����column- ֻ��ʾ���еļ���coloums----

### dplyr::select - (dplyr)
dplyr::select(elongation_long, indiv, year, length)   #�ӷ�
dplyr::select(elongation_long, -zone)                 # ����
elongation_long[ , -1]                                # �����R
dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)  # �߹��˱�column����



#group_by----

group_by(elongation_long, indiv)  # ������������ۿ������ı�ģ��������ڲ���indiv�ֳ��˼�����ͬ���飬���ڼ򻯺��ڴ���



#summary----

summarise(elongation_long, total.growth = sum(length))  # sum
summarise(trees.grouped, count = length(CommonName))    # length 

### tally - (dplyr)ʹ��ǰ����group by
tally(trees.grouped)                                    # ���Է����column�£�ÿ����ͬ����ĸ��������ø���һ��summaries lengthһ��
summarise(elong_grouped, total.growth = sum(length),    # һ��
          mean.growth = mean(length),
          sd.growth = sd(length))
summarise_all(trees, mean)                              # summaries����column��mean



#����˳��----

merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  # ��treatments��Ϊ��������˳��



# pipes %>%----

trees.summary <- trees %>%
  group_by(CommonName) %>%
  tally() 



#ifeles----

ifelse(vector < 10, "A", "B")  # ���vactor < 10, ��չʾA, ��Ȼ��B



# case_when----

case_when(vector2 == "What am I?" ~ "I am the walrus",    #���=What am I�� ��I am the walrus
          vector2 %in% c("A", "B") ~ "goo",               #�����A����B�� ��goo
          vector2 == "C" ~ "ga",                          #�����C      ����ga
          vector2 == "D" ~ "joob")                        #�����D      ����joob



# boxplot----

boxplot(Length ~ Year, data = elongation_long,              # y-length, x-year
        xlab = "Year", ylab = "Elongation (cm)",
        main = "Annual growth of Empetrum hermaphroditum")



 #scateplot---- 

#�ܹ�ʽǰ���()��ʾֱ�ӻ�ͼ (ggplot2)
(map.all <- ggplot(trees.five) +                                 # ���ݵ�Ϊtrees.five
   geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) + 
   # x�ᣬy�ᣬ���ݵ�Ĵ�С=Height.cat����ֵ��С��color=��ͬGenus��ͬ��ɫ�� alpha=���ݵ��͸����
   theme_bw() +                                                  # ����Ϊ��򵥵ĺڰ׷��
   theme(panel.grid = element_blank(),                           # ����е�����Ԫ��  panel.gridɾ��������
         axis.text = element_text(size = 12),                    # �����ı�=12
         legend.text = element_text(size = 12))                  # legent�ı�=12
)


# histogram ----

hist(soils$Soil_pH, xlab="Soil pH", main="")
abline(v = median(soils$Soil_pH))               # ����λ��
abline(v = mean(soils$Soil_pH))                 # ��ƽ����

plot(pH ~ Habitat, data=soils)                   # ��ͨ�������������߶�ҪΪnumerical
hist(soils$Soil_pH, breaks=10)                   # ���ü��Ϊ10����ֻ�ǽ��飬R��һ�������



#��ͼ�Զ�����⣬groupby��pipes----

trees.five%>% 
  group_by(Genus)%>%               # trees.five�����ֻ��ʾ����Genus���������
  do(plots =                       # do��������ʹ���κ�Rָ���do����
       ggplot(data = .) +          # ��ֱ�Ӹ��������ָ���Զ��ṩ����
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) + # xy�ᣬ��С��͸����
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) + 
       # paste���ı�ճ��һ��title�Genus�����ֻ����Genus���ı䣬sepָ��������ɲ���֮���пո� 
       theme_bw() +                                   # ����ڰ� 
       theme(panel.grid = element_blank(),            # ɾ��������
             axis.text = element_text(size = 14),     # �����ı�=14
             legend.text = element_text(size = 12),   # legent�ı�=12
             plot.title = element_text(hjust = 0.5),  # title�ı�λ��=0.5
             legend.position = "bottom")              # legend��plot������
  )


# ��ͼ�е����ݵ��ʶ���� ----

rownames (soils) 



#�ϲ�����---

#�Ը߶�Ϊ����������һ���µ�coloum (pipes. mutate,case when)
trees.genus <- trees.genus %>%                                                         # overwriting our data frame
  mutate(Height.cat =                                                                  # creating our new column   #�ٴ���һ����coloum��Height.cat��
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",      # ��hight ="Up to 5 meters", "5 to 10 meters"�� ��coloumΪShort
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",   #��������
                     Height == "20 to 25 meters" ~ "Tall")                             #��������
  )
# (pipes, filter)

trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# ������plot�����Ҳ�ͬ�Զ�������

tree.plots %>%              # the saving call within the do function
  do(.,
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm")) 


# linear model (lm) ----
lm(height~fertiliser, data = alldata)
lm(height~fertiliser, data = alldata[-1,])      # ָ���ݿ����ų���һ��
lm(log(height)~fertiliser, data = alldata)      # log����

anova(height_lm)                                # ��Ҫ��F-test, pֵ<0.05Ϊ����
summary(height_lm)
resid(height_lm)                                # ��residual������Ƿ���constant variances��
                                                # Ҳ���Լ��dependent variable ��independent variable �Ƿ�non-linear manner
shapiro.test(resid(height_lm))                  # ��residual��normality (����ֻ�ܷ�residuals)
                                                # p-value<0.05, model is normal distributed
bartlett.test(height~fertiliser, data=alldata)  # ��equality of variances��Ҳ���Կ�homoscedasticity

kruskal.test(Sodium~ Habitat,data=soils)        # pֵ<0.05��ָ���Կ϶���˵habitat��Ӱ��sodium��Ũ��
                                                # pֵ>0.05˵������homogeneous variances
cor.test(soils$Sand, soils$Saturation,data=soils, method="spearman",exact=FALSE)  # correlation test
                                                # pֵ<0.05�� ָ������ȫ�����

TukeyHSD(aov(height~fertiliser,data=alldata))   # ����ƽ�ľ�������������ԼƽԽ��


plot(height_lm)                                 # ���ĸ�ͼ (QQ plot)
                                                # QQ plot - ������ߣ�����model normal distributed, tail����ƫ����small data set �ĳ����ص�
                                                # ������ͼ(bartlett.test),�������ƽ�ģ�б�Ľ��������log���� 
log(soils$Potassium)                            # log����

abline(lm(pH~Saturation,data=soils))            # ������һ��model��Ȼ����Ϊ�����



# linear model (lm) (plus) ----

lm(Phosphorus~ Habitat + Basin, data=soils_trim)


# linear model (lm) (interaction) ----
# д��1
lm(pH~Habitat + Saturation + Habitat:Saturation, data=soils)  #����ph ~��habitat + saturation + habitat��saturation����������ָinteraction
# д��2
lm(pH ~ Habitat* Saturation, data=soils)


# AIC ----

AIC(lm_Habitat, lm_TBS)    # �Ƚϲ�ͬmodel��AIC

# ����AIC

logLik (model)             #���ҳ�model��likelihood�����Ϊ 3.59134
2*4 - 2*3.59134            #���빫ʽ�ó������0.81732


















