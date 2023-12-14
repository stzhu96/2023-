library(openxlsx)
data<-read.xlsx("G:\\2023培训班\\基本数据处理\\2019年晋城市环保资料.xlsx")
summary(data)
table(data$环保站点名称)
library(dplyr)
data %>%
  count(环保站点名称)

data<- subset(data, 环保站点名称 != "技术学院")
str(data)
#查看数据
data %>%
  count(环保站点名称)

################################################################################
#环保资料处理
data2<-read.xlsx("G:\\2023培训班\\基本数据处理\\2019年晋城市环保资料.xlsx",sheet=2)
summary(data2)
grouped_data <- split(data2, rep(1:ceiling(nrow(data2)/15), each = 15, length.out = nrow(data2)))
# 计算每组的平均值
group_means <- lapply(grouped_data, function(group) {
  col_means <- colMeans(group, na.rm = TRUE)  # 每列的平均值
  col_means[is.nan(col_means)] <- NA  # 将NaN替换为NA
  col_means
})
result_table <- do.call(rbind, group_means)
str(result_table)
View(result_table)
################################################################################
##气象数据处理
# 读取数据集
library(readxl)
data3 <- read_excel("G:\\2023培训班\\基本数据处理\\2019年晋城市气象资料数据.xls",sheet=2)
str(data3)
# 将数据集拆分成每2行一组
grouped_data <- split(data3, rep(1:ceiling(nrow(data3)/2), each = 2, length.out = nrow(data3)))

# 计算每组的平均值
group_means <- lapply(grouped_data, function(group) {
  col_means <- colMeans(group, na.rm = TRUE)  # 每列的平均值
  col_means[is.nan(col_means)] <- NA  # 将NaN替换为NA
  col_means
})
# 合并每个组的平均值为一个数据框
result_table2 <- do.call(rbind, group_means)
View(result_table2)
################################################################################
##死因检测资料处理
data4<-read.xlsx("G:\\2023培训班\\基本数据处理\\2019年晋城市死因监测数据.xlsx")
summary(data4)
data4$date<-as.Date(data4$死亡时间,origin = "1899-12-30")
summary(data4$date)
deathsum<-table(data4$date)
View(deathsum)
################################################################################
##门诊资料处理
data5<-read.xlsx("G:\\2023培训班\\基本数据处理\\2019年晋城市医院门诊数据.xlsx",sheet=2)
str(data5)
data5$门诊总量<-as.numeric(data5$门诊总量)
# 使用 na.omit() 函数删除包含缺失值的行
data5 <- na.omit(data5)
any(is.na(data3))
# 使用dplyr库进行按日期求和
result <- data5 %>%
  group_by(接诊日期) %>%
  summarize(Sum_Value = sum(门诊总量))
# 打印结果
View(result)
################################################################################
#合并数据集
View(result_table2)
View(result_table)
View(deathsum)
View(result)

merged_df <- cbind(result_table2, result_table, deathsum, result)
View(merged_df)
str(merged_df)
merged_df$date<-as.Date(merged_df$Var1)


################################################################################
##相关性分析
# 重命名第7列和第11列
names(merged_df)[9] <- "每日死亡人数"
names(merged_df)[11] <- "每日门诊人数"
# 选择特定的行和列
selected_data <- merged_df[c(1:7,9,11)]
# 使用 cor() 函数计算相关性矩阵
cor_matrix <- cor(selected_data)
# 输出相关性矩阵
print(cor_matrix)
# 绘制相关性矩阵的热图
library(corrplot)
corrplot(cor_matrix, method = "color", addCoef.col = "white")



################################################################################
##绘制时间序列图
install.packages("ggplot2")
library(ggplot2)
p1<- ggplot(data = merged_df, aes(x = date, y = PM10)) + 
  geom_line(color = "#00AFBB", size = 1) +
  labs(x="时间",y = bquote(每日PM[10]~浓度~(μg/m^3))) +  # 修改y轴的名称
  theme_minimal() 
p1
p2<-p1+ stat_smooth(
  color = "#FC4E07",fill = "#FC4E07",
  method = "gam")
p2
################################################################################
##广义相加模型
library(splines)
library(mgcv)
merged_df$dow<-as.factor(weekdays(merged_df$date))
str(merged_df)
model<-gam(每日死亡人数~PM10+ns(date,df=1*7)+
             ns(CO,df=1*3)+ns(SO2,df=1*3)+ns(NO2,df=1*3)+
             ns(O3,df=1*3)+ns(平均温度 ,df=1*6)+ns(平均温度,df=1*3)+as.factor(dow),
           data=merged_df,
           family=quasipoisson)
summary(model)


################################################################################
##cox比例风险模型
install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
data<-read_excel("G:\\2023培训班\\基本数据处理\\2019年晋城市死因监测数据.xlsx",sheet=2)
str(data)
data$time<-as.numeric(data$time)
res.cox <- coxph(Surv(time, status) ~ PM10+ sex , data=data)
summary(res.cox)
在多元Cox分析中，PM10不显着（p = 0.86，大于0.05）。如果有意义，每上升1ug/m^3则风险高0.0046%.
性别的p<0.001，风险比HR = exp（coef）= 145.8，表明患者的性别与死亡风险降低之间有很强的关系。例如，保持其他协变量不变的前提条件下，女性（性别= 2）是男性死亡风险的145.8倍。
我们得出的结论是，男性与良好的预后相关。

# 绘出基线的生存函数
ggsurvplot(survfit(res.cox), color = "#2E9FDF",data=data,
           ggtheme = theme_minimal())

# 创建新的数据表
sex_df <- with(data,
               data.frame(sex = c(1, 2),
                          PM10= rep(mean(PM10, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df

fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),data=lung,
           ggtheme = theme_minimal())

################################################################################
##安装并加载R包
library(mgcv)
library(splines)
##读取数据集
data<-chicagoNMMAPS
str(data)
summary(data)

library(zoo)
data$pm10<-na.approx(data$pm10)
any(is.na(data$pm10))

# 准备一个8行3列表
tablag <- matrix(NA,7+1,3,dimnames=list(paste("Lag",0:7),
                                        c("RR","ci.low","ci.hi")))
# 进行for循环
for(i in 0:7) {
  # 滞后7天
  o3lag<- Lag(data$o3,i)
  #pm10lag <- Lag(data$pm10,i)
  # 进行拟合
  mod <- glm(death~ o3lag+ns(temp,39)+ns(time,51) +as.factor(dow),data,
             family=quasipoisson)
  tablag[i+1,] <- ci.lin(mod,subset="o3lag",Exp=T)[5:7]
}
tablag

plot(0:7,0:7,type="n",ylim=c(0.999,1.0015),main="Lag terms modelled one at a time",
     xlab="Lag (days)",ylab="RR and 95%CI per 1ug/m3 ozone increase")
abline(h=1)
arrows(0:7,tablag[,2],0:7,tablag[,3],length=0.05,angle=90,code=3)
points(0:7,tablag[,1],pch=19)


# 准备一个8行3列表
tablag <- matrix(NA,7+1,3,dimnames=list(paste("Lag",0:7),
                                        c("RR","ci.low","ci.hi")))
# 进行for循环
for(i in 0:7) {
  # 滞后7天
  #o3lag<- Lag(data$o3,i)
  pm10lag <- Lag(data$pm10,i)
  # 进行拟合
  mod <- glm(death~ pm10lag+ns(temp,39)+ns(time,51) +as.factor(dow),data,
             family=quasipoisson)
  tablag[i+1,] <- ci.lin(mod,subset="pm10lag",Exp=T)[5:7]
}
tablag

plot(0:7,0:7,type="n",ylim=c(0.999,1.001),main="Lag terms modelled one at a time",
     xlab="Lag (days)",ylab="RR and 95%CI per 1ug/m3 pm10 increase")
abline(h=1)
arrows(0:7,tablag[,2],0:7,tablag[,3],length=0.05,angle=90,code=3)
points(0:7,tablag[,1],pch=19)

################################################################################
##双污染物模型
# 准备一个8行3列表
tablag <- matrix(NA,7+1,3,dimnames=list(paste("Lag",0:7),
                                        c("RR","ci.low","ci.hi")))
# 进行for循环
for(i in 0:2) {
  # 滞后7天
  o3lag2<- Lag(data$o3,2)
  pm10lag <- Lag(data$pm10,i)
  # 进行拟合
  mod <- glm(death~ pm10lag+o3lag2+ns(temp,39)+ns(time,51) +as.factor(dow),data,
             family=quasipoisson)
  tablag[i+1,] <- ci.lin(mod,subset="pm10lag",Exp=T)[5:7]
}
tablag

o3lag2<- Lag(data$o3,2)
mod <- glm(death~ pm10+o3lag2+ns(temp,39)+ns(time,51) +as.factor(dow),data,
           family=quasipoisson)
summary(mod)


#############
##############################################
##准备工作
##############################################
#install.packages("bkmr")  #下载需要用到的包
#install.packages("ggplot2") 
library(bkmr)#BKMR程序包实现BKMR的主要包
library(ggplot2)#绘制反应曲线

#使用bkmr程序包内置的模拟数据集
set.seed(111)#设置种子数，重复结果，如果下次你把种子数改为121数据就变了
dat <- SimData(n = 50, M = 4)#Simdata函数模拟预测变量、协变量和连续结果数据；n为观察次数；M为要生成的预测变量数；这个地方不用管到时候用自己数据就行
#?SimData #用?+函数名查看函数的用法
y <- dat$y #y为长度为 n 的结果数据向量。同样适用你的数据
Z <- dat$Z #Z 一个包含在 h 函数中的预测变量的 n×M 矩阵。 每个行代表一个观察值，每列代表一个预测变量。
X <- dat$X #X 协变量数据的 n×K 矩阵，其中每一行代表一个观察值和每列代表一个协变量。 不应包含拦截列。

View(y) #查看y的数据情况，为一列50行的数据。
View(Z) #Z为4×50的矩阵
View(X) #X为1×50的矩阵



#install.packages("openxlsx")
library(openxlsx)
yourdata<-read.xlsx("c:/yourdata.xlsx", sheet = 1)

#Z<- as.matrix(yourdata[, 2:6])  #暴露变量为（第2列至6列），as.matrix为创建矩阵，即变为矩阵形式
#X <- as.matrix(yourdata[, 7:9])  #协变量（第7列至9列），as.matrix为创建矩阵
#y <- yourdata$y #结局
#接下来继续模拟数据


############################################################
##开始贝叶斯回归
############################################################
set.seed(123) #同样道理设置种子数
fitkm <- kmbayes(y = y, Z = Z, X = X, iter = 100, verbose = FALSE, varsel = TRUE)#kmbayes为运行BKMR函数；y：反应变量；Z：混合物；X：协变量；iter：模拟次数，一般需要25,000以上，这里设置100，只是方便出结果；verbose：是否应打印总结模型拟合进度的临时输出，一般选否（FSLAE）；varsel：是否对暴露变量进行变量选择，一般选是（TRUE）

#结果1，ExtractPIPs函数看暴露变量的后验包含概率(PIP)，PIP表示对结局影响的相对重要程度，越高表示对结局越重要
ExtractPIPs(fitkm)#PIP大的重要


#结果2，单暴露反应
pred.resp.univar <- PredictorResponseUnivar(fit=fitkm) 
pred.resp.univar 
#数据预测计算,得到具有预测变量名称、预测变量值、后验均值估计和后验标准差的长数据框
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + facet_wrap(~ variable) + ylab("gu(z)")  #可视化#
#重点1：aes()函数确定x轴y轴，ymin，ymax确定y轴最小最大值
#重点2：geom_smooth()函数用于在散点图中添加拟合曲线或回归线。如果使用geom_smooth(stat = "identity")，则不会对数据进行拟合，而是将原始数据点直接连接成线条。也就是说，这个参数强制将拟合的统计方法设置为“identity”，这将绕过stat_smooth()函数的拟合功能，并直接将原始数据点连接成线条。
#重点3：facet_wrap()是一个用于在多个小图中展示子集数据的函数。facet_wrap()可以将一个变量拆分成多个小面板，每个小面板都展示变量的一个子集数据
#重点4：est 模型参数的值
#重点5：主要查看固定其余3种变量水平固定在中位数时，单个变量与结局的暴露反应关系


#结果3：PredictorResponseBivar函数：可视化两个预测变量的双变量暴露-响应函数，其中所有其他预测变量都固定在特定的百分位数。
#PredictorResponseBivarLevels函数：类似地可视化两个预测变量的双变量暴露-响应函数，其中所有其他预测变量都固定在特定的百分位数。
pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)
pred.resp.bivar.levels <- PredictorResponseBivarLevels(pred.resp.df = pred.resp.bivar, Z = Z, qs = c(0.2, 0.6, 0.8,0.9))
pred.resp.bivar.levels
ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")

#重点1：facet_grid 并使用facet_grid()函数将数据按照x和y两个变量进行分组，生成了一个网格图。具体来说，rows = vars(y)表示按照y变量分组，cols = vars(x)表示按照x变量分组。
#重点2：summary(Z)
#重点3：一种变量在另一种变量水平固定在20，60，80和90百分位数时（其余2种变量全部固定在中位数）与结局的剂量反应曲线。曲线假如存在相交，可能存在交互作用.


#结果4：OverallRiskSummaries函数：一个潜在的感兴趣的汇总度量是计算预测变量的整体效果，方法是比较当所有预测变量都处于特定百分位数时与所有预测变量都处于第 50 个百分位数时的 h ℎ 值。 函数 OverallRiskSummaries 允许使用参数 qs 指定一系列分位数值，使用参数 q.fixed 指定固定分位数（默认为第 50 个百分位数）。
risks.overall <- OverallRiskSummaries(fit = fitkm, y = y, Z = Z, X = X, 
                                      qs = seq(0.25, 0.75, by = 0.05), 
                                      q.fixed = 0.5, method = "exact")
risks.overall
ggplot(risks.overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
  geom_pointrange()
#重点1：混合物总体效应与结局的关联图，4种化合物同时固定不同百分位时与其固定在中位数时相比，估计结局的变化，可以看出总体效应与结局呈显著正相关
#重点2：以4种污染物处于中位水平为参照，不同水平下估计改变量的后验均值及其95%置信区间。表明混合物暴露与全因死亡间存在正向联合效应，特别是当水平等于或高于第60百分位时存在显著关联。

#结果5：总结单个预测变量对响应的贡献。 例如，单个预测变量处于第 75 个百分位数时与该预测变量处于第 25 个百分位数时的风险，我们将所有剩余的预测变量固定到一个特定的百分位数。 我们将此称为单一预测健康风险，并且可以使用函数 SingVarRiskSummaries 计算这些风险。 使用 qs.diff 参数指定用于比较风险的两个不同分位数，并且可以使用 q.fixed 参数指定固定剩余污染物的值序列。
risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = y, Z = Z, X = X, 
                                      qs.diff = c(0.25, 0.75), 
                                      q.fixed = c(0.25, 0.50, 0.75,0.8),
                                      method = "exact")
risks.singvar
ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                          ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  coord_flip()

#重点:coord_flip()函数是用来翻转坐标轴的，它会将原先的纵向坐标轴（y轴）变成横向坐标轴（x轴），横向坐标轴（x轴）变成纵向坐标轴（y轴）。
#geom_pointrange()调宽窄

#我们看到预测变量z3和z4对风险没有贡献，z1 和 z2的较高值与h函数的较高值相关联。 此外，该图表明，对于z1，随着其余预测变量的值从第 25 个百分位数增加到第 75 个百分位数，与 z1相关的结果的风险增加。这表明z1和 z2相互作用的可能性。 
#为了使这个概念更正式一点，我们可能希望计算特定的“交互”参数。 例如，当 Z 中的所有其他预测变量都固定在其第 75 个百分位时，我们可以比较单个预测变量的健康风险与当 Z 中的所有其他预测变量都固定在其第 25 个百分位时。 在上图中，这对应于从蓝色圆圈表示的估计值中减去红色圆圈表示的估计值。 这可以使用函数 SingVarIntSummaries 来完成。
risks.int <- SingVarIntSummaries(fit = fitkm, y = y, Z = Z, X = X, 
                                 qs.diff = c(0.25, 0.75), 
                                 qs.fixed = c(0.25, 0.75),
                                 method = "exact")
risks.int

#install.packages("CVEK")
library(CVEK)

kern_par <- data.frame(method = c("linear", "rbf"), 
                       l = rep(1, 2), p = 1:2, stringsAsFactors = FALSE)
kern_func_list <- define_library(kern_par)#定义内核库
#定义反应函数
ZZ<-as.data.frame(Z)
str(ZZ)
str(y)
data<-data.frame(ZZ[,1:3],y,X)
str(data)

formula <- y~ X+ k(z1) + k(z2)
formula_test <- y~ k(z1): k(z2)
#基于指定公式进行交叉验证的核集成。
fit_bos <- cvek(formula, kern_func_list = kern_func_list, data = data, 
                formula_test = formula_test, 
                lambda = exp(seq(-3, 5)), test = "asymp")
#给定拟合对象 (fit_bos)，cvek 测试的 p 值可以提取如下：P<0.05代表存在交互作用
fit_bos$pvalue

formula <- y~ X+ k(z1) + k(z3)
formula_test <- y~ k(z1): k(z3)
#基于指定公式进行交叉验证的核集成。
fit_bos <- cvek(formula, kern_func_list = kern_func_list, data = data, 
                formula_test = formula_test, 
                lambda = exp(seq(-3, 5)), test = "asymp")
#给定拟合对象 (fit_bos)，cvek 测试的 p 值可以提取如下：P<0.05代表存在交互作用
fit_bos$pvalue

