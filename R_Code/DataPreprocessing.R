##数据模拟
# 设置随机种子，使数据模拟过程可重复
set.seed(12345)
# 定义观测数目
ncust<-1000
# 建立数据框存放模拟观测，初始数据框中只有一列id，即消费者编号
seg_dat<-data.frame(id=as.factor(c(1:ncust)))
# 指定要生成的变量，并为变量命名
vars<-c("age","gender","income","house","store_exp","online_exp","store_trans","online_trans")
# 每个变量对应的数据类型
# norm： 正态分布
# binom: 二项分布
# pois： 泊松分布
vartype<-c("norm","binom","norm","binom","norm","norm","pois","pois")
# 四个消费者分组的名称
group_name<-c("Price","Conspicuous","Quality","Style")
# 各消费者群组的大小
group_size<-c(250,200,200,350)
# group_name和group_size的第一个元素表明，对于“Price”这组消费者，我们将模拟N=250个观测。

# 定义均值矩阵
mus <- matrix( c(
  # 价格敏感（Price）类对应均值
  60, 0.5, 120000,0.9, 500,200,5,2,
  # 炫耀性消费（Conspicuous）类对应均值
  40, 0.7, 200000,0.9, 5000,5000,10,10,
  # 质量（Quality）类对应均值
  36, 0.5, 70000, 0.4, 300, 2000,2,15,
  # 风格（Style）类对应均值
  25, 0.2, 90000, 0.2, 200, 2000,2,20), ncol=length(vars), byrow=TRUE)

# 设置样本的标准差矩阵，只针对正态分布随机样本
sds<- matrix( c(
  # 价格敏感（Price）类对应均值
  3,NA,8000,NA,100,50,NA,NA,
  # 炫耀性消费（Conspicuous）类对应均值
  5,NA,50000,NA,1000,1500,NA,NA,
  # 质量（Quality）类对应均值
  7,NA,10000,NA,50,200,NA,NA,
  # 风格（Style）类对应均值
  2,NA,5000,NA,10,500,NA,NA), ncol=length(vars), byrow=TRUE)

# 抽取描述客户的变量
sim.dat<-NULL
set.seed(2016)
# 对消费者类别进行循环（i）
 for (i in seq_along(group_name)){
   # 打印
   cat (i, group_name[i],"\n")
  # 创建一个空矩阵用于存放该类消费者相关数据
  seg<-data.frame(matrix(NA,nrow=group_size[i], ncol=length(vars)))  
  # 在这个类之内，对不同变量迭代，抽取相应的随机数据
  for (j in seq_along(vars)){
    # 在每个变量上迭代
    if (vartype[j]=="norm"){
      # 抽取正态分布变量
      seg[,j]<-rnorm(group_size[i], mean=mus[i,j], sd=sds[i,j])
    } else if (vartype[j]=="pois") {
      # 抽取泊松分布变量
      seg[,j]<-rpois(group_size[i], lambda=mus[i,j])
    } else if (vartype[j]=="binom"){
      # 抽取二项分布变量
      seg[,j]<-rbinom(group_size[i],size=1,prob=mus[i,j])
    } else{
      # 如果变量类型不是上述几种，程序停止运行并提示信息
      stop ("Don't have type:",vartype[j])
    }        
  }
  # 将该消费者类的数据添加到总数据集
  sim.dat<-rbind(sim.dat,seg)
 }

# summary(sim.dat)
# 指定数据框的列名为我们定义的变量名
names(sim.dat)<-vars
# 将二项变量转化为贴有标签的因子变量
# Female: 女性
# Male: 男性
sim.dat$gender<-factor(sim.dat$gender, labels=c("Female","Male"))
sim.dat$house<-factor(sim.dat$house, labels=c("No","Yes"))
# summary(sim.dat)
# 假设在线购买和在实体店购买的次数至少为1，所以这里在原随机值上加1
sim.dat$store_trans<-sim.dat$store_trans+1
sim.dat$online_trans<-sim.dat$online_trans+1
# 年龄为整数
sim.dat$age<-floor(sim.dat$age)

# 加入缺失值
idxm <- as.logical(rbinom(ncust, size=1, prob=sim.dat$age/200))
sim.dat$income[idxm]<-NA

# 错误输入，离群点
set.seed(123)
idx<-sample(1:ncust,5)
sim.dat$age[idx[1]]<-300
sim.dat$store_exp[idx[2]]<- -500
sim.dat$store_exp[idx[3:5]]<-c(50000,30000,30000)

# table(sim.dat$segment,sim.dat$gender)
# hist(scale(sim.dat$online_exp))

#head(sim.dat)
# 抽取问卷调查回复
# 问卷问题数目
nq<-10
# 各类消费者对问卷回复的正态分布均值矩阵

mus2 <- matrix( c(
  # 价格敏感（Price）类对应均值
 5,2,1,3,1,4.5,1,4.5,2,4.5,
  # 炫耀性消费（Conspicuous）类对应均值
 1,4.5,5,4.5,4.5,4.5,4.5,1,4.5,2,
  # 质量（Quality）类对应均值
 5,2,3,4.5,3,2,4.5,2,3,3,
  # 风格（Style）类对应均值
 3,1,1,2,4.5,1,5,3,4.5,2), ncol=nq, byrow=TRUE)

# 方差假设都是0.2
sd2<-0.2

sim.dat2<-NULL
set.seed(1000)
# 对消费者类别进行循环（i）
for (i in seq_along(group_name)){
  # 打印
  cat (i, group_name[i],"\n")
  # 创建一个空矩阵用于存放该类消费者相关数据
  seg<-data.frame(matrix(NA,nrow=group_size[i], ncol=nq))  
  # 在这个类之内，对不同变量迭代，抽取相应的随机数据
  for (j in 1:nq){
    # 抽取正态分布变量
    res<-rnorm(group_size[i], mean=mus2[i,j], sd=sd2)
    # 设置上下限度
    res[res>5]<-5
    res[res<1]<-1
    # 通过 floor()函数将连续值转化成离散整数。
    seg[,j]<-floor(res)
  }
  # 将该消费者类的数据添加到总数据集
  sim.dat2<-rbind(sim.dat2,seg)
}

# 为数据框添加列标签
names(sim.dat2)<-paste("Q",1:10,sep="")
# 合并两部分数据
sim.dat<-cbind(sim.dat,sim.dat2)
# 加上一个因子列表明每个观测的对应的消费者类别
sim.dat$segment<-factor(rep(group_name,times=group_size))
#summary(sim.dat)
# 将数据储存起来
setwd("/Users/happyrabbit/Documents/GitHub/DataScientistR/Data")
write.csv(sim.dat,"SegData.csv",row.names=F)
###############################################################
# 先建立因子载荷矩阵
# 其中前12项符合双因子结构，因为每项对应一个总体因子载荷和某特定因子的载荷
# 比如购票容易度对应总体因子载荷0.33，对因特定购票因子载荷0.58
# 我们可以将结果评分看成是总体因子和特定因子共同作用的结果

loadings <- matrix(c (
  # 购票体验
  .33, .58, .00, .00,  # 购票容易度 
  .35, .55, .00, .00,  # 座椅选择
  .30, .52, .00, .00,  # 航班选择
  .40, .50, .00, .00,  # 票价
  # 机舱设施
  .50, .00, .55, .00,  # 座椅舒适度
  .41, .00, .51, .00,  # 位置前后空间
  .45, .00, .57, .00,  # 随机行李存放
  .32, .00, .54, .00,  # 机舱清洁
  # 空航服务
  .35, .00, .00, .50,  # 礼貌
  .38, .00, .00, .57,  # 友善
  .60, .00, .00, .50,  # 能够提供需要的帮助
  .52, .00, .00, .58,  # 食物饮料服务
  # 总体指数  
  .43, .10, .30, .30,  # 总体满意度
  .35, .50, .40, .20,  # 再次选择次航空公司
  .25, .50, .50, .20), # 向朋友推荐此航空公司
  nrow=15,ncol=4, byrow=TRUE)
  
# 将载荷矩阵乘以它的转秩，然后将对角线元素设置为1得到相关矩阵
cor_matrix<-loadings %*% t(loadings)
# Diagonal set to ones.
diag(cor_matrix)<-1

# 我们通过mvtnorm包模拟有特定相关矩阵的数据集
library(mvtnorm)
# 设置3个航空公司对应的评分均值向量
mu1=c(5,6,5,6, 7,8,6,7, 5,5,5,5, 6,6,6)
mu2=c(3,3,2,3, 5,4,5,6, 8,8,8,8, 3,3,3)
mu3=c(2,2,2,2, 8,8,8,8, 8,8,8,8, 8,8,8)

#设置随机种子
set.seed(123456) 
# 受访者ID
resp.id <- 1:1000 

library(MASS) 
rating1 <- mvrnorm(length(resp.id),
                     mu=mu1,
                     Sigma=cor_matrix)
rating2 <- mvrnorm(length(resp.id),
                   mu=mu2,
                   Sigma=cor_matrix)
rating3 <- mvrnorm(length(resp.id),
                   mu=mu3,
                   Sigma=cor_matrix)


# 将分值限定在1到9之间
rating1[rating1>9]<-9
rating1[rating1<1]<-1
rating2[rating2>9]<-9
rating2[rating2<1]<-1
rating3[rating3>9]<-9
rating3[rating3<1]<-1

# 将分值转化为整数
rating1<-data.frame(round(rating1,0))
rating2<-data.frame(round(rating2,0))
rating3<-data.frame(round(rating3,0))
rating1$ID<-resp.id
rating2$ID<-resp.id
rating3$ID<-resp.id
rating1$Airline<-rep("AirlineCo.1",length(resp.id))
rating2$Airline<-rep("AirlineCo.2",length(resp.id))
rating3$Airline<-rep("AirlineCo.3",length(resp.id))
rating<-rbind(rating1,rating2,rating3)

# 为数据集的各列命名
names(rating)<-c(
  "Easy_Reservation",
  "Preferred_Seats",
  "Flight_Options",
  "Ticket_Prices",
  "Seat_Comfort",
  "Seat_Roominess",
  "Overhead_Storage",
  "Clean_Aircraft",
  "Courtesy",
  "Friendliness",
  "Helpfulness",
  "Service",
  "Satisfaction",
  "Fly_Again",
  "Recommend",
  "ID",
  "Airline")
setwd("/Users/happyrabbit/Documents/GitHub/DataScientistR/Data")
write.csv(rating,"AirlineRating.csv",row.names=F)
###############################################################
# 先安装这些包才能用library()函数载入
# caret: 提供获取、使用、评估成百上千个机器学习模型及其拟合效果的系统交互界面
# 为机器学习提供了结构化的方法并且对一系列机器学习过程进行评估
library(caret)
# e1071: 各类计量经济和机器学习的延伸；我们通过naiveBayes()函数进行朴素贝叶斯判别
library(e1071)
# gridExtra: 绘图辅助功能，讲不同的图形组合在一起成为图表
library(gridExtra) 
# lattice: 建立在核心绘图能力上的格子框架图形
library(lattice)
# imputeMissings: 填补缺失值
library(imputeMissings)
# RANN: 应用k邻近算法
library(RANN)
# corrplot: 相关矩阵的高级可视化
library(corrplot)
# nnet: 拟合单个潜层级的神经网络模型
library(nnet)
###############################################################
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# summary(sim.dat)
## 数据清理
# 将错误的年龄观测设置为缺失值
sim.dat$age[which(sim.dat$age>100)]<-NA
# 将错误的实体店购买观测设置为缺失值
sim.dat$store_exp[which(sim.dat$store_exp<0)]<-NA
# 通过summary()函数检查清理情况
summary(subset(sim.dat,select=c("age","income")))
###############################################################
## 填补缺失值
# 将填补后的数据存在另外一个数据框中
demo_imp<-impute(sim.dat,method="median/mode")
# 只检查前5列，因为后面没有缺失值
summary(demo_imp[,1:5])
##-------------
## 用preProcess()函数
imp<-preProcess(sim.dat,method="medianImpute")
demo_imp2<-predict(imp,sim.dat)
summary(demo_imp2[,1:5])
###############################################################
# 分位数变换函数
qscale<-function(dat){
  for (i in 1:ncol(dat)){
    up<-quantile(dat[,i],0.99)
    low<-quantile(dat[,i],0.01)
    diff<-up-low
    dat[,i]<-(dat[,i]-low)/diff
  }
  return(dat)
}
###############################################################有偏分布
# 需要使用e1071包中的偏度计算函数skewness()
set.seed(1000)
par(mfrow=c(1,2),oma=c(2,2,2,2))
# 抽取1000个自由度为2的开方分布，右偏分布
x1<-rchisq(1000,2, ncp = 0)
# 通过x1得到对应的左偏分布变量x2
x2<-max(x1)-x1
plot(density(x2),family ="Songti SC",main=paste("左偏，偏度＝",round(skewness(x2),2)), xlab="X2")
plot(density(x1),family ="Songti SC",main=paste("右偏，偏度＝",round(skewness(x1),2)), xlab="X1")
fig_nums <- captioner()
mtext(fig_nums("skew","有偏分布展示"),side = 3,line=0,outer=T,family ="Songti SC")
###############################################################
# 寻找因子变量对应列
idx<-which(lapply(temp,class)=="factor")
###############################################################
library(gpairs)
library(car)
# 选取非问卷调查变量
sdat<-subset(sim.dat,select=c("age","income","store_exp","online_exp","store_trans","online_trans" ))
gpairs(sdat)
scatterplotMatrix(sdat,diagonal="boxplot",smoother=FALSE)

sdat<-impute(sdat)
summary(sdat)
