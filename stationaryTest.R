# 代码8-1

# 设置工作目录并读取数据
setwd("F:/第5章/01-任务程序")
dataFile <- data.frame(data.table::fread('./data/user_balance_table.csv'))  # 快速读取数据

# 日期转化为标准时间格式
dataFile$report_date_new <- as.Date(as.character(dataFile$report_date), '%Y%m%d')

# 创建一个新的数据集，对相同日期的资金申购量进行统计
library(plyr)
data.purchase <- ddply(dataFile, .(report_date_new), summarize, 
                      purchase = sum(total_purchase_amt))

# 绘制时序图
purchase <- ts(data.purchase$purchase, start = c(2013, 7), 
               end = c(2014, 8), frequency = 365)
plot(purchase, type="l", ylab = "资金申购量", xlab = "date", xaxt = "n")



# 代码8-2

acf(purchase, lag.max = 30)  # 数据自相关检验



# 代码8-3

# 筛选2014-03-01至2014-07-31的数据作为训练集，2014—08数据作为测试集
index <- which(data.purchase$report_date_new >= as.Date("2014-03-01") &
                 data.purchase$report_date_new <= as.Date("2014-07-31"))
halcyon.purchase <- ts(data.purchase$purchase[index])
plot(halcyon.purchase, ylab = "资金申购量", xlab = "date", xaxt = "n")  # 绘制时序图
acf(halcyon.purchase, lag.max = 30)  # 自相关性检验



# 代码8-4

# 差分
purchase.diff <- diff(halcyon.purchase, lag = 7, differences = 1)
plot(purchase.diff, ylab = '差分后的资金申购量', xlab = 'date', xaxt = "n")  # 绘制时序图
acf(purchase.diff, lag.max = 30)  # 自相关性检验


library(quadprog)
# 代码8-5

# 单位根检验
library(tseries)
adf.test(purchase.diff)

