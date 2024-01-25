set.seed(123)  # 设置随机种子以获得可重复的结果

# 生成年份和月份
years <- rep(2015:2019, each = 12)  # 重复每年12个月
months <- rep(1:12, times = 5)      # 每个月重复5次（对应5年）

# 生成随机的行人和汽车数量
total_peds <- sample(1000:5000, length(years), replace = TRUE) # 行人数量
total_cars <- sample(5000:20000, length(years), replace = TRUE) # 汽车数量

# 创建数据框
simulated_data <- data.frame(year = years, month = months, total_peds = total_peds, total_cars = total_cars)

# 查看前几行数据
head(simulated_data) 
 