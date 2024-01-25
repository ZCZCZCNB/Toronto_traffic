#clean data
library(dplyr)
library(lubridate)
library(readr)
# 读取CSV文件
traffic_data <- read_csv("toronto_traffic_10.csv")

# 处理数据的函数
process_traffic_data <- function(traffic_data) {
  traffic_data$count_date <- as.Date(traffic_data$count_date)
  traffic_data$year <- year(traffic_data$count_date)
  traffic_data$month <- month(traffic_data$count_date)
  
  # 筛选2015年至2019年的数据
  filtered_data <- traffic_data %>% 
    filter(year >= 2015, year <= 2019)
  
  
  
  # 按年和月汇总数据
  summary_df <- filtered_data %>%
    group_by(year, month) %>%
    summarise(
      total_cars = sum(sb_cars_r + sb_cars_t + sb_cars_l + nb_cars_r + nb_cars_t + nb_cars_l + wb_cars_r + wb_cars_t + wb_cars_l + eb_cars_r + eb_cars_t + eb_cars_l, na.rm = TRUE),
      total_peds = sum(nx_peds + sx_peds + ex_peds + wx_peds, na.rm = TRUE),
      total_bikes = sum(nx_bike + sx_bike + ex_bike + wx_bike, na.rm = TRUE),
      total_others = sum(nx_other + sx_other + ex_other + wx_other, na.rm = TRUE)
    )
  return(summary_df)
}
# 处理数据
processed_traffic_data <- process_traffic_data(traffic_data)

# 查看处理后的数据
print(head(processed_traffic_data))

write_csv(
  x = processed_traffic_data,
  file = "clean_traffic_data.csv"
)