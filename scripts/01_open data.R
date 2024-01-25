git config --global user.name "ZCZCZCNB"
git config --global user.email "chenxutao99@gmail.com"
git config --global --list

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

use_git_config(
  user.name = "ZCZCZCNB",
  user.email = "chenxutao99@gmail.com"
)

install.packages("janitor")
install.packages("knitr")
install.packages("lubridate")
install.packages("opendatatoronto")
install.packages("tidyverse")
library("opendatatoronto")
library("janitor")
library("knitr")
library("lubridate")
library("tidyverse")
 
 
Active_Permits <- list_package_resources("108c2bd1-6945-46f6-af92-02f5658ee7f7")
Active_Permits

traffic <- list_package_resources("traffic-volumes-at-intersections-for-all-modes")
traffic

#### Acquire ####
toronto_traffic <-
  list_package_resources("traffic-volumes-at-intersections-for-all-modes") |>
  filter(name == "raw-data-2020-2029") |>
  get_resource()

write_csv(
  x = toronto_traffic,
  file = "toronto_traffic.csv"
)

head(toronto_traffic)

toronto_traffic_10 <-
  list_package_resources("traffic-volumes-at-intersections-for-all-modes") |>
  filter(name == "raw-data-2010-2019.csv") |>
  get_resource()

write_csv(
  x = toronto_traffic_10,
  file = "toronto_traffic_10.csv"
)

head(toronto_traffic_10)

# 重新调整的R代码

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# 读取并处理数据
traffic_data <- read_csv("toronto_traffic_10.csv")
traffic_data$count_date <- as.Date(traffic_data$count_date)
traffic_data$year <- year(traffic_data$count_date)
traffic_data$month <- month(traffic_data$count_date)

# 筛选2015年至2019年的数据并汇总
processed_data <- traffic_data %>%
  filter(year >= 2015, year <= 2019) %>%
  group_by(year, month) %>%
  summarise(
    total_cars = sum(sb_cars_r + sb_cars_t + sb_cars_l + nb_cars_r + nb_cars_t + nb_cars_l + wb_cars_r + wb_cars_t + wb_cars_l + eb_cars_r + eb_cars_t + eb_cars_l, na.rm = TRUE),
    total_peds = sum(nx_peds + sx_peds + ex_peds + wx_peds, na.rm = TRUE)
  )

# 时间序列图
ggplot(processed_data, aes(x = as.Date(paste(year, month, "1", sep = "-")))) +
  geom_line(aes(y = total_cars, colour = "Cars")) +
  geom_line(aes(y = total_peds, colour = "Pedestrians")) +
  labs(title = "Monthly Traffic Counts (2015-2019)", x = "Date", y = "Count") +
  scale_colour_manual("", values = c("Cars" = "blue", "Pedestrians" = "red"))

# 年度总数条形图
annual_summary <- processed_data %>%
  group_by(year) %>%
  summarise(
    total_cars = sum(total_cars),
    total_peds = sum(total_peds)
  )
annual_summary_long <- pivot_longer(annual_summary, cols = c(total_cars, total_peds), names_to = "Type", values_to = "Count")
ggplot(annual_summary_long, aes(x = as.factor(year), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Annual Total Traffic Counts (2015-2019)", x = "Year", y = "Count")



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


library(ggplot2)




# 使用之前处理的数据：processed_data
# 散点图和线性拟合线
ggplot(processed_data, aes(x = total_cars, y = total_peds)) +
  geom_point(alpha = 0.5) +  # 透明度设置为0.5
  geom_smooth(method = "lm", color = "blue") +  # 添加线性拟合线
  labs(title = "Relationship Between Monthly Car and Pedestrian Counts (2015-2019)",
       x = "Total Cars", y = "Total Pedestrians")
 