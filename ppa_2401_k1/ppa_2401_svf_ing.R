# 安装并加载必要的包

library(gstat)
library(sp)
library(sf)
library(tmap)

# 设置随机种子以保证结果可重现
set.seed(123)

# 生成150个随机站点的数据
locations <- data.frame(
  lon = runif(150, min = -10, max = 10),   # 经度在-10到10之间
  lat = runif(150, min = -10, max = 10),   # 纬度在-10到10之间
  temperature = rnorm(150, mean = 15, sd = 2)  # 温度均值为15，标准差为2
)

# 转换为sf对象
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

# 创建空间数据框
coordinates(locations) <- ~lon+lat

# 计算半变异函数
variogram <- variogram(temperature ~ 1, data = locations)

# 拟合半变异函数模型
fit_variogram <- fit.variogram(variogram, model = vgm("Sph"))

# 绘制半变异函数及其拟合曲线
plot(variogram, fit_variogram, main = "Semivariogram with Fitted Model")

# 阈值（Range）
range_value <- fit_variogram$range[2]
cat("The range value (threshold) is:", range_value, "\n")
