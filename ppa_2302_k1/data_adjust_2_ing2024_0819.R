library(readxl)
library(ggplot2)
library(gridExtra)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#======================================

vari_set <- 'TP' #to_be_set
time_set <- 3 #to_be_set
data_1 <- read_excel(paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '.xlsx'))
data_1_col <- rep(colnames(data_1)[1:11], 5)
column_pairs <- list()

for (ii in 1:5){
  for(jj in 1:6){
    if(jj == 1){
      x_col <- (ii - 1) * 11 + 1
      y_col <- ii * 11     
    }else if(jj == 2){
      x_col <- (ii - 1) * 11 + 2
      y_col <- ii * 11
    }else if(jj == 3){
      x_col <- (ii - 1) * 11 + 3
      y_col <- ii * 11
    }else if(jj == 4){
      x_col <- (ii - 1) * 11 + 4
      y_col <- ii * 11
    }else if(jj == 5){
      x_col <- (ii - 1) * 11 + 7
      y_col <- ii * 11
    }else{
      x_col <- (ii - 1) * 11 + 1
      y_col <- (ii - 1) * 11 + 4     
    }
    column_pairs[[length(column_pairs) + 1]] <- c(x_col, y_col)
  }
}

#=======================================

plot1_f <- function(f_x, f_y, f_x_label, f_y_label) {
  f_model <- lm(f_y ~ f_x)
  f_equation <- paste0('y = ', round(coef(f_model)[1], 3), ' + ', round(coef(f_model)[2], 3), '*x')
  f_r_squared <- paste0('R² = ', round(summary(f_model)$r.squared, 3))
  
  ggplot(data.frame(f_x, f_y), aes(x = f_x, y = f_y)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    annotate('text', x = min(f_x), y = max(f_y), label = f_equation, hjust = 0, vjust = 1.5, size = 4, color = 'blue') +
    annotate('text', x = min(f_x), y = max(f_y) - (max(f_y) - min(f_y)) * 0.2, label = f_r_squared, hjust = 0, vjust = 1.5, size = 4, color = 'orange') +
    labs(x = f_x_label, y = f_y_label) +
    theme(
      panel.background = element_rect(fill = "white"),  # 设置背景颜色为白色
      plot.background = element_rect(fill = "white"),   # 设置整个图的背景颜色为白色
      panel.grid.major = element_line(color = "grey80"), # 设置主网格线颜色
      panel.grid.minor = element_line(color = "grey90")  # 设置次网格线颜色
    )
}

#=======================================

plots <- list()
for (ii in seq_along(column_pairs)){
  x_col <- data_1[[column_pairs[[ii]][1]]]  # 获取x轴列
  y_col <- data_1[[column_pairs[[ii]][2]]]  # 获取y轴列
  x_label <- data_1_col[column_pairs[[ii]][1]]
  y_label <- data_1_col[column_pairs[[ii]][2]]
  
  plots[[ii]] <- plot1_f(x_col, y_col, x_label, y_label)
}

#=======================================
#组合子图并绘制大图
combined_plot <- grid.arrange(grobs = plots, nrow = 5, ncol = 6)
ggsave(paste0('com_plot_Fig_z2_df_ORI_', vari_set, '_time', time_set, '.jpg'), plot = combined_plot, width = 12, height = 12)