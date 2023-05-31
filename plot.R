# 加载必要的包
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(svglite)

# 读取并预处理所有的数据
all_data <- list()
# 这里的k值根据你有几个k来自定。比如你画的k=3-5，就把2:8改成3:5
for (k in 2:8) {
  snp_data <- read.table(paste0("./存储路径/snp_rename.", k, ".Q"), header=FALSE)
  colnames(snp_data) <- c("Population", paste0("Ancestry", seq_len(ncol(snp_data)-1)))
  # 创建一个新列来代表同一群体内的个体
  snp_data$Individual <- 1:nrow(snp_data)
  # 将数据转换为长格式
  snp_data_long <- melt(snp_data, id.vars=c("Individual", "Population"))
  snp_data_long$K <- paste0("K=", k)
  all_data[[k-1]] <- snp_data_long
}

# 合并所有的数据
all_data_combined <- do.call(rbind.fill, all_data)

# 这里用来先输出一下绘图的结果
ggplot(all_data_combined, aes(x=Individual, y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  xlab("") + ylab("") + # Remove axis labels
  labs(fill = "") + # Remove legend title
  guides(fill=FALSE) + # Remove legend
  facet_wrap(~K, ncol=1, strip.position = "right") +
  theme(
    axis.text.x = element_blank(), # Remove X axis text
    axis.text.y = element_blank(), # Remove Y axis text
    axis.ticks.y = element_blank(), # Remove Y axis ticks
    strip.background = element_blank(), # Remove strip background
    strip.placement = "outside", # Move strip to the right
    panel.spacing = unit(0, "lines") # Remove space between facets
  )


#  -----------------------------可选分块的开头-------------------------
# 这里我有180个个体6个群体，每个群体有一个名字。这一偏代码是用来修改聚在一起的柱子的对应的群体的名字的。
# 假设每个群体都有30个个体，这里根据你的情况修改
individuals_per_population <- 30
# 计算每个群体的中间位置
population_midpoints <- seq(individuals_per_population / 2, nrow(snp_data), by = individuals_per_population)
# 对应的群体名字
population_names <- c("A", "B", "C", "D", "E", "F")
# 创建标签数据框
label_data <- data.frame(Individual = population_midpoints, Population = population_names, K = "K=2", variable="")

# 使用ggplot2绘图
p <- ggplot(all_data_combined, aes(x=reorder(Individual, Individual), y=value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  xlab("") + ylab("") + # Remove axis labels
  labs(fill = "") + # Remove legend title
  guides(fill=FALSE) + # Remove legend
  facet_wrap(~K, ncol=1, strip.position = "right") +
  theme(
    axis.text.x = element_blank(), # Remove X axis text
    axis.text.y = element_blank(), # Remove Y axis text
    axis.ticks.y = element_blank(), # Remove Y axis ticks
    strip.background = element_blank(), # Remove strip background
    strip.placement = "outside", # Move strip to the right
    panel.spacing = unit(0, "lines"), # Remove space between facets
    plot.margin = unit(c(1,1,1,1), "cm") # Add margins to adjust text position
  ) +
  # 更改字体大小
  theme(text = element_text(size=13)) +
  # 更改文本位置和方向
  geom_text(data = label_data, aes(x = Individual, label = Population, y=1.35, vjust=0.5, group=Population), size=3, angle = 0) # Add population names


#-----------------------------分块开头--------------------------------
# 打印预览一下绘制的图片
print(p)

#下面是几种不同的保存格式
ggsave("ancestry_plot.svg", plot = p, width = 10, height = 10)
ggsave("ancestry_plot.pdf", plot = p, width = 10, height = 10)
ggsave("ancestry_plot.eps", plot = p, width = 10, height = 10)
