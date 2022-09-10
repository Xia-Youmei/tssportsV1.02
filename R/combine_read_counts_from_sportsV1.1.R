# Youmei Xia
# 2022-9-4 - 2022-9-4
# 可实现完全自动化
# a dictionary
#rm(list = ls()) # 清除变量
# getwd() #获取当前的工作目录

#' combine_read_counts_from_sports
#'
#' @param x A folder address.
#'
#' @return 3 files.
#' sports_combined_sample_fragments_counts_matrix_all.txt
#' sports_combined_sample_fragments_counts_matrix_0.5.txt
#' sports_combined_sample_fragments_annotation.txt
#' @export
#'
#' @examples combine_read_counts("E:/life/02Build_R_bao/02learn/add100/examples")
combine_read_counts <- function(x){
setwd(x) #指定工作目录

library(tidyverse) # 管道%>%
library(stringr) # 正则表达式
library(stringi)
library(dplyr)

# 批量读取文件夹中内容
filelist <- list.files(pattern = "*_miRNA.txt$")
collapse_miRNA <- lapply(filelist, function(x) read.table(x, sep = "\t", check.names = F, stringsAsFactors = F, header = T)) # lapply函数 对列表、数据框数据集进行循环,输入为列表;function()指令来命名和创建函数

#--------------1.批量实现截取字符串为文件名-------------
# library(stringr)
filelist <- list.files(pattern = "*.txt$")
cha1 <- c(filelist) # 这是8个字符串，每个字符串里面都包含数字，考虑如何把数字提取出来

col1 <- str_extract_all(cha1, "\\d") # 得到字符串列表，每个元素对应每个字符串的数字，但是不是你想象的那样

# 具体形式是这样的：如23，得到的是"2" "3"，所以该怎样把它变成我们想要的数字23是个问题，解决方法如下：

i <- 1
while (i <= length(col1)) {
  if (length(col1[[i]]) == 0) col1 <- col1[-i] else i <- i + 1 # 这一步是考虑把没有数字的字符串对应的列表元素删掉，比如说"e"
}
col11 <- numeric(length(col1))
for (i in 1:length(col1)) {
  l1 <- length(col1[[i]])
  l11 <- c()
  for (j in 1:l1) {
    l11 <- paste(l11, col1[[i]][j], sep = "")
  } # 将列表的每个元素连接起来，比如"2" "3"就变成了字符串"23"
  col11[i] <- as.numeric(l11) # 再将链接好的字符串进行数值化
}

col11 <- col11[!duplicated(col11)] # 有的数字在处理之后，即将字母去掉之后会有所重复，这步是向量去重处理（不需要去重的请忽略哈）
col11
#----批量处理sports_combined_sample_fragments_counts_matrix_all.txt
count_results <- list()
reone <- list()
for (i in 1:length(collapse_miRNA)) {
  reone[[i]] <- collapse_miRNA[[i]][, c(2, 4)]
  filename <- paste("SRR", col11, sep = "")
  cnames <- c("Sequence", filename[i]) # 列名
  colnames(reone[[i]]) <- cnames # 换列名
}
count_results <- reone %>% reduce(full_join, by = "Sequence") # 列表内数据框都合并
# 使用循环：在数据框中每一个变量间循环，分别判断每列中的na值，并赋值为0
for (i in 1:ncol(count_results)) {
  count_results[, i][is.na(count_results[, i])] <- 0
}
# 保存sports_combined_sample_fragments_counts_matrix_all.txt
write.table(count_results, file = "sports_combined_sample_fragments_counts_matrix_all.txt", sep = "\t", row.names = FALSE, quote = F)
#-------------------第二个文件------------
count_results <- read.table("sports_combined_sample_fragments_counts_matrix_all.txt", sep = "\t", check.names = F, stringsAsFactors = F, header = T) # 算了序号列，所以多加了一列
count_results <- count_results[rowSums(count_results == 0) <= (ncol(count_results) - 1) * 0.5, ] # (列数-1)*0.5,0的样本个数小于0.5# non zero sample number > 0.5
write.table(count_results, file = "sports_combined_sample_fragments_counts_matrix_0.5.txt", sep = "\t", row.names = FALSE, quote = F)
#-------------------第三个文件------------
count_results_ann <- list()
for (i in 1:length(collapse_miRNA)) {
  count_results_ann[[i]] <- collapse_miRNA[[i]][, c(2, 3, 5, 6)]
  cnames <- c("Fragment", "Length", "Match_Genome", "Annotation")
  colnames(count_results_ann[[i]]) <- cnames
}
count_results <- count_results_ann %>% reduce(full_join, by = c("Fragment", "Length", "Match_Genome", "Annotation")) # 列表内数据框一样的都合并，不一样的保留
write.table(count_results, file = "sports_combined_sample_fragments_annotation.txt", sep = "\t", row.names = FALSE, quote = F)

}
