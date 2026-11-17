e1 <- new.env()
e2 <- new.env()

load("pre_model_output_test/pre_model_full_history.RData", envir = e1)
load("pre_model_output_test/pre_model_full_history_200k.RData", envir = e2)

object.size(e1$pre_model_full_history)
object.size(e2$pre_model_full_history)

dim(e1$pre_model_full_history)
dim(e2$pre_model_full_history)

data.frame(
  dataset = c("full_history", "full_history_200k"),
  rows = c(dim(e1$pre_model_full_history)[1], dim(e2$pre_model_full_history)[1]),
  cols = c(dim(e1$pre_model_full_history)[2], dim(e2$pre_model_full_history)[2])
)


load("pre_model_output_test/qual_data_full_history.RData", envir = e1)
load("pre_model_output_test/qual_data_full_history_200k.RData", envir = e2)


object.size(e1$qual_data)
object.size(e2$qual_data)

dim(e1$qual_data)
dim(e2$qual_data)

data.frame(
  dataset = c("full_history", "full_history_200k"),
  rows = c(dim(e1$qual_data)[1], dim(e2$qual_data)[1]),
  cols = c(dim(e1$qual_data)[2], dim(e2$qual_data)[2])
)





# > e1 <- new.env()
# > e2 <- new.env()
# > 
#   > load("pre_model_output_test/pre_model_full_history.RData", envir = e1)
# > load("pre_model_output_test/pre_model_full_history_200k.RData", envir = e2)
# > 
#   > object.size(e1$pre_model_full_history)
# 776250096 bytes
# > object.size(e2$pre_model_full_history)
# 787372968 bytes
# > 
#   > dim(e1$pre_model_full_history)
# [1] 3593709      33
# > dim(e2$pre_model_full_history)
# [1] 3645204      33
# > 
#   > data.frame(
#     +   dataset = c("full_history", "full_history_200k"),
#     +   rows = c(dim(e1$pre_model_full_history)[1], dim(e2$pre_model_full_history)[1]),
#     +   cols = c(dim(e1$pre_model_full_history)[2], dim(e2$pre_model_full_history)[2])
#     + )
# dataset    rows cols
# 1      full_history 3593709   33
# 2 full_history_200k 3645204   33
# > 
#   > 
#   > load("pre_model_output_test/qual_data_full_history.RData", envir = e1)
# > load("pre_model_output_test/qual_data_full_history_200k.RData", envir = e2)
# > 
#   > 
#   > object.size(e1$qual_data)
# 162420104 bytes
# > object.size(e2$qual_data)
# 326567944 bytes
# > 
#   > dim(e1$qual_data)
# [1] 1933501      11
# > dim(e2$qual_data)
# [1] 3887642      11
# > 
#   > data.frame(
#     +   dataset = c("full_history", "full_history_200k"),
#     +   rows = c(dim(e1$qual_data)[1], dim(e2$qual_data)[1]),
#     +   cols = c(dim(e1$qual_data)[2], dim(e2$qual_data)[2])
#     + )
# dataset    rows cols
# 1      full_history 1933501   11
# 2 full_history_200k 3887642   11
# > 