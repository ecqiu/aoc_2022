library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_4',header=F)

test[,`:=`(
  v1_a=as.numeric(gsub('(.*)-(.*)','\\1',V1)),  
  v1_b=as.numeric(gsub('(.*)-(.*)','\\2',V1)),
  v2_a=as.numeric(gsub('(.*)-(.*)','\\1',V2)),  
  v2_b=as.numeric(gsub('(.*)-(.*)','\\2',V2))
)]

test[v1_a>=v2_a & v1_a<=v2_b&v1_b>=v2_a & v1_b<=v2_b | (v2_a>=v1_a & v2_a<=v1_b&v2_b>=v1_a & v2_b<=v1_b),.N]

test[v1_a>=v2_a & v1_a<=v2_b|v1_b>=v2_a & v1_b<=v2_b | (v2_a>=v1_a & v2_a<=v1_b|v2_b>=v1_a & v2_b<=v1_b),.N]
