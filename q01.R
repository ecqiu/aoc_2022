library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_1')

test[,border:=0]
test[is.na(V1),border:=1]

test[,elf_no:=cumsum(border)]

test[,sum(V1,na.rm=T),by=elf_no][order(-V1)]

test[,sum(V1,na.rm=T),by=elf_no][order(-V1)][1:3][,sum(V1)]
