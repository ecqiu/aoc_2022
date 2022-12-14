library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_6',header=F,fill=T,nrows=9,sep='\t')


str2=strsplit(test$V1,split='')[[1]]



for(i in 1:length(str2)){
  print(i)
  chars=str2[i:(i+13)]
  if(length(unique(chars))==14){
    break
  }
}
