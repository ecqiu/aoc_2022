library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_3',header=F)



test[,`:=`(c1=substr(V1,1,nchar(V1)/2),c2=substr(V1,nchar(V1)/2+1,nchar(V1)))]

c1_str=strsplit(test$c1,split='')
c2_str=strsplit(test$c2,split='')

both_comps=unlist(lapply(1:length(c2_str),function(x) intersect(c1_str[[x]],c2_str[[x]])))

both_comps
myLetters <- c(letters[1:26],toupper(letters[1:26]))
sum(match(both_comps, myLetters))

##
d1_str=strsplit(test$V1[(0:99)*3+1],split='')
d2_str=strsplit(test$V1[(0:99)*3+2],split='')
d3_str=strsplit(test$V1[(0:99)*3+3],split='')

tri_comps=unlist(lapply(1:length(d2_str),function(x) intersect(intersect(d1_str[[x]],d2_str[[x]]),d3_str[[x]])  ))

sum(match(tri_comps, myLetters))

