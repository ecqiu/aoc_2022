library(data.table)
library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_12',header=F,fill=T)#,nrows=9,sep='\t'

# test=fread('
# Sabqponm
# abcryxxl
# accszExk
# acctuvwj
# abdefghi
# ',header=F)

# S->E
# (a,z)

base_matrix=as.matrix(as.data.table(strsplit(test$V1,split='')))


# base_matrix=matrix(rep('a',1000),nrow=100,ncol=100)
# base_matrix[1]='S'
# base_matrix[100]='E'

vec2=match(base_matrix,letters)
s=which(base_matrix=='S')
e=which(base_matrix=='E')
vec2[s]=1
vec2[e]=26


base_mat_num=matrix(vec2,nrow=nrow(base_matrix),ncol=ncol(base_matrix) )

ncol(base_matrix)
nrow(base_matrix)


lr_jump_n=ncol(base_matrix)
jump_n=nrow(base_matrix)
# mem=data.table(index=1:length(vec2),dist=as.numeric(NA),path_to=Inf,pathx='')
# mem[index==e,dist:=0]
# 
# iter_find_path=function(start,end,map=vec2,jump_n=nrow(base_matrix),path=c()){
#   
#   if( !is.na(mem[index==start]$dist) && mem[index==start]$path_to <= length(path)   ){
#     return(list(dist=mem[index==start]$dist ) )#,pathx=mem[index==start]$path
#   }
#   
#   if(start==end){
#     return(list(dist=0 ) )#,pathx=as.character(end)
#   }
#   
#   path_so_far=c(path,start)
#   
#   dists=c()
#   
#   # pathxs=c()
#   if( (start%%jump_n)!=0 && abs(map[start+1]- map[start])<=1 && !( (start+1) %in% path_so_far) ){
#     dist=iter_find_path(start+1,end=end,map=map,jump_n=jump_n,path=path_so_far)
#     dists=c(dists,dist$dist)
#     # pathxs=c(pathxs,paste0(start,dist$pathx) )
#   }
#   
#   if( (start%%jump_n) !=1 && abs(map[start-1]- map[start])<=1 && !( (start-1) %in% path_so_far) ){
#     dist=iter_find_path(start-1,end=end,map=map,jump_n=jump_n,path=path_so_far)
#     dists=c(dists,dist$dist)
#     # pathxs=c(pathxs,paste0(start,dist$pathx) )
#   }
#   
#   if( ((start-1)%/%jump_n) !=0 && abs(map[start-jump_n]- map[start])<=1 && !((start-jump_n) %in% path_so_far) ){
#     dist=iter_find_path(start-jump_n,end=end,map=map,jump_n=jump_n,path=path_so_far)
#     dists=c(dists,dist$dist)
#     # pathxs=c(pathxs,paste0(start,dist$pathx) )
#   }
#   
#   if( ((start-1)%/%jump_n) !=(lr_jump_n-1) && abs(map[start+jump_n]- map[start])<=1 && !((start+jump_n) %in% path_so_far) ){
#     dist=iter_find_path(start+jump_n,end=end,map=map,jump_n=jump_n,path=path_so_far)
#     dists=c(dists,dist$dist)
#     # pathxs=c(pathxs,paste0(start,dist$pathx) )
#   }
#   
#   if(length(dists)==0){
#     min_dist=Inf
#     # pathx=''
#   }else{
#     min_dist=min(dists)+1 
#     # pathx=pathxs[which(dists==min_dist)[1] ]
#   }
#   
#   # if(mem[index==start]$dist
#   
#   mem[index==start,`:=`(dist=min_dist,path_to=length(path))]#,pathx=pathx 
#   return(list(dist=min_dist) )#,pathx=pathx
#   
# }
# out=iter_find_path(start=s,end=e)
map=vec2
mem2=data.table(index=1:length(vec2),dist=as.numeric(NA))
mem2[index==e,dist:=0]
d=0
while( nrow(mem2[is.na(dist)])>0){
  print(d)
  n_spreads=mem2[dist==d]
  if(nrow(n_spreads)==0){
    break
  }
  
  print(nrow(n_spreads))
  for(i in 1:nrow(n_spreads)){
   row=n_spreads[i,]
   
   start=row$index
   
   if( (start%%jump_n)!=0 && -(map[start+1]- map[start])<=1 && is.na(mem2[index==(start+1)]$dist) ){
     mem2[index==(start+1),dist:=d+1]
   }
   
   if( (start%%jump_n) !=1 && -(map[start-1]- map[start])<=1 && is.na(mem2[index==(start-1)]$dist) ){
     mem2[index==(start-1),dist:=d+1]
   }
   
   if( ((start-1)%/%jump_n) !=0 && -(map[start-jump_n]- map[start])<=1 &&is.na(mem2[index==(start-jump_n)]$dist) ){
     mem2[index==(start-jump_n),dist:=d+1]
   }
   
   if( ((start-1)%/%jump_n) !=(lr_jump_n-1) && -(map[start+jump_n]- map[start])<=1 &&is.na(mem2[index==(start+jump_n)]$dist) ){
     mem2[index==(start+jump_n),dist:=d+1]
   }
  }
  
  d=d+1
}
mem2[index==s]

mem2[,elev:=vec2]
mem2[elev==1,min(dist,na.rm=T)]

# library(ggplot2)
# qplot(x=1:length(base_mat_num) %%nrow(base_mat_num) ,y=1:length(base_mat_num) %/%nrow(base_mat_num), fill=mem2$dist)+ 
#   geom_tile()

# matrix(mem2$dist,nrow=nrow(base_matrix),ncol=ncol(base_matrix))[145:161,]
# base_mat_num[145:161,]
# base_matrix[145:161,]