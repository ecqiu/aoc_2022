library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_18',header=F,fill=T)#,nrows=9,sep='\t'

# conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_18',open="r")
# linn <-readLines(conn)
# close(conn)

dirs=list(
  c(1,0,0)
  ,c(0,1,0)
  ,c(0,0,1)
  ,c(-1,0,0)
  ,c(0,-1,0)
  ,c(0,0,-1)
)

# test=fread(
# '2,2,2
# 1,2,2
# 3,2,2
# 2,1,2
# 2,3,2
# 2,2,1
# 2,2,3
# 2,2,4
# 2,2,6
# 1,2,5
# 3,2,5
# 2,1,5
# 2,3,5'
# )


# test=fread(
# '1,1,1
# 2,1,1'
# )



#p1
test[,key:=paste0(V1,'_',V2,'_',V3)]

surfaces=c()
for(i in 1:nrow(test)){
  print(i)
  cube_pos=unlist(test[i,1:3])
  surfaces=c(surfaces,setdiff(unlist(lapply(dirs,function(x) paste0(x+cube_pos,collapse='_'))),test$key))
}
length(surfaces)

# length(surfaces)

#p2
uni_surfaces=unique(surfaces)

#find all 'outer' cubes
final_point=c(22,22,22)

init_point=c(-1,-1,-1)
points=c(paste0(init_point,collapse='_'))
new_points=points
while(length(new_points)>0 &!(paste0(final_point,collapse='_') %in% points)){
  print(length(points))
  
  new_points2=c()
  for(c in new_points){
    cube_pos=c(
      as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\1',c))
      ,as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\2',c))
      ,as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\3',c))
    )
    new_points2=c(new_points2, unlist(lapply(dirs,function(x) paste0(x+cube_pos,collapse='_'))) ) 
  }
  
  new_points2=setdiff(new_points2,c(test$key,points))
  
  new_points2_x=as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\1',new_points2))
  new_points2_y=as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\2',new_points2))
  new_points2_z=as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\3',new_points2))
  new_points2=new_points2[new_points2_x>(-2)&new_points2_x<=23&new_points2_y>(-2)&new_points2_y<=23&new_points2_z>(-2)&new_points2_z<=23]
  
  points=unique(c(points,new_points2))
  
  new_points=new_points2
}

#count number of surfaces part of outer cubes
sum(surfaces%in%intersect(surfaces,points))




# rem_list=c()
# for(i in 1:length(uni_surfaces)){
#   cube_pos=c(
#     as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\1',uni_surfaces[i]))
#     ,as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\2',uni_surfaces[i]))
#     ,as.numeric(gsub('(.*)\\_(.*)\\_(.*)','\\3',uni_surfaces[i]))
#   )
#   
#   n1=test[V1==cube_pos[1] &V2==cube_pos[2],.N]
#   n2=test[V3==cube_pos[3] &V2==cube_pos[2],.N]
#   n3=test[V1==cube_pos[1] &V3==cube_pos[3],.N]
#   
#   if(n1*n2*n3>0){
#     rem_list=c(rem_list,uni_surfaces[i])
#   }
# }
# sum(surfaces%in%setdiff(surfaces,rem_list))

