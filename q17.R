library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_17',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_17',open="r")
linn <-readLines(conn)
close(conn)

# linn='>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>'

shapes=list(
data.table(x=0:3,y=rep(0,4))
,data.table(x=c(1,0,1,2,1),y=c(2,1,1,1,0) )
,data.table(x=c(0,1,2,2,2),y=c(0,0,0,1,2))
,data.table(x=rep(0,4),y=c(0,1,2,3))
,data.table(x=c(0,0,1,1),y=c(0,1,0,1))
)

base_filled=data.table(x=1:7,y=0)

shape_num=1
run_index=1
run_mod=1
shape_mod=1
recorder=data.table(shape_num=0,run_index=0,height=0)
record_list=list()
while(shape_num<=3500){
  record_list[[shape_num]]=data.table(shape_num=shape_num,run_index=run_index,shape_mod=shape_mod,run_mod=run_mod,height=max(base_filled$y))
  print(shape_num)
  shape_mod=shape_num%%5
  if(shape_mod==0){shape_mod=5}
  
  shape=copy(shapes[[shape_mod]])
  start_y=max(base_filled$y)+4
  start_x=3
  
  shape[,`:=`(x=x+start_x,y=y+start_y)]
  
  stopped=F
  next_shape=copy(shape)
  while((!stopped)){
    run_mod=run_index%%nchar(linn)
    if(run_mod==0){run_mod=nchar(linn)}
    run_index=run_index+1
    
    dir=substr(linn,run_mod,run_mod)
    if(dir=='<'){dir_change=-1}
    if(dir=='>'){dir_change=1}
    
    next_shape[,x:=x+dir_change]
    if(max(next_shape$x)>7 |min(next_shape$x)<1 |length(intersect(base_filled[,paste0(x,'_',y)],next_shape[,paste0(x,'_',y)]))>0 ){
      next_shape[,x:=x-dir_change]
    }
    
    next_shape=next_shape[,y:=y-1]
    if(length(intersect(base_filled[,paste0(x,'_',y)],next_shape[,paste0(x,'_',y)]))>0 ){
      next_shape[,y:=y+1]
      base_filled=rbindlist(list(base_filled,next_shape))
      stopped=T
    }
  }
  shape_num=shape_num+1
  print(run_index)
}
max(base_filled$y)
record_stack=rbindlist(record_list)


#p2 - R seems to be unstable at dividing with numberes this large, realistically need to use 64bit to get consistent answer..
use=record_stack[,.N,by=.(run_mod,shape_mod)][N>1,][15,]
dt_mod=record_stack[run_mod==use$run_mod&shape_mod==use$shape_mod]

period=dt_mod[2,]$shape_num-dt_mod[1,]$shape_num
height_gain=dt_mod[2,]$height-dt_mod[1,]$height

# (1000000000000-dt_mod[1,]$shape_num+1)%/%period
# floor((1000000000000-dt_mod[1,]$shape_num+1)/period)

dt_mod[1,]$shape_num
height_gain*((1000000000000-dt_mod[1,]$shape_num+1)%/%period)

dt_mod[1,]$height+height_gain*((1000000000000-dt_mod[1,]$shape_num+1)%/%period)+record_stack[shape_num==(period+dt_mod[1,]$shape_num+(1000000000000-dt_mod[1,]$shape_num+1)%%period)   ]$height-record_stack[shape_num==(period+dt_mod[1,]$shape_num)   ]$height



# height_gain*(1000000000000-dt_mod[1,]$shape_num)%/%period+record_stack[shape_num== (((1000000000000-dt_mod[1,]$shape_num)%%period)+6) ]$height
# record_stack[shape_num== ((1000000000000-dt_mod[1,]$shape_num+1)%%period + period) ]$height-height_gain
# record_stack[shape_num== ((1000000000000-dt_mod[1,]$shape_num+1)%%period ) ]$height





1516860465101

record_stack=rbindlist(record_list)
# test=matrix(0,10,10)
# test[c(1,2),]