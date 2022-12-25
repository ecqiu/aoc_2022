library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_9',header=F,fill=T)#,nrows=9,sep='\t'

# test=fread(
# '
# R 4
# U 4
# L 3
# D 1
# R 4
# D 1
# L 5
# R 2
# ',header=F)
# 
# 
# test=fread(
#   '
# R 5
# U 8
# L 8
# D 3
# R 17
# D 10
# L 25
# U 20
# ',header=F)


# sum(test$V2)

#p1
h_pos=c(0,0)
t_pos=c(0,0)
h_list=paste(h_pos,collapse='_')
t_list=paste(t_pos,collapse='_')
for(i in 1:nrow(test)){
  print(i)
  r=test[i,]

  for(j in 1:r$V2){
    print(i)
    if(r$V1=='D'){
      h_pos=h_pos+c(0,-1)
    }
    if(r$V1=='U'){
      h_pos=h_pos+c(0,1)
    }
    if(r$V1=='L'){
      h_pos=h_pos+c(-1,0)
    }
    if(r$V1=='R'){
      h_pos=h_pos+c(1,0)
    }
    t_diff=h_pos-t_pos
    if(max(abs(t_diff))>1){
      t_change=sign(t_diff)*pmin(abs(t_diff),c(1,1))
      t_pos=t_pos+t_change
    }
    t_list=c(t_list,paste(t_pos,collapse='_'))
    h_list=c(h_list,paste(h_pos,collapse='_'))
    print(h_pos)
    print(t_pos)
  }

}
h_list
t_list
length(unique(t_list))

###################################3
#p2
pos_dt=data.table(x=rep(0,10),y=rep(0,10))

h_list=paste(pos_dt[1,],collapse='_')
t_list=paste(pos_dt[10,],collapse='_')
for(i in 1:nrow(test)){
  print(i)
  r=test[i,]
  
  for(j in 1:r$V2){
    # print(i)
    #move head
    if(r$V1=='D'){
      pos_dt[1,]=pos_dt[1,]+c(0,-1)
    }
    if(r$V1=='U'){
      pos_dt[1,]=pos_dt[1,]+c(0,1)
    }
    if(r$V1=='L'){
      pos_dt[1,]=pos_dt[1,]+c(-1,0)
    }
    if(r$V1=='R'){
      pos_dt[1,]=pos_dt[1,]+c(1,0)
    }
    
    #sim later knot movements    
    for(u in 1:9){
      t_diff=as.vector(pos_dt[u,])-as.vector(pos_dt[u+1,])
      if(max(abs(t_diff))>1){
        t_change=sign(t_diff)*pmin(abs(t_diff),c(1,1))
        pos_dt[u+1,]=pos_dt[u+1,]+t_change
      }
    }
    
    #####
    # t_diff=h_pos-t_pos
    # if(max(abs(t_diff))>1){
    #   t_change=sign(t_diff)*pmin(abs(t_diff),c(1,1))
    #   t_pos=t_pos+t_change
    # }
    #####
    #extend list
    t_list=c(t_list,paste(as.vector(pos_dt[10,]),collapse='_'))
    h_list=c(h_list,paste(as.vector(pos_dt[1,]),collapse='_'))
    print(pos_dt[1,])
    print(pos_dt[10,])
  }
  
}
length(unique(t_list))
