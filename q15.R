library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_15',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_15',open="r")
linn <-readLines(conn)
close(conn)

blocked=c()
beacons=c()
# linn=c(
# 'Sensor at x=2, y=18: closest beacon is at x=-2, y=15'
# ,'Sensor at x=9, y=16: closest beacon is at x=10, y=16'
# ,'Sensor at x=13, y=2: closest beacon is at x=15, y=3'
# ,'Sensor at x=12, y=14: closest beacon is at x=10, y=16'
# ,'Sensor at x=10, y=20: closest beacon is at x=10, y=16'
# ,'Sensor at x=14, y=17: closest beacon is at x=10, y=16'
# ,'Sensor at x=8, y=7: closest beacon is at x=2, y=10'
# ,'Sensor at x=2, y=0: closest beacon is at x=2, y=10'
# ,'Sensor at x=0, y=11: closest beacon is at x=2, y=10'
# ,'Sensor at x=20, y=14: closest beacon is at x=25, y=17'
# ,'Sensor at x=17, y=20: closest beacon is at x=21, y=22'
# ,'Sensor at x=16, y=7: closest beacon is at x=15, y=3'
# ,'Sensor at x=14, y=3: closest beacon is at x=15, y=3'
# ,'Sensor at x=20, y=1: closest beacon is at x=15, y=3'
# )
blockline=2000000
# blockline=10
dist_vec=c()
s_x=c()
s_y=c()
for(l in linn){
   print(length(blocked))
   s=c(
      as.numeric(gsub('.*x=([-[:digit:]]+).*closest.*','\\1',l))
      ,as.numeric(gsub('.*y=([-[:digit:]]+).*closest.*','\\1',l))
   )
   
   b=c(
      as.numeric(gsub('.*closest.*x=([-[:digit:]]+).*','\\1',l))
      ,as.numeric(gsub('.*closest.*y=([-[:digit:]]+).*','\\1',l))
   )
   
   # print(s)
   # print(b)
   
   d=sum(abs(s-b))
   dist_vec=c(dist_vec,d)
   s_x=c(s_x,s[1])
   s_y=c(s_y,s[2])
   
   if(b[2]==blockline){
      beacons=c(beacons,b[1])
   }
   
   if( d >= abs(blockline-s[2])){
      max_dist=d-abs(blockline-s[2])
      blocked=c(blocked, (s[1]-max_dist):(s[1]+max_dist))
      blocked=unique(blocked)
   }
}

blocked=unique(blocked)
print(length(setdiff(blocked,beacons)))

############################################

# max_val=20
max_val=4000000

#if only 1 point must be on the edge of a beacon circle
possibs_x=c()
possibs_y=c()
possibs=vector('list',length(dist_vec))
dist_vec2=dist_vec+1
for(i in 1:length(dist_vec)){
   print(i)
   possibs_x=c(
      (s_x[i]-dist_vec2[i]):(s_x[i]+dist_vec2[i]-1),
      (s_x[i]+dist_vec2[i]):(s_x[i]-dist_vec2[i]+1)
   )
   possibs_y=c(
      s_y[i]:(s_y[i]+dist_vec2[i]),(s_y[i]+dist_vec2[i]-1):(s_y[i]+1),
      s_y[i]:(s_y[i]-dist_vec2[i]),(s_y[i]-dist_vec2[i]+1):(s_y[i]-1)
   )
   # print(length(possibs_x))
   # print(length(possibs_y))
   
   filter=possibs_x>=0&possibs_x<=max_val&possibs_y>=0&possibs_y<=max_val
   possibs_x=possibs_x[filter]
   possibs_y=possibs_y[filter]
   
   
   possibs[[i]]=unique(paste0(possibs_x,'_',possibs_y))
}
# print(possibs)

#also assume needs to be on the edge of two beacon circles (otherwise is in a corner)
break_flag=F
answers=c()
for(a in 1:length(dist_vec)){
   print(a)
   for(b in (a+1):length(dist_vec)){
      answer=intersect(possibs[[a]],possibs[[b]])
      answers=unique(c(answers,answer))
      # if(length(answer)==1){
      #    print(answer)
      #    break_flag=T
      #    break
      # }
      
   }
   if(break_flag){
      break
   }
}
saveRDS(answers,'/home/eqiu/code_projects/aoc_2022/data/q15_possibs.rds')
answers=readRDS('/home/eqiu/code_projects/aoc_2022/data/q15_possibs.rds')

#filted reduced list by beacons
a_x=as.numeric(gsub('(.*)\\_(.*)','\\1',answers))
a_y=as.numeric(gsub('(.*)\\_(.*)','\\2',answers))
for(l in linn){
   print(l)

   s=c(
      as.numeric(gsub('.*x=([-[:digit:]]+).*closest.*','\\1',l))
      ,as.numeric(gsub('.*y=([-[:digit:]]+).*closest.*','\\1',l))
   )
   b=c(
      as.numeric(gsub('.*closest.*x=([-[:digit:]]+).*','\\1',l))
      ,as.numeric(gsub('.*closest.*y=([-[:digit:]]+).*','\\1',l))
   )


   d=sum(abs(s-b))

   # exclude_index=c()
   # for(j in 1:length(answers)){
      # p_x=a_x[j]
      # p_y=a_y[j]

      # if(sum(abs(c(p_x,p_y)-s))<=d ){
      #    exclude_index=c(exclude_index,j)
      # }
   exclude_index= unique(c(exclude_index,which( (abs(a_x-s[1])+abs(a_y-s[2])) <=d)))

   # }
   print(length(exclude_index))
   # if(length(exclude_index)>0){
      
   # }
   
}

answers=answers[-exclude_index]


format(as.numeric(gsub('(.*)\\_(.*)','\\1',answers))*4000000+
   as.numeric(gsub('(.*)\\_(.*)','\\2',answers)),digits=13)

# if(length(possibs_x)>0){
#    possibs=unique(c(possibs,paste0(possibs_x,'_',possibs_y)))
#    break
# }

# dist_vec
# s_x
# s_y


#####
# blockline=2000000
# blines=0:4000000
# blocked=vector('list',4000001)
# for(l in linn){
#    print(l)
#    # print(length(blocked))
#    s=c(
#       as.numeric(gsub('.*x=([-[:digit:]]+).*closest.*','\\1',l))
#       ,as.numeric(gsub('.*y=([-[:digit:]]+).*closest.*','\\1',l))
#    )
#    
#    b=c(
#       as.numeric(gsub('.*closest.*x=([-[:digit:]]+).*','\\1',l))
#       ,as.numeric(gsub('.*closest.*y=([-[:digit:]]+).*','\\1',l))
#    )
#    
#    print(s)
#    print(b)
#    
#    d=sum(abs(s-b))
#    
#    # if(b[2]==blockline){
#    #    beacons=c(beacons,b[1])
#    # }
#    
#    for(blockline in blines){
#       if( d >= abs(blockline-s[2])){
#          max_dist=d-abs(blockline-s[2])
#          blocked[[blockline+1]]=c(blocked[[blockline+1]], (s[1]-max_dist):(s[1]+max_dist))
#          blocked[[blockline+1]]=unique(blocked[[blockline+1]])
#       }
#    }
# }