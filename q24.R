library(data.table)
options(scipen=999)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_24',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_24',open="r")
linn <-readLines(conn)
close(conn)

# linn=c(
#   '#E######'
#   ,'#>>.<^<#'
#   ,'#.<..<<#'
#   ,'#>v.><>#'
#   ,'#<^v^^>#'
#   ,'######.#'
# )

map_dt=t(as.matrix(as.data.table(strsplit(linn,split='') )))

nr=nrow(map_dt)-2
nc=ncol(map_dt)-2


u_blizzs=which(map_dt=='^')
d_blizzs=which(map_dt=='v')
l_blizzs=which(map_dt=='<')
r_blizzs=which(map_dt=='>')

get_coords=function(pos,map_dt=map_dt){
  r=nrow(map_dt)-((pos-1)%%nrow(map_dt) +1)+1
  c=(pos-1)%/%nrow(map_dt) +1
  
  r=r-1
  c=c-1
  
  return(paste(c,r,sep='_'))
}

get_indiv=function(coords){
  xs=as.numeric(gsub('(.*)\\_(.*)','\\1',coords) )
  ys=as.numeric(gsub('(.*)\\_(.*)','\\2',coords) )
  return(list(xs=xs,ys=ys))
}


get_possib_next=function(currents,map_dt){
  nr=nrow(map_dt)-2
  nc=ncol(map_dt)-2
  
  get_possib_next_single=function(c){
    x=as.numeric(gsub('(.*)\\_(.*)','\\1',c) )
    y=as.numeric(gsub('(.*)\\_(.*)','\\2',c) )
    
    nexts=c(
      paste0(c(x,y),collapse='_')
      ,paste0(c(x,y)+c(1,0),collapse='_')
      ,paste0(c(x,y)+c(0,1),collapse='_')
      ,paste0(c(x,y)+c(-1,0),collapse='_')
      ,paste0(c(x,y)+c(0,-1),collapse='_')
    )
    
    next_indivs=get_indiv(nexts)
    filter=(next_indivs$xs>0 &next_indivs$xs<=nc&next_indivs$ys>0 &next_indivs$ys<=nr)|
      nexts%in%c(paste0(c(1,nr+1),collapse='_'),paste0(c(nc,0),collapse='_'))
    nexts=nexts[filter]
    
    return(nexts)
  }
  
  out=unique(unlist(lapply(currents,function(x) get_possib_next_single(x))))
  return(out)
}

# get_possib_next(c('1_5','1_4','2_3'),map_dt)


init_coord=paste0(c(1,nr+1),collapse='_')
final_coord=paste0(c(nc,0),collapse='_')

u_co=get_coords(u_blizzs,map_dt)
d_co=get_coords(d_blizzs,map_dt)
l_co=get_coords(l_blizzs,map_dt)
r_co=get_coords(r_blizzs,map_dt)

coords=init_coord
step_counter=0
while(!(final_coord %in% coords)){
  # print(step_counter)
  
  u_co_list=get_indiv(u_co)
  u_co_next=paste0(u_co_list$xs,'_',(u_co_list$ys+1-1)%%nr+1)
  
  d_co_list=get_indiv(d_co)
  d_co_next=paste0(d_co_list$xs,'_',(d_co_list$ys-1-1)%%nr+1)
  
  l_co_list=get_indiv(l_co)
  l_co_next=paste0((l_co_list$xs-1-1)%%nc+1,'_',l_co_list$ys)
  
  r_co_list=get_indiv(r_co)
  r_co_next=paste0((r_co_list$xs+1-1)%%nc+1,'_',r_co_list$ys)
  
  u_co=u_co_next
  d_co=d_co_next
  l_co=l_co_next
  r_co=r_co_next
  
  hurricanes=unique(c(u_co_next,d_co_next,l_co_next,r_co_next))
  coords=get_possib_next(coords,map_dt)
  coords=setdiff(coords,hurricanes)
  
  step_counter=step_counter+1
}
print(paste0('part 1 answer:',step_counter))

coords=final_coord
while(!(init_coord %in% coords)){
  # print(step_counter)
  
  u_co_list=get_indiv(u_co)
  u_co_next=paste0(u_co_list$xs,'_',(u_co_list$ys+1-1)%%nr+1)
  
  d_co_list=get_indiv(d_co)
  d_co_next=paste0(d_co_list$xs,'_',(d_co_list$ys-1-1)%%nr+1)
  
  l_co_list=get_indiv(l_co)
  l_co_next=paste0((l_co_list$xs-1-1)%%nc+1,'_',l_co_list$ys)
  
  r_co_list=get_indiv(r_co)
  r_co_next=paste0((r_co_list$xs+1-1)%%nc+1,'_',r_co_list$ys)
  
  u_co=u_co_next
  d_co=d_co_next
  l_co=l_co_next
  r_co=r_co_next
  
  hurricanes=unique(c(u_co_next,d_co_next,l_co_next,r_co_next))
  coords=get_possib_next(coords,map_dt)
  coords=setdiff(coords,hurricanes)
  
  step_counter=step_counter+1
}
print(step_counter)

coords=init_coord
while(!(final_coord %in% coords)){
  # print(step_counter)
  
  u_co_list=get_indiv(u_co)
  u_co_next=paste0(u_co_list$xs,'_',(u_co_list$ys+1-1)%%nr+1)
  
  d_co_list=get_indiv(d_co)
  d_co_next=paste0(d_co_list$xs,'_',(d_co_list$ys-1-1)%%nr+1)
  
  l_co_list=get_indiv(l_co)
  l_co_next=paste0((l_co_list$xs-1-1)%%nc+1,'_',l_co_list$ys)
  
  r_co_list=get_indiv(r_co)
  r_co_next=paste0((r_co_list$xs+1-1)%%nc+1,'_',r_co_list$ys)
  
  u_co=u_co_next
  d_co=d_co_next
  l_co=l_co_next
  r_co=r_co_next
  
  hurricanes=unique(c(u_co_next,d_co_next,l_co_next,r_co_next))
  coords=get_possib_next(coords,map_dt)
  coords=setdiff(coords,hurricanes)
  
  step_counter=step_counter+1
}
print(paste0('part 2 answer:',step_counter))





