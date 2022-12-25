library(data.table)
library(Rcpp)
sourceCpp('/home/eqiu/code_projects/aoc_2022/q19_rcpp.cpp')
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_19',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_19',open="r")
linn <-readLines(conn)
close(conn)

l=linn[11]


# l='Blueprint 1:Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.'
# l='Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.'


blueprint=data.table(
  orer_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\1',l))
  ,clayr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\2',l))
  ,obsr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\3',l))
  ,obsr_clay=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\4',l))
  ,geor_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\5',l))
  ,geor_obs=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\6',l))
)



# time_left=24
# robot_table=data.table(orer=1,clayr=0,obsr=0,geor=0)
# res_table=data.table(ore=0,clay=0,obs=0,geo=0)
# mem_list<<-list()
# min_calc=function(n_geo_req,blueprint,tl_min1,robots,resources){
#   n_obs_raw=n_geo_req*blueprint$geor_obs-( (tl_min1-1-n_geo_req)*robots$obsr+resources$obs)
#   n_obs_req=pmax(ceiling( (n_obs_raw)/(tl_min1-1-n_geo_req) ),0)
#   if( (tl_min1-1-n_geo_req)<=0){n_obs_req=ifelse(n_obs_raw<=0,0,Inf)}
#   
#   n_clay_raw=n_obs_req*blueprint$obsr_clay-( (tl_min1-2-n_geo_req)*robots$clayr+resources$clay)
#   n_clay_req=pmax(ceiling( (n_clay_raw)/(tl_min1-2-n_geo_req) ),0)
#   if( (tl_min1-2-n_geo_req)<=0){n_clay_req=ifelse(n_clay_raw<=0,0,Inf)}
#   
#   n_ore_raw=(n_geo_req*blueprint$geor_ore+ n_obs_req*blueprint$obsr_ore+n_clay_req*blueprint$clayr_ore) -( (tl_min1-1-n_geo_req)*robots$orer+resources$ore)
#   n_ore_req=pmax(ceiling( (n_ore_raw)/(tl_min1-1-n_geo_req) ),0)
#   if( (tl_min1-1-n_geo_req)<=0){n_ore_req=ifelse(n_ore_raw<=0,0,Inf)}  
#   
#   return(list(n_obs_raw=n_obs_raw,n_obs_req=n_obs_req,n_clay_raw=n_clay_raw,n_clay_req=n_clay_req,n_ore_raw=n_ore_raw,n_ore_req=n_ore_req))
# }
# 
# max_calc=function(n_geo_req,blueprint,tl_min1,robots,resources){
#   n_obs_raw=n_geo_req*blueprint$geor_obs-(robots$obsr+resources$obs)
#   n_obs_req=n_obs_raw
#   
#   n_clay_raw=n_obs_req*blueprint$obsr_clay-(robots$clayr+resources$clay)
#   n_clay_req=n_clay_raw
#   
#   n_ore_raw=(n_geo_req*blueprint$geor_ore+ n_obs_req*blueprint$obsr_ore+n_clay_req*blueprint$clayr_ore) -( robots$orer+resources$ore)
#   n_ore_req=n_ore_raw
#   
#   return(list(n_obs_raw=n_obs_raw,n_obs_req=n_obs_req,n_clay_raw=n_clay_raw,n_clay_req=n_clay_req,n_ore_raw=n_ore_raw,n_ore_req=n_ore_req))
# }
# 
# get_ngeos=function(time_left,robots,resources,blueprint,blueprint_num=1,cutoff=0,maxc=-Inf){
#   if(time_left<=0){
#     # mem_list[[f_key]]=resources$geo
#     return(resources$geo)
#   }
#   
#   #memoise
#   f_key=paste0(time_left,'|',paste0(robots[1,1:4],collapse=','),'|',paste0(resources[1,1:4],collapse=','),"|",blueprint_num)
#   if(f_key %in% names(mem_list)){
#     return(mem_list[[f_key]])#+resources[1,]$geo+robots$geor)
#   }
# 
# 
#   #find no. geo robots
#   tl_min1=time_left
#   n_geo_req=pmin(20,time_left-1)
#   
#   calc_out=min_calc_cpp(n_geo_req,unlist(blueprint),tl_min1,unlist(robots),unlist(resources))
#   calc_out=list(
#           n_obs_raw=calc_out[1],
#           n_obs_req=calc_out[2],
#           n_clay_raw=calc_out[3],
#           n_clay_req=calc_out[4],
#           n_ore_raw=calc_out[4],
#           n_ore_req=calc_out[6]
#         )
#   
#   
#   
#   n_obs_raw=calc_out$n_obs_raw
#   n_obs_req=calc_out$n_obs_req
#   n_clay_raw=calc_out$n_clay_raw
#   n_clay_req=calc_out$n_clay_req
#   n_ore_raw=calc_out$n_ore_raw
#   n_ore_req=calc_out$n_ore_req
#   
# 
#   
#   #find max geo robots constructable
#   while((n_ore_req+n_clay_req+n_obs_req+n_geo_req) >(tl_min1-1) &n_geo_req>0){
#     n_geo_req=n_geo_req-1
#     
#     calc_out=min_calc_cpp(n_geo_req,unlist(blueprint),tl_min1,unlist(robots),unlist(resources))
#     calc_out=list(
#           n_obs_raw=calc_out[1],
#           n_obs_req=calc_out[2],
#           n_clay_raw=calc_out[3],
#           n_clay_req=calc_out[4],
#           n_ore_raw=calc_out[4],
#           n_ore_req=calc_out[6]
#         )
#   
#     n_obs_raw=calc_out$n_obs_raw
#     n_obs_req=calc_out$n_obs_req
#     n_clay_raw=calc_out$n_clay_raw
#     n_clay_req=calc_out$n_clay_req
#     n_ore_raw=calc_out$n_ore_raw
#     n_ore_req=calc_out$n_ore_req
#   }
#   
#   #find min time for constructing robots + number of each robot required to get there
#   if(n_geo_req==0){
#     n_ore_req=0
#     n_obs_req=0
#     n_clay_req=0
#     n_geo_req=0
#   }else{
#     if((n_ore_req+n_clay_req+n_obs_req+n_geo_req) <tl_min1 ){
#       # while((n_ore_req+n_clay_req+n_obs_req+n_geo_req) <tl_min1 ){
#       #   tl_min1=tl_min1-1
#       #   
#       #   
#       #   calc_out=max_calc_cpp(n_geo_req,unlist(blueprint),tl_min1,unlist(robots),unlist(resources))
#       #   calc_out=list(
#       #     n_obs_raw=calc_out[1],
#       #     n_obs_req=calc_out[2],
#       #     n_clay_raw=calc_out[3],
#       #     n_clay_req=calc_out[4],
#       #     n_ore_raw=calc_out[4],
#       #     n_ore_req=calc_out[6]
#       #   )
#       #   n_obs_raw=calc_out$n_obs_raw
#       #   n_obs_req=calc_out$n_obs_req
#       #   n_clay_raw=calc_out$n_clay_raw
#       #   n_clay_req=calc_out$n_clay_req
#       #   n_ore_raw=calc_out$n_ore_raw
#       #   n_ore_req=calc_out$n_ore_req
#       # }
#       # tl_min1=tl_min1+1
#     }
#     calc_out=max_calc_cpp(n_geo_req,unlist(blueprint),tl_min1,unlist(robots),unlist(resources))
#     calc_out=list(
#       n_obs_raw=calc_out[1],
#       n_obs_req=calc_out[2],
#       n_clay_raw=calc_out[3],
#       n_clay_req=calc_out[4],
#       n_ore_raw=calc_out[4],
#       n_ore_req=calc_out[6]
#     )
# 
#     n_obs_raw=calc_out$n_obs_raw
#     n_obs_req=calc_out$n_obs_req
#     n_clay_raw=calc_out$n_clay_raw
#     n_clay_req=calc_out$n_clay_req
#     n_ore_raw=calc_out$n_ore_raw
#     n_ore_req=calc_out$n_ore_req
#   }
#       
#   #early pruning based based on robots/time
#   max_geo=sum((time_left-1):(pmax(time_left-n_geo_req,0)))+robots$geor*time_left+resources$geo
#   if(max_geo<=maxc){
#     # mem_list[[f_key]]=0
#     return(0)
#   }
#   
#   #resource add from robots
#   resources[,`:=`(
#     ore=ore+robots$orer,
#     clay=clay+robots$clayr,
#     obs=obs+robots$obsr,
#     geo=geo+robots$geor
#   )]
#   
# 
#   #fork construct options, if robots not required to get to max geo, dont bother looking
#   possib_geo_out=c()
#   if((resources$ore-robots$orer) >=blueprint$geor_ore& (resources$obs-robots$obsr) >=blueprint$geor_obs &n_geo_req>0 ){
#     copy_robots=copy(robots)
#     copy_robots[,geor:=geor+1]
#     copy_resources=copy(resources)
#     copy_resources[,`:=`(ore=ore-blueprint$geor_ore,obs=obs-blueprint$geor_obs)]
#     possib_geo_out=c(possib_geo_out,get_ngeos(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc) )
#   }
#   maxc=max(c(maxc,possib_geo_out))
#   
#   if((resources$ore-robots$orer) >=blueprint$obsr_ore& (resources$clay-robots$clayr) >=blueprint$obsr_clay  & n_obs_req>0 ){
#     copy_robots=copy(robots)
#     copy_robots[,obsr:=obsr+1]
#     copy_resources=copy(resources)
#     copy_resources[,`:=`(ore=ore-blueprint$obsr_ore,clay=clay-blueprint$obsr_clay)]
#     possib_geo_out=c(possib_geo_out,get_ngeos(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc) )
#   }
#   maxc=max(c(maxc,possib_geo_out))
#   
#   if((resources$ore-robots$orer) >=blueprint$clayr_ore & n_clay_req>0){
#     copy_robots=copy(robots)
#     copy_robots[,clayr:=clayr+1]
#     copy_resources=copy(resources)
#     copy_resources[,ore:=ore-blueprint$clayr_ore]
#     possib_geo_out=c(possib_geo_out,get_ngeos(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc) )
#   }
#   maxc=max(c(maxc,possib_geo_out))
#   
#   if((resources$ore-robots$orer) >=blueprint$orer_ore & n_ore_req>0 ){
#     copy_robots=copy(robots)
#     copy_robots[,orer:=orer+1]
#     copy_resources=copy(resources)
#     copy_resources[,ore:=ore-blueprint$orer_ore]
#     possib_geo_out=c(possib_geo_out,get_ngeos(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc) )
#   }
#   maxc=max(c(maxc,possib_geo_out))
# 
#   #fork null option
#   copy_robots=copy(robots)
#   copy_resources=copy(resources)
#   possib_geo_out=c(possib_geo_out,get_ngeos(time_left-1,copy_robots,copy_resources,blueprint,maxc=maxc))
# 
#   #memoise
#   mem_list[[f_key]]<<-max(possib_geo_out)#-(resources[1,]$geo-robots$geor)-(robots$geor*time_left)
#   return(max(possib_geo_out))
# }

# robot_table=data.table(orer=1,clayr=0,obsr=0,geor=0)
# res_table=data.table(ore=0,clay=0,obs=0,geo=0)

# t0=Sys.time()
# test=get_ngeos(time_left=24,robots=robot_table,resources=res_table,blueprint)
# Sys.time()-t0
# print(test)

# 
# t0=Sys.time()
# time_left=24
# resources=data.table(ore=0,clay=0,obs=0,geo=0)
# robots=data.table(orer=1,clayr=0,obsr=0,geor=0)
# test=q19_cpp(time_left=time_left,robots=unlist(robots),resources=unlist(resources),blueprint=unlist(blueprint), -20);
# Sys.time()-t0
# print(test)

#gave up and used Rcpp... (also had to run some stolen code which solves using Z3 for examples to fix all the bugs)

out_table=data.table(index=1:length(linn),geodes=-1)
for(i in 1:length(linn)){
  print(i)
  time_left=24
  l=linn[i]
  blueprint=data.table(
    orer_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\1',l))
    ,clayr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\2',l))
    ,obsr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\3',l))
    ,obsr_clay=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\4',l))
    ,geor_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\5',l))
    ,geor_obs=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\6',l))
  )

  robot_table=data.table(orer=1,clayr=0,obsr=0,geor=0)
  res_table=data.table(ore=0,clay=0,obs=0,geo=0)
  mem_list<<-list()

  t0=Sys.time()
  test=q19_cpp(time_left=time_left,robots=unlist(robot_table),resources=unlist(res_table),blueprint=unlist(blueprint), -20);
  print(Sys.time()-t0)
  print(test)
  out_table[i,geodes:=test]
}
sum(out_table[,index*geodes])



out_table2=data.table(index=1:3,geodes=-1)
for(i in 1:3){
  print(i)
  time_left=32
  l=linn[i]
  blueprint=data.table(
    orer_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\1',l))
    ,clayr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\2',l))
    ,obsr_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\3',l))
    ,obsr_clay=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\4',l))
    ,geor_ore=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\5',l))
    ,geor_obs=as.numeric(gsub('.* ore robot costs (.*) ore.* costs (.*) ore.* costs (.*) ore and (.*) clay. Each.* (.*) ore and (.*) obsidian.*','\\6',l))
  )

  robot_table=data.table(orer=1,clayr=0,obsr=0,geor=0)
  res_table=data.table(ore=0,clay=0,obs=0,geo=0)
  mem_list<<-list()

  t0=Sys.time()
  test=q19_cpp(time_left=time_left,robots=unlist(robot_table),resources=unlist(res_table),blueprint=unlist(blueprint), -20);
  print(Sys.time()-t0)
  print(test)
  out_table2[i,geodes:=test]
}
prod(out_table2$geodes)

# time_left=21
# robots=data.table(orer=2,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=1,clay=0,obs=0,geo=0)

# mem_list<<-list()
# time_left=22
# robots=data.table(orer=1,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=2,clay=0,obs=0,geo=0)

# time_left=20
# robots=data.table(orer=1,clayr=1,obsr=0,geor=0)
# resources=data.table(ore=2,clay=1,obs=0,geo=0)

# time_left=18
# robots=data.table(orer=1,clayr=2,obsr=0,geor=0)
# resources=data.table(ore=2,clay=4,obs=0,geo=0)
# 
# time_left=14
# robots=data.table(orer=1,clayr=3,obsr=0,geor=0)
# resources=data.table(ore=4,clay=15,obs=0,geo=0)

# time_left=14
# robots=data.table(orer=1,clayr=3,obsr=0,geor=0)
# resources=data.table(ore=4,clay=15,obs=0,geo=0)

# time_left=12
# robots=data.table(orer=1,clayr=4,obsr=1,geor=0)
# resources=data.table(ore=2,clay=7,obs=1,geo=0)

# time_left=11
# robots=data.table(orer=1,clayr=4,obsr=1,geor=0)
# resources=data.table(ore=2,clay=11,obs=2,geo=0)

# time_left=10
# robots=data.table(orer=1,clayr=4,obsr=1,geor=0)
# resources=data.table(ore=3,clay=15,obs=3,geo=0)

# time_left=7
# robots=data.table(orer=1,clayr=4,obsr=2,geor=0)
# resources=data.table(ore=3,clay=13,obs=8,geo=0)

# time_left=4
# robots=data.table(orer=1,clayr=4,obsr=2,geor=1)
# resources=data.table(ore=4,clay=25,obs=7,geo=2)

# time_left=3
# robots=data.table(orer=1,clayr=4,obsr=2,geor=2)
# resources=data.table(ore=3,clay=29,obs=2,geo=3)


# time_left=1
# robots=data.table(orer=1,clayr=4,obsr=2,geor=2)
# resources=data.table(ore=5,clay=37,obs=6,geo=7)


# q19_cpp(time_left=time_left,robots=unlist(robots),resources=unlist(resources),blueprint=unlist(blueprint), -1000);
# get_ngeos(time_left=time_left,robots=robots,resources=resources,blueprint)




# time_left=22
# robots=data.table(orer=1,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=2,clay=0,obs=0,geo=0)

# time_left=21
# robots=data.table(orer=2,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=1,clay=0,obs=0,geo=0)

# time_left=20
# robots=data.table(orer=2,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=3,clay=0,obs=0,geo=0)

# time_left=19
# robots=data.table(orer=3,clayr=0,obsr=0,geor=0)
# resources=data.table(ore=3,clay=0,obs=0,geo=0)

# q19_cpp(time_left=time_left,robots=unlist(robots),resources=unlist(resources),blueprint=unlist(blueprint), -1000);
# mem_list<<-list()
# get_ngeos(time_left=time_left,robots=robots,resources=resources,blueprint)