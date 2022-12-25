library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_16',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_16',open="r")
linn <-readLines(conn)
close(conn)

# linn=c(
#    'Valve AA has flow rate=0; tunnels lead to valves DD, II, BB'
#    ,'Valve BB has flow rate=13; tunnels lead to valves CC, AA'
#    ,'Valve CC has flow rate=2; tunnels lead to valves DD, BB'
#    ,'Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE'
#    ,'Valve EE has flow rate=3; tunnels lead to valves FF, DD'
#    ,'Valve FF has flow rate=0; tunnels lead to valves EE, GG'
#    ,'Valve GG has flow rate=0; tunnels lead to valves FF, HH'
#    ,'Valve HH has flow rate=22; tunnel leads to valve GG'
#    ,'Valve II has flow rate=0; tunnels lead to valves AA, JJ'
#    ,'Valve JJ has flow rate=21; tunnel leads to valve II'
# )


flow_dict=list()

for(l in linn){
   room_val=gsub('Valve (.*) has flow rate=(.*); tunnel(s?) lead(s?) to valve(s?) (.*)','\\1',l)
   flow_rate=as.numeric(gsub('Valve (.*) has flow rate=(.*); tunnel(s?) lead(s?) to valve(s?) (.*)','\\2',l))
   lead_to=trimws(strsplit(gsub('Valve (.*) has flow rate=(.*); tunnel(s?) lead(s?) to valve(s?) (.*)','\\6',l),split=',')[[1]])
   
   flow_dict[[room_val]]=list(flow_rate=flow_rate,lead_to=lead_to)
}

room_dist_list=list()

for(init_room in names(flow_dict)){

   # init_room='AA'
   room_dists=data.table(init_room=init_room,final_room=names(flow_dict),dist=as.numeric(NA))
   d=0
   room_dists[final_room==init_room,dist:=0]
   while(sum(is.na(room_dists$dist))>0 ){
      prev_rooms=room_dists[dist==d]$final_room
      next_rooms=unique(unlist(lapply(flow_dict[prev_rooms],function(x) x$lead_to)))
      
      room_dists[is.na(dist) &final_room %in% next_rooms,dist:=d+1]
      d=d+1
   }
   
   room_dist_list[[init_room]]=room_dists
}

room_dist_stack=rbindlist(room_dist_list)
room_dist_stack[,dist:=dist+1]

# remaining_flows=sort(unlist(lapply(flow_dict[rooms_remaining],function(x) x$flow)),decreasing=T)






rooms_to_visit=names(which(lapply(flow_dict,function(x) x$flow_rate)>0))
search_flows=function(room,time_left,visited=c()){

   # room_order=c()
   flow=0
   if(time_left<=0){
      return(list(flow=flow,vis_order=c()))
   }
   
   visited=c(visited,room)
   flow=flow+flow_dict[[room]]$flow_rate*time_left
   
   if(length(setdiff(rooms_to_visit,visited))==0 ){
      return(list(flow=flow,vis_order=c(room)))
   }
   
   flow_vec=list()
   rooms_remaining=setdiff(rooms_to_visit,visited)
   max_flow=-Inf
   remaining_flows=sort(unlist(lapply(flow_dict[rooms_remaining],function(x) x$flow)),decreasing=T)
   
   # heuristic=remaining_flows[rooms_remaining]*(time_left-room_dist_stack[init_room=='AA'][rooms_remaining,on='final_room']$dist)
   # rooms_remaining=rooms_remaining[order(heuristic,decreasing=T)]
   for(r in rooms_remaining){
      
      tl_after=(time_left-room_dist_stack[init_room==room&final_room==r]$dist)
      
      if(sum(remaining_flows*(tl_after:(tl_after-length(remaining_flows)+1)))<max_flow ){
         next
      }
      
      search_res=search_flows(room=r,time_left=tl_after,visited=visited)
      
      if(search_res$flow>max_flow){
         vis_order=c(room,search_res$vis_order)
         max_flow=search_res$flow
      }
   }
   flow=flow+max_flow
   
   return(list(flow=flow,vis_order=vis_order))
}

#p1 9.2 minutes :/
# t0=Sys.time()
# test_out=search_flows('AA',30,c())
# Sys.time()-t0
# print(test_out)
#########
combos=combn(rooms_to_visit,floor(length(rooms_to_visit)/2) )
max_flow=-Inf
for(i in 1:ncol(combos)){
  print(paste0(i,'/',ncol(combos)))
  vis_a=combos[,i]
  vis_b=setdiff(rooms_to_visit,combos[,i])
  
  out_a=search_flows('AA',26,visited=vis_a)
  out_b=search_flows('AA',26,visited=vis_b)
  
  flow=out_a$flow+out_b$flow
  
  if(flow>max_flow){
    max_flow=flow
  }
}
print(max_flow)

#p2
# search_flows2=function(room,time_left,visited=c()){
#    
#    # room_order=c()
#    flow=0
#    if(time_left<=0){
#       return(list(flow=flow,visited=c()))
#    }
#    
#    visited=c(visited,room)
#    flow=flow+flow_dict[[room]]$flow_rate*time_left
#    
#    if(length(setdiff(rooms_to_visit,visited))==0 ){
#       return(list(flow=flow,visited=c(room)))
#    }
#    
#    flow_vec=list()
#    rooms_remaining=setdiff(rooms_to_visit,visited)
#    max_flow=-Inf
#    remaining_flows=sort(unlist(lapply(flow_dict[rooms_remaining],function(x) x$flow)),decreasing=T)
#    
#    # heuristic=remaining_flows[rooms_remaining]*(time_left-room_dist_stack[init_room=='AA'][rooms_remaining,on='final_room']$dist)
#    # rooms_remaining=rooms_remaining[order(heuristic,decreasing=T)]
#    for(r in rooms_remaining){
#       
#       tl_after=(time_left-room_dist_stack[init_room==room&final_room==r]$dist)
#       
#       if(sum(remaining_flows*(tl_after:(tl_after-length(remaining_flows)+1)))<max_flow ){
#          next
#       }
#       
#       search_res=search_flows(room=r,time_left=tl_after,visited=visited)
#       
#       if(search_res$flow>max_flow){
#          vis_order=c(room,search_res$vis_order)
#          max_flow=search_res$flow
#       }
#    }
#    flow=flow+max_flow
#    
#    return(list(flow=flow,vis_order=vis_order))
# }
# t0=Sys.time()
# test_out=search_flows('AA',26,c())
# Sys.time()-t0
# print(test_out)
