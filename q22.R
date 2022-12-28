library(data.table)
options(scipen=999)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_22',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_22',open="r")
linn <-readLines(conn)
close(conn)

map_lines=linn[1:200]
instr_lines=linn[202]

# map_lines=c(
#  '        ...#'
# ,'        .#..'
# ,'        #...'
# ,'        ....'
# ,'...#.......#'
# ,'........#...'
# ,'..#....#....'
# ,'..........#.'
# ,'        ...#....'
# ,'        .....#..'
# ,'        .#......'
# ,'        ......#.'
# )
# instr_lines='10R5L5R10L4R5L5'



# map_dt=as.data.table(strsplit(map_lines,split=''),check.rows=FALSE)

map_dt=t(as.matrix(as.data.table(lapply(strsplit(map_lines,split=''),function(x) c(x,rep(' ',max(nchar(map_lines))-length(x)) )))))


str_splitter=function(x){
  out=c()
  index=1
  
  str=''
  while(index <=nchar(x)){
    str=paste0(str,substr(x,index,index))
    if(xor( is.finite(as.numeric(substr(x,index,index))), is.finite(as.numeric(substr(x,index+1,index+1))) ) ){
      out=c(out,str)
      str=''
    }
    index=index+1
  }  
  return(out)
  
}

get_move=function(dir){
  if(dir==0){
    move=c(0,1)
  }
  if(dir==1){
    move=c(1,0)
  }
  if(dir==2){
    move=c(0,-1)
  }
  if(dir==3){
    move=c(-1,0)
  }
  
  return(move)
}
instrs=str_splitter(instr_lines)

#part 1
# move_me=function(pos,n_moves,dir_int,map){
#   dir=get_move(dir_int)
#   
#   #getting vector to move upon
#   if(dir[1]!=0){
#     vec=map[,pos[2]]
#     p=pos[1]
#     d=dir[1]
#     vec_indices=1:nrow(map)
#   }else{
#     vec=map[pos[1],]
#     p=pos[2]
#     d=dir[2]
#     vec_indices=1:ncol(map)
#   }
#   
#   
#   #removing empty spaces for warp around
#   vec_indices=vec_indices[!(vec==' ')]
#   vec=vec[!(vec==' ')]
#   
#   #finding where we are
#   p2=which(vec_indices==p)
#   
#   #finding range where could go
#   range= ((p2+sign(d)):(p2+n_moves*d)-1)%%length(vec)+1
#   
#   #find where we end up, either stopping before wall or at end of path
#   final_p2=pmin(min(which(vec[range]=='#'))-1,length(range))
#   if(final_p2==0){
#     final_index=p
#   }else{
#     final_index=vec_indices[range[final_p2]]
#   }
#   
#   #output final pos
#   if(dir[1]!=0){
#     out=c(final_index,pos[2])
#   }else{
#     out=c(pos[1],final_index)
#   }
#   
#   return(out)
# }
# 
# 
# instrs=str_splitter(instr_lines)
# dir_int=0 #right
# init_pos=c(1,min(which(map_dt[1,]!=' ')))
# 
# pos=init_pos
# for(i in 1:length(instrs)){
#   # if(i==13){
#   #   break
#   # }
#   
#   print(i)
#   print(paste0('pos:',pos))
#   print(paste0('dir_int:',dir_int))
#   inst=instrs[i]
#   print(paste0('inst:',inst))
#   if(inst=='R'){
#     dir_int=(dir_int+1)%%4
#   }else if(inst=='L'){
#     dir_int=(dir_int-1)%%4
#   }else{
#     pos=move_me(pos,as.numeric(inst),dir_int,map=map_dt)
#   }
#   print(pos)
# }
# 
# 1000*pos[1]+4*pos[2]+dir_int

######
#p2

map_dt

# cube=vector("list",6)
# cube[[1]]=map_dt[9:12,9:12]
# cube[[2]]=map_dt[8:5,4:1]
# cube[[3]]=map_dt[1:4,9:12]
# cube[[4]]=map_dt[5:8,9:12]
# cube[[5]]=map_dt[9:12,13:16]
# cube[[6]]=apply(t(map_dt[5:8,5:8]),2,rev)
# 
cubei=vector("list",6)
map_dti=matrix(1:length(map_dt),nrow(map_dt))
map_dti[which(map_dt==' ')]=-1

connections=rep(list(rep(-1,4)),length(map_dt))

index_to_dim=function(index){
  r=(index-1)%%nrow(map_dt) +1
  c=(index-1)%/%nrow(map_dt) +1
  
  return(c(r,c))
}

#easy connections
for(i in 1:length(connections)){
  if(map_dt[i]!=' '){
    dims=index_to_dim(i)
    
    #R
    if(dims[2]<ncol(map_dt) && map_dti[dims[1],dims[2]+1]!=-1 ){
      connections[[i]][1]=map_dti[dims[1],dims[2]+1]
    }
    #D
    if(dims[1]<nrow(map_dt) && map_dti[dims[1]+1,dims[2]]!=-1 ){
      connections[[i]][2]=map_dti[dims[1]+1,dims[2]]
    }
    #L
    if(dims[2]>1 && map_dti[dims[1],dims[2]-1]!=-1 ){
      connections[[i]][3]=map_dti[dims[1],dims[2]-1]
    }
    #U
    if(dims[1]>1 && map_dti[dims[1]-1,dims[2]]!=-1 ){
      connections[[i]][4]=map_dti[dims[1]-1,dims[2]]
    }
  }
}

rotate <- function(x) t(apply(x, 2, rev))
shifter <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, n), head(x, -n))
}

#difficult connections (glue from a corner point outwards both ways
###test
# cubei[[1]]=map_dti[9:12,9:12]
# cubei[[2]]=rotate(rotate(map_dti[5:8,1:4]))
# cubei[[3]]=map_dti[1:4,9:12]
# cubei[[4]]=map_dti[5:8,9:12]
# cubei[[5]]=map_dti[9:12,13:16]
# cubei[[6]]=rotate(rotate(rotate(map_dti[5:8,5:8])))
# 
# for(e in unlist(cubei[[2]])){
#   connections[[e]]=shifter(connections[[e]],2)
# }
# for(e in unlist(cubei[[6]])){
#   connections[[e]]=shifter(connections[[e]],3)
# }
###
cubei[[1]]=map_dti[1:50,51:100]
cubei[[2]]=map_dti[51:100,51:100]
cubei[[3]]=map_dti[101:150,51:100]
cubei[[4]]=rotate(rotate(rotate(map_dti[151:200,1:50])))
cubei[[5]]=map_dti[1:50,101:150]
cubei[[6]]=rotate(rotate(map_dti[101:150,1:50]))


for(e in unlist(cubei[[4]])){
  connections[[e]]=shifter(connections[[e]],3)
}
for(e in unlist(cubei[[6]])){
  connections[[e]]=shifter(connections[[e]],2)
}
#for problem


nr=nrow(cubei[[1]])
nc=ncol(cubei[[1]])

for(i in 1:6){
  for(j in 0:3){
    #specifying the edges to glue
    if(i %in% 1:4 ){
      if(j==1){
        vec=cubei[[i]][nr,]
        join_vec=cubei[[(i)%%4+1]][1,]
      }
      if(j==3){
        vec=cubei[[i]][1,]
        join_vec=cubei[[(i-2)%%4+1]][nr,]
      }
      
      if(j==0){
        vec=cubei[[i]][,nc]
        
        if(i==1){
          join_vec=cubei[[5]][,1]
        }
        if(i==2){
          join_vec=cubei[[5]][nr,]
        }
        if(i==3){
          join_vec=cubei[[5]][,nc]
        }
        if(i==4){
          join_vec=cubei[[5]][1,]
        }
        
        if(i %in%c(3,4)){
          join_vec=rev(join_vec)
        }
      }
      if(j==2){
        vec=cubei[[i]][,1]  
        if(i==1){
          join_vec=cubei[[6]][,nc]
        }
        if(i==2){
          join_vec=cubei[[6]][nr,]
        }
        if(i==3){
          join_vec=cubei[[6]][,1]
        }
        if(i==4){
          join_vec=cubei[[6]][1,]
        }
        
        if(i %in%c(2,3)){
          join_vec=rev(join_vec)
        }
      }
    }
    
    if(i==5){
      if(j==0){
        vec=cubei[[5]][,nc]
        join_vec=cubei[[3]][,nc]
        join_vec=rev(join_vec)
      }
      if(j==1){
        vec=cubei[[5]][nr,]
        join_vec=cubei[[2]][,nc]
        
      }
      if(j==2){
        vec=cubei[[5]][,1]
        join_vec=cubei[[1]][,nc]
      }
      if(j==3){
        vec=cubei[[5]][1,]
        join_vec=cubei[[4]][,nc]
        join_vec=rev(join_vec)
      }
    }
    if(i==6){
      if(j==0){
        vec=cubei[[6]][,nc]
        join_vec=cubei[[1]][,1]
        
      }
      if(j==1){
        vec=cubei[[6]][nr,]
        join_vec=cubei[[2]][,1]
        join_vec=rev(join_vec)
      }
      if(j==2){
        vec=cubei[[6]][,1]
        join_vec=cubei[[3]][,1]
        join_vec=rev(join_vec)
      }
      if(j==3){
        vec=cubei[[6]][1,]
        join_vec=cubei[[4]][,1]
      }
    }
    
    #join em up
    for(k in 1:length(vec)){
      
      # if(connections[[vec[k]]][j+1]!=-1 & connections[[vec[k]]][j+1]!=join_vec[k]){
      #   print(paste0('compare: ',i,'_',j,'_',vec[k]))
      #   print(connections[[vec[k]]][j+1])
      #   print(join_vec[k])
      # }
      
      connections[[vec[k]]][j+1]=join_vec[k]
    }
  }
}

# init_point=97
init_point=10001
init_dir=0
pos=init_point
dir_int=init_dir
path=c(pos)
for(u in 1:length(instrs)){
  print(u)
  print(paste0('pos:',pos))
  print(paste0('dir_int:',dir_int))
  inst=instrs[u]
  print(paste0('inst:',inst))
  # if(u==5){
  #   break
  # }
  
  if(inst=='R'){
    dir_int=(dir_int+1)%%4
  }else if(inst=='L'){
    dir_int=(dir_int-1)%%4
  }else{
    for(v in 1:as.numeric(inst)){
      next_index=connections[[pos]][dir_int+1]
      if(map_dt[next_index]=='#'){
        break
      }
      new_dir_int=which(connections[[next_index]]==pos)-1
      pos=next_index
      dir_int=(new_dir_int+2)%%4
      path=c(path,pos)
    }
  }
  # print(pos)
}

1000*((pos-1)%%nrow(map_dt)+1)+
4*((pos-1)%/%nrow(map_dt)+1)+
(dir_int-2)%%4 #manually unrotate by visually checking which square it ends up and undoing rotations




###############################################################################
# cubei[[1]]=map_dti[9:12,9:12]
# cubei[[2]]=map_dti[8:5,4:1]
# cubei[[3]]=map_dti[1:4,9:12]
# cubei[[4]]=map_dti[5:8,9:12]
# cubei[[5]]=map_dti[9:12,13:16]
# cubei[[6]]=apply(t(map_dti[5:8,5:8]),2,rev)
# 
# move_map=function(pos,dir_int,cube){
#   nc=ncol(cube[[1]])
#   nr=nrow(cube[[1]])
#   # recover()
#   if(pos[3] %in% c(1,2,3,4) & dir_int %in% c(1,3)){
#     cube_change=-(dir_int-2)
#     
#     new_cube=(pos[3]+cube_change-1)%%4+1
#     
#     new_pos=c(ifelse(cube_change==1,1,nrow(cube[[1]])),pos[2],new_cube)
#     new_dir_int=dir_int
#   }else if(pos[3] %in% c(1,2,3,4) & dir_int == 0  |pos[3]==5){
#     if(pos[3] %in% c(1,2,3,4)){
#       # new_dir_int=pos[3]-1
#       
#       if(pos[3]==1){
#         new_dir_int=0
#         new_pos_local=c(pos[1],1)
#       }
#       if(pos[3]==2){
#         new_dir_int=3
#         new_pos_local=c(1,pos[1])
#       }
#       if(pos[3]==3){
#         new_dir_int=2
#         new_pos_local=c(nr-pos[1]+1,nc)
#       }
#       if(pos[3]==4){
#         new_dir_int=1
#         new_pos_local=c(nr,nr-pos[1]+1)
#       }
#       
#       new_pos=c(new_pos_local,5)
#     }else{
#       new_dir_int=2
#       
#       if(dir_int==0){
#         new_cube=3
#         new_pos_local=c(nr-pos[1]+1,nc)
#       }
#       if(dir_int==1){
#         new_cube=2
#         new_pos_local=c(pos[2],nc)
#       }
#       if(dir_int==2){
#         new_cube=1
#         new_pos_local=c(pos[1],nc)
#       }
#       if(dir_int==3){
#         new_cube=4
#         new_pos_local=c(nr-pos[2]+1,nc)
#       }
#       
#       new_pos=c(new_pos_local,new_cube)
#     }
#   }else if(pos[3] %in% c(1,2,3,4) & dir_int == 2  |pos[3]==6){
#     if(pos[3] %in% c(1,2,3,4)){
#       
#       if(pos[3]==1){
#         new_dir_int=2
#         new_pos_local=c(pos[1],nc)
#       }
#       if(pos[3]==2){
#         new_dir_int=1
#         new_pos_local=c(1,nr-pos[1]+1)
#       }
#       if(pos[3]==3){
#         new_dir_int=0
#         new_pos_local=c(nr-pos[1]+1,1)
#       }
#       if(pos[3]==4){
#         new_dir_int=3
#         new_pos_local=c(nr,pos[1])
#       }
#       
#       new_pos=c(new_pos_local,6)
#     }else{
#       new_dir_int=0
#       
#       if(dir_int==0){
#         new_cube=1
#         new_pos_local=c(pos[1],1)
#       }
#       if(dir_int==1){
#         new_cube=4
#         new_pos_local=c(nc-pos[2]+1,1)
#       }
#       if(dir_int==2){
#         new_cube=3
#         new_pos_local=c(nr-pos[1]+1,1)
#       }
#       if(dir_int==3){
#         new_cube=2
#         new_pos_local=c(pos[2],1)
#       }
#       
#       new_pos=c(new_pos_local,new_cube)
#     }
#   }
#   
#   if(cube[[new_pos[3]]][new_pos[1],new_pos[2]]=='#'){
#     return(list(pos=pos,dir_int=dir_int))
#   }else{
#     return(list(pos=new_pos,dir_int=new_dir_int))
#   }
# }
# 
# move_me2=function(posc,n_moves,dir_int,cube){
#   dir=get_move(dir_int)
#   
#   out=posc
#   for(i in 1:n_moves){
#     # print(out)
#     new_pos=out+c(dir,0)
#     if(new_pos[1]>nrow(cube[[1]])|| new_pos[1]<=0|| new_pos[2]>ncol(cube[[1]])|| new_pos[2]<=0 ){#|| map[new_pos[1],new_pos[2]]==''
#       move_change=move_map(out,dir_int,cube)
#       new_pos=move_change$pos
#       dir_int=move_change$dir_int
#       dir=get_move(dir_int)
#       out=new_pos
#     }else if(cube[[new_pos[3]]][new_pos[1],new_pos[2]]=='#'){
#       break;
#     }else{
#       out=new_pos
#     }
#   }
#   return(list(pos=out,dir_int=dir_int))
# }
# 
# #run part 2
# pos=c(1,1,3)
# dir_int=0
# for(i in 1:length(instrs)){
#   # if(i==13){
#   #   break
#   # }
#   
#   print(i)
#   print(paste0('pos:',pos))
#   print(paste0('dir_int:',dir_int))
#   inst=instrs[i]
#   print(paste0('inst:',inst))
#   if(inst=='R'){
#     dir_int=(dir_int+1)%%4
#   }else if(inst=='L'){
#     dir_int=(dir_int-1)%%4
#   }else{
#     out_list=move_me2(pos,n_moves=as.numeric(inst),dir_int,cube)
#     pos=out_list$pos
#     dir_int=out_list$dir_int
#     dir=get_move(dir_int)
#   }
#   # print(pos)
# }
# 
# out_row=(cubei[[pos[3]]][pos[1],pos[2]]-1) %%nrow(map_dt)+1
# out_col=(cubei[[pos[3]]][pos[1],pos[2]]-1) %/%nrow(map_dt)+1
# dir_int
