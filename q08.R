library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_8',header=F,fill=T)#,nrows=9,sep='\t'

# test=data.table(V1=as.character(c(
#   30373
#   ,25512
#   ,65332
#   ,33549
#   ,35390
# )))


t_matrix=matrix(as.numeric(unlist(strsplit(test$V1,split='')) ),nrow=nrow(test),ncol=nrow(test) )
test2=as.data.table(t_matrix)

#p1
# c_table=c()
# for(i in 1:nrow(test)){
#   
#   last=-Inf
#   str_i=test[i,]$V1
#   counter=0
#   for(j in 1:nchar(str_i)){
#     tree=as.numeric(substr(str_i,j,j))
#     if( tree >last){
#       # counter=counter+1
#       c_table=c(c_table,paste0(i,'_',j) )
#       last=tree
#     }
#   }
#   
#   last2=-Inf
#   counter2=0
#   str_i2=paste(rev(strsplit(str_i,split='')[[1]] ),collapse='')
#   for(j in 1:nchar(str_i2)){
#     tree=as.numeric(substr(str_i2,j,j))
#     if( tree >last2){
#       # counter2=counter2+1
#       c_table=c(c_table,paste0(i,'_',nrow(test)-j+1) )
#       last2=tree
#     }
#   }
#   
#   # c_table[i,`:=`(c1=counter,c2=counter2)]
# }
# 
# 
# 
# 
# for(i in 1:nrow(test2)){
#   last=-Inf
#   str_i=paste(unlist(test2[i,]),collapse='')
#   counter=0
#   for(j in 1:nchar(str_i)){
#     tree=as.numeric(substr(str_i,j,j))
#     if( tree >last){
#       # counter=counter+1
#       c_table=c(c_table,paste0(j,'_',i) )
#       last=tree
#     }
#   }
#   
#   last2=-Inf
#   counter2=0
#   str_i2=paste(rev(strsplit(str_i,split='')[[1]] ),collapse='')
#   for(j in 1:nchar(str_i2)){
#     tree=as.numeric(substr(str_i2,j,j))
#     if( tree >last2){
#       # counter2=counter2+1
#       c_table=c(c_table,paste0(nrow(test)-j+1,'_',i) )
#       last2=tree
#     }
#   }
#   
#   # c_table[i,`:=`(c3=counter,c4=counter2)]
# }
# 
# 
# length(unique(c_table))
##############

max_score=0

for(i in 1:nrow(t_matrix)){
  for(j in 1:ncol(t_matrix)){
    
    base_height=t_matrix[i,j]
    
    l=0
    r=0
    u=0
    d=0
    
    lx=i-l-1
    rx=i+r+1
    ux=j+u+1
    dx=j-d-1
    if(lx<=0|lx>nrow(t_matrix)){
      l_n=Inf
    }else{
      l_n=t_matrix[i-l-1,j]
      l=1
    }
    
    if(rx<=0|rx>nrow(t_matrix)){
      r_n=Inf
    }else{
      r_n=t_matrix[i+r+1,j]
      r=1
    }
    
    if(ux<=0|ux>ncol(t_matrix)){
      u_n=Inf
    }else{
      u_n=t_matrix[i,j+u+1]
      u=1
    }
    
    if(dx<=0|dx>ncol(t_matrix)){
      d_n=Inf
    }else{
      d_n=t_matrix[i,j-d-1]
      d=1
    }
    
    while(l_n<base_height){
      
      ind1=i-l-1
      ind2=j
      
      if(ind1<=0 |ind1>nrow(t_matrix)|ind2<=0 |ind2>ncol(t_matrix)){
        break
      }
      l=l+1
      l_n=t_matrix[ind1,ind2]
      print(l_n)
    }
    
    while(r_n<base_height){
      
      ind1=i+r+1
      ind2=j
      
      if(ind1<=0 |ind1>nrow(t_matrix)|ind2<=0 |ind2>ncol(t_matrix)){
        break
      }
      r=r+1
      r_n=t_matrix[ind1,ind2]
    }
    
    while(d_n<base_height){
      
      ind1=i
      ind2=j-d-1
      
      if(ind1<=0 |ind1>nrow(t_matrix)|ind2<=0 |ind2>ncol(t_matrix)){
        break
      }
      d=d+1
      d_n=t_matrix[ind1,ind2]
    }
    
    while(u_n<base_height){
      
      ind1=i
      ind2=j+u+1
      
      if(ind1<=0 |ind1>nrow(t_matrix)|ind2<=0 |ind2>ncol(t_matrix)){
        break
      }
      u=u+1
      u_n=t_matrix[ind1,ind2]
    }
    score=l*r*u*d
    
    print(paste0(i,'_',j))
    print(c(l,r,u,d))
    print(score)
    
    if(score>max_score){
      max_score=score
    }
    
}
}


# max_score