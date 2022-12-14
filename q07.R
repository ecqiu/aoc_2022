library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_7',header=F,fill=T)#,nrows=9,sep='\t'

test[,index:=1:.N]

# test=fread(
# "
# $ cd /
#   $ ls
# dir a
# 14848514 b.txt
# 8504156 c.dat
# dir d
# $ cd a
# $ ls
# dir e
# 29116 f
# 2557 g
# 62596 h.lst
# $ cd e
# $ ls
# 584 i
# $ cd ..
# $ cd ..
# $ cd d
# $ ls
# 4060174 j
# 8033020 d.log
# 5626152 d.ext
# 7214296 k
# ",header=F,fill=T)

l1_dirs=c(
  'brhvclj'
  ,'clnvqg'
  ,'dtqtvvrn'
  ,'lcz'
  ,'pcqjncwl'
  ,'qwvfpgl'
  ,'rtmj'
  ,'shg'
  ,'tcdmgwp'
)

dir_chain=c()
# dir_lookup=data.table(dir=unique(c(test$V3,test[V1=='dir',]$V1) ))
dir_lookup[,size:=0]
dir_lookup=data.table(dir='/',size=0 )
allowed_dirs=c()
for(i in 1:nrow(test)){
  print(i)
  r=test[i,]
  
  if(r$V1=='$' & r$V2=='cd'){
    if(r$V3!='..' &r$V3!='/'){
      dir_chain=c(dir_chain,r$V3)
    }else if(r$V3=='/'){
      dir_chain=c('/')
    }else if(r$V3=='..'){
      dir_chain=dir_chain[1:(length(dir_chain)-1)]
    }else{
      print('error')
    }
    
    
    
    # test[i,dir:=dir_chain[length(dir_chain)]]
  }
  test[i,dir_chainx:=paste(dir_chain,collapse='/')]
  # print(dir_chain)
  
  
  if(r$V1!='$' & r$V1!='dir'){
    # if(length(intersect(l1_dirs,dir_chain))>1){
    #   print('error')
    #   print(dir_chain)
    #   break
    # }
    for(j in 1:length(dir_chain) ){
      dir_key=paste(dir_chain[1:j],collapse='_')
      # if(!(x %in% dir_lookup$dir)){print('error')}
      # if(nrow(dir_lookup[dir==x])!=1){print('error')}
      
      if(!(dir_key %in% dir_lookup$dir)){
        dir_lookup=rbind(dir_lookup,data.table(dir=dir_key,size=0))
      }
      dir_lookup[dir==dir_key,size:=size+as.numeric(r$V1)]
    }
  }
}
dir_lookup[size<=100000,sum(size)]

unused_space=70000000-dir_lookup[dir=='/']$size
needed_space=30000000-unused_space
dir_lookup[size>needed_space][order(size)]

dir_lookup[order(size)][1:100]

