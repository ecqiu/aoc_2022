library(data.table)
options(scipen=999)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_23',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_23',open="r")
linn <-readLines(conn)
close(conn)

# linn=c(
# '..............'
# ,'..............'
# ,'.......#......'
# ,'.....###.#....'
# ,'...#...#.#....'
# ,'....#...##....'
# ,'...#.###......'
# ,'...##.#.##....'
# ,'....#..#......'
# ,'..............'
# ,'..............'
# ,'..............'
# )

map_dt=t(as.matrix(as.data.table(strsplit(linn,split='') )))


elf_positions=which(map_dt=='#')

r=(elf_positions-1)%%nrow(map_dt) +1
c=(elf_positions-1)%/%nrow(map_dt) +1

elf_pos_list=unlist(lapply(1:length(r),function(x) paste0(c[x],'_',-r[x]) ))



return_dirs=function(pos){
  c(
    paste0(pos+c(0,1),collapse='_')
    ,paste0(pos+c(1,1),collapse='_')
    ,paste0(pos+c(1,0),collapse='_')
    ,paste0(pos+c(1,-1),collapse='_')
    ,paste0(pos+c(0,-1),collapse='_')
    ,paste0(pos+c(-1,-1),collapse='_')
    ,paste0(pos+c(-1,0),collapse='_')
    ,paste0(pos+c(-1,1),collapse='_')
  )
}
return_dirs(c(0,0))

prop_move=function(surrounds,e_pos_list,modulo){
  out=as.character(NA)
  if(sum(surrounds %in% e_pos_list)==0){
  }else{
    for(z in 0:3){
      z_mod=(z+modulo)%%4
      #
      if(z_mod==0){
        if(sum(surrounds[c(1,2,8)] %in% e_pos_list)==0){
          out=surrounds[1]
          break
        }
      }
      if(z_mod==1){
        if(sum(surrounds[c(4,5,6)] %in% e_pos_list)==0){
          out=surrounds[5]
          break
        }
      }
      if(z_mod==2){
        if(sum(surrounds[c(6,7,8)] %in% e_pos_list)==0){
          out=surrounds[7]
          break
        }
      }
      if(z_mod==3){
        if(sum(surrounds[c(2,3,4)] %in% e_pos_list)==0){
          out=surrounds[3]
          break
        }
      }
      #
    }
  }
  
  return(out)
}

mod_int=0
proposed=rep(as.character(NA),length(elf_pos_list))
n_rounds=10000
for(mod_int in 0:(n_rounds-1)){
  print(mod_int)
  # print(elf_pos_list)
  for(i in 1:length(elf_pos_list)){
    e_pos=elf_pos_list[i]
    e_pos_vec=as.numeric(c(gsub('(.*)\\_(.*)','\\1',e_pos),gsub('(.*)\\_(.*)','\\2',e_pos)))
    
    surrounds=return_dirs(e_pos_vec)
    proposed[i]=prop_move(surrounds,e_pos_list=elf_pos_list,mod_int)
  }
  
  dups=proposed[duplicated(proposed)]
  change_filter=!(is.na(proposed)|proposed%in%dups)
  elf_pos_list[change_filter]=proposed[change_filter]
  
  if(mod_int==9){#part 1
    xs=as.numeric(gsub('(.*)\\_(.*)','\\1',elf_pos_list))
    ys=as.numeric(gsub('(.*)\\_(.*)','\\2',elf_pos_list))
    print((max(xs)-min(xs)+1)*(max(ys)-min(ys)+1)-length(elf_pos_list))
  }
  
  if(sum(change_filter)==0){
    break
  }
}
#part 2
print(mod_int+1)

# length(elf_pos_list)
#p1





