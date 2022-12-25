library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_5',header=F,fill=T,nrows=9,sep='\t')

test=fread('/home/eqiu/code_projects/aoc_2022/data/input_5',header=F,fill=T,skip=9,sep='\t')


# [F]         [L]     [M]            
# [T]     [H] [V] [G] [V]            
# [N]     [T] [D] [R] [N]     [D]    
# [Z]     [B] [C] [P] [B] [R] [Z]    
# [M]     [J] [N] [M] [F] [M] [V] [H]
# [G] [J] [L] [J] [S] [C] [G] [M] [F]
# [H] [W] [V] [P] [W] [H] [H] [N] [N]
# [J] [V] [G] [B] [F] [G] [D] [H] [G]
# 1   2   3   4   5   6   7   8   9 

init_crates=data.table(
  x1='FTNZMGHJ',
  x2='JWV',
  x3='HTBJLVG',
  x4='LVDCNJPB',
  x5='GRPMSWF',
  x6='MVNBFCHG',
  x7='RMGHD',
  x8='DZVMNH',
  x9='HFNG'
)

test[,`:=`(
  n_crates=gsub('move (.*) from (.*) to (.*)','\\1',V1),
  from=gsub('move (.*) from (.*) to (.*)','\\2',V1),
  to=gsub('move (.*) from (.*) to (.*)','\\3',V1)
)]

crate_list=as.list(init_crates)
for(i in 1:nrow(test)){
  print(i)
  row=test[i,]
  
  to_move=substr(crate_list[[as.numeric(row$from)]],1,row$n_crates)
  
  # rev_to_mov=paste(rev(strsplit(to_move,split='')[[1]]),collapse='')#part1
  rev_to_mov=to_move#part2
  
  crate_list[[as.numeric(row$from)]]=substr(crate_list[[as.numeric(row$from)]],as.numeric(row$n_crates)+1,1000)
  crate_list[[as.numeric(row$to)]]=paste0(rev_to_mov,crate_list[[as.numeric(row$to)]])
}
