library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_20',header=F,fill=T)#,nrows=9,sep='\t'
# 
# test=fread(
# '1
# 2
# -3
# 3
# -2
# 0
# 4')




#p1
n_reps=1
key=1

#p2
n_reps=10
key=811589153

#################
test[,V1:=V1*key]
pos=1:length(mixed)

mixed=copy(test$V1)
for(x in 1:(length(mixed)*n_reps)){
  print(x)
  i=(x-1)%%length(mixed)+1
  # print(mixed[i])
  if(i==5016){break}
  
  mode_base=length(mixed)-1
  
  decryp_key=test$V1[i]
  
  modded_test=ifelse(decryp_key>0,decryp_key%%mode_base, decryp_key-mode_base*((decryp_key-1)%/%mode_base+1) )#got blown out on this one for ~3 hours lol, ring buffer moves are modulo N-1, not N... 
  new_node=(pos[i]+modded_test) %%length(mixed)
  # new_pos= ((pos[i]+test$V1[i] -1 ) %%length(test$V1)) +1
  # curr_pos=pos[i]
  # for(j in 1:abs(modded_test)){
  #   next_mode=(curr_pos+sign(modded_test)-1)%%length(mixed) +1
  #   i1=pos==next_mode
  #   i2=pos==curr_pos
  #   pos[i1]=curr_pos
  #   pos[i2]=next_mode
  #   curr_pos=next_mode
  # }
  
  move_dir=sign(modded_test)
  nodes_to_move= ((pos[i]+move_dir):(pos[i]+modded_test) -1) %% length(mixed)+1
  
  pos[pos%in%nodes_to_move]=pos[pos%in%nodes_to_move]-move_dir
  pos[i]=new_node
  pos[pos==0]=length(mixed)
  pos=(pos-1)%%length(mixed)+1

  
  if(length(unique(pos))!=length(mixed)){
    break
  }
  
  # print(length(unique(pos)))
  # print(pos)
  # print(mixed[order(pos)])
}


zero_pos=pos[which(mixed==0)]

mixed[pos==((zero_pos+1000) %%length(mixed))]+
mixed[pos==((zero_pos+2000) %%length(mixed))]+
mixed[pos==((zero_pos+3000) %%length(mixed))]



# which(pos=1000)



# conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_18',open="r")
# linn <-readLines(conn)
# close(conn)
