library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_10',header=F,fill=T)#,nrows=9,sep='\t'

# test[!is.na(V2),V1:=paste0(V1,' ',V2)]
# test=fread(
#   'addx 15
# addx -11
# addx 6
# addx -3
# addx 5
# addx -1
# addx -8
# addx 13
# addx 4
# noop
# addx -1
# addx 5
# addx -1
# addx 5
# addx -1
# addx 5
# addx -1
# addx 5
# addx -1
# addx -35
# addx 1
# addx 24
# addx -19
# addx 1
# addx 16
# addx -11
# noop
# noop
# addx 21
# addx -15
# noop
# noop
# addx -3
# addx 9
# addx 1
# addx -3
# addx 8
# addx 1
# addx 5
# noop
# noop
# noop
# noop
# noop
# addx -36
# noop
# addx 1
# addx 7
# noop
# noop
# noop
# addx 2
# addx 6
# noop
# noop
# noop
# noop
# noop
# addx 1
# noop
# noop
# addx 7
# addx 1
# noop
# addx -13
# addx 13
# addx 7
# noop
# addx 1
# addx -33
# noop
# noop
# noop
# addx 2
# noop
# noop
# noop
# addx 8
# noop
# addx -1
# addx 2
# addx 1
# noop
# addx 17
# addx -9
# addx 1
# addx 1
# addx -3
# addx 11
# noop
# noop
# addx 1
# noop
# addx 1
# noop
# noop
# addx -13
# addx -19
# addx 1
# addx 3
# addx 26
# addx -30
# addx 12
# addx -1
# addx 3
# addx 1
# noop
# noop
# noop
# addx -9
# addx 18
# addx 1
# addx 2
# noop
# noop
# addx 9
# noop
# noop
# noop
# addx -1
# addx 2
# addx -37
# addx 1
# addx 3
# noop
# addx 15
# addx -21
# addx 22
# addx -6
# addx 1
# noop
# addx 2
# addx 1
# noop
# addx -10
# noop
# noop
# addx 20
# addx 1
# addx 2
# addx 2
# addx -6
# addx -11
# noop
# noop
# noop
# '
# )


#p1 and p2
x=1
cycle_number=0

out=0
out_vec=rep(0,240)
for(i in 1:nrow(test)){
  op=test$V1[i]
  
  if(op=='noop'){
    x_inc=0
    cycle_inc=1  
  }
  else{
    x_inc=as.numeric(gsub('addx (.*)','\\1',op))
    
    cycle_inc=2  
  }
  
  for(j in 1:cycle_inc){
    cycle_number=cycle_number+1
    
    if(cycle_number%%40==20){
      print(i)
      print(cycle_number)
      print(x)
      out=out+cycle_number*x
    }
    
    if((cycle_number%%40-1) %in% c(x-1,x,x+1) ){
      out_vec[cycle_number]=1
    }
    
    if(j==cycle_inc){
      x=x+x_inc
    }
    
    
  }
    
}
out

t(matrix(out_vec,nrow=40,ncol=6)) #[c(6:1),]


#
#p2
#40w, 6 high(0-39)