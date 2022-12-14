library(data.table)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_11',header=F,fill=T)#,nrows=9,sep='\t'



ops=function(var,index){
  if(index==0){
    out=var*5
  }
  if(index==1){
    out=var*11
  }
  if(index==2){
    out=var+2
  }
  if(index==3){
    out=var+5
  }
  if(index==4){
    out=var^2
  }
  if(index==5){
    out=var+4
  }
  if(index==6){
    out=var+6
  }
  if(index==7){
    out=var+7
  }
  
  return(out)
}

test_mods=c(11,5,19,13,7,17,2,3)
if_trues=c(2,4,5,2,0,7,7,4)+1
if_falses=c(3,0,6,6,3,1,5,1)+1

items=list(
  c(83, 88, 96, 79, 86, 88,  70 ),
  c(59, 63, 98, 85, 68,  72),
  c(90, 79, 97, 52, 90, 94, 71,70),
  c(97, 55,  62),
  c(74, 54, 94,  76),
  c(58),
  c(66,  63),
  c(56, 56, 90, 96,  68)
)


m1_op = m1_op*5
m1_test = m1_test%%11==0
m1_if_true=2
m1_if_false=3

n_inspect=rep(0,8)

n_rounds=20#p1
n_rounds=10000#p2

for(r in 1:n_rounds){
  print(r)
  for(m in 0:7){
    # print(m)
    item_vec=items[[m+1]]
    if(length(item_vec)>0){
    for(i in 1:length(item_vec)){
      # print(i)
      item=item_vec[i]
      item=ops(item,m)
      #item=floor(item/3)#p1
      item=item%%prod(test_mods)#p2
      
      if(item%%test_mods[m+1]==0){
        items[[if_trues[m+1] ]] =c(items[[if_trues[m+1] ]],item)
      }else{
        items[[if_falses[m+1] ]] =c(items[[if_falses[m+1] ]],item)
      }
      
      items[[m+1]]=items[[m+1]][-1]
      n_inspect[m+1]=n_inspect[m+1]+1
    }
    }
    # print(items)
    
  }
  print(n_inspect)
}

prod(sort(n_inspect,decreasing=T)[1:2])