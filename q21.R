library(data.table)
options(scipen=999)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_21',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_21',open="r")
linn <-readLines(conn)
close(conn)


#p1
num_base=test[!is.na(as.numeric(V2))]
num_base[,`:=`(char=gsub(':','',V1),val=as.integer64(V2))]
# num_base[,`:=`(char=gsub(':','',V1),val=as.numeric(V2))]
# num_base[char=='humn',val:=Inf]
op_base=test[is.na(as.numeric(V2)),`:=`(join_ind=0)]

out_dt=num_base

while(nrow(op_base)>0){
  print(nrow(op_base))

  op_base[,char:=gsub(':','',V1)]
  
  op_base[num_base,`:=`(val_one=i.val,join_ind=join_ind+1),on=c(V2='char')]
  op_base[num_base,`:=`(val_two=i.val,join_ind=join_ind+1),on=c(V4='char')]
  
  
  new_ops=op_base[join_ind==2]
  
  op_base=op_base[join_ind!=2]
  
  new_ops[V3=='+',val:=val_one+val_two]
  new_ops[V3=='-',val:=val_one-val_two]
  new_ops[V3=='*',val:=val_one*val_two]
  new_ops[V3=='/',val:=val_one/val_two]
  
  if(nrow(new_ops)==0){
    break
  }
  
  num_base=copy(new_ops)
  
  out_dt=rbind(out_dt,new_ops,fill=T,use.names=T)
}

out_dt[char=='root']$val

# out_dt[char=='root']
print(out_dt)

# -(out_dt[char=='root']$val_two-out_dt[char=='root']$val_one)/7

#p2
#recalc solutions a placeholder for humn to see variables that depend on it
num_base=test[!is.na(as.numeric(V2))]
num_base[,`:=`(char=gsub(':','',V1),val=as.integer64(V2))]
num_base[char=='humn',val:=Inf]
op_base=test[is.na(as.numeric(V2)),`:=`(join_ind=0)]

out_dt=num_base

while(nrow(op_base)>0){
  print(nrow(op_base))

  op_base[,char:=gsub(':','',V1)]
  
  op_base[num_base,`:=`(val_one=i.val,join_ind=join_ind+1),on=c(V2='char')]
  op_base[num_base,`:=`(val_two=i.val,join_ind=join_ind+1),on=c(V4='char')]
  
  
  new_ops=op_base[join_ind==2]
  
  op_base=op_base[join_ind!=2]
  
  new_ops[V3=='+',val:=val_one+val_two]
  new_ops[V3=='-',val:=val_one-val_two]
  new_ops[V3=='*',val:=val_one*val_two]
  new_ops[V3=='/',val:=val_one/val_two]
  
  if(nrow(new_ops)==0){
    break
  }
  
  num_base=copy(new_ops)
  
  out_dt=rbind(out_dt,new_ops,fill=T,use.names=T)
}



#build table of variables that depend on humn
dt_lookup=out_dt[char=='humn']
diff=1
while(diff!=0){
  print(nrow(dt_lookup))
  n=nrow(dt_lookup)
  dt_lookup=out_dt[V2%in%dt_lookup$char|V4%in%dt_lookup$char|char %in%dt_lookup$char]
  diff=nrow(dt_lookup)-n
}
print(dt_lookup)


#assigning preparatory variables for inverting calculation
dt_lookup[is.finite(val_one),to_calc_var:=V2]
dt_lookup[is.finite(val_two),to_calc_var:=V4]

dt_lookup[V3=='+',inverse_op:='-']
dt_lookup[V3=='-',inverse_op:='+']
dt_lookup[V3=='*',inverse_op:='/']
dt_lookup[V3=='/',inverse_op:='*']

dt_lookup[,`:=`(val_to_calc=as.integer64(0),val_out=as.integer64(0))]
dt_lookup[70,`:=`(val_to_calc=0,val_out=7012559479583)]

#invert operations from cmmh(==lqcd) back to humn
for(i in (nrow(dt_lookup)-1):1 ){
  dt_lookup[(i),val_to_calc:=dt_lookup[(i+1),val_out]]
  
  #invert operation lokup
  if(dt_lookup$V3[i] %in% c('+','*')){
    if(!is.finite(dt_lookup[i,]$val_two)  ){
      dt_lookup[i,val_out_str:=paste0(val_to_calc,inverse_op,val_one)]
    }else{
      dt_lookup[i,val_out_str:=paste0(val_to_calc,inverse_op,val_two)]
    }
  }else if(dt_lookup$V3[i] %in% c('-','/')){
    if(!is.finite(dt_lookup[i,]$val_two)  ){
      dt_lookup[i,val_out_str:=paste0(val_one,V3,val_to_calc)]
    }else{
      dt_lookup[i,val_out_str:=paste0(val_to_calc,inverse_op,val_two)]
    }
  
  }
  dt_lookup[i,val_out:=eval(parse(text=val_out_str))]
}
dt_lookup[1]$val_to_calc


# 




# dt_lookup[,repl_string:=paste0(V2,V3,V4)]
# dt_lookup[is.finite(val_one),repl_string:=Vectorize(gsub)(V2,val_one,repl_string)]
# dt_lookup[is.finite(val_two),repl_string:=Vectorize(gsub)(V4,val_two,repl_string)]


# for(i in 2:(nrow(dt_lookup)-1) ){
#   dt_lookup[(i+1),repl_string:=gsub(dt_lookup[i,char],paste0('(',dt_lookup[i,repl_string],')'),repl_string)]
# }
# 
# humn=1
# (((255215189589277-((((((409+(((2*((4*(((((((2*(366+(((((10*((((((((((((((((184+(((((5*(926+(((2*((((((((2*(((637+(674+((((437+humn)*9)-659)/2)))/2)-400))-322)/4)+360)*22)+901)/3)-4))-553)/3)))-392)/3)+175)*2))/4)-130)*2)+634)*2)-394)/5)-572)/6)+14)*20)-51)+343)/4)+532))+425)*2)-738)/2)))-655)/3)+400)+658)/3)-976))+394))-165)/3))/2)-807)*3)+726)*2))+495)/8)+7012559479583


#equation 1:
h=k*20734/9-437+659/9


# 
# #p2
# num_base=test[!is.na(as.numeric(V2))]
# num_base[,`:=`(char=gsub(':','',V1),val=as.numeric(V2))]
# num_base[char=='humn',val:=1939]
# 
# op_base=test[is.na(as.numeric(V2))]
# 
# out_dt=num_base
# 
# while(nrow(op_base)>0){
#   print(nrow(op_base))
# 
#   op_base[,char:=gsub(':','',V1)]
#   
#   op_base[num_base,val_one:=i.val,on=c(V2='char')]
#   op_base[num_base,val_two:=i.val,on=c(V4='char')]
#   
#   
#   new_ops=op_base[!is.na(val_one)&!is.na(val_two)]
#   
#   op_base=op_base[!(!is.na(val_one)&!is.na(val_two))]
#   
#   new_ops[V3=='+',val:=val_one+val_two]
#   new_ops[V3=='-',val:=val_one-val_two]
#   new_ops[V3=='*',val:=val_one*val_two]
#   new_ops[V3=='/',val:=val_one/val_two]
#   
#   
#   num_base=copy(new_ops)
#   
#   out_dt=rbind(out_dt,new_ops,fill=T,use.names=T)
# }
# 
# out_dt[char=='root']

