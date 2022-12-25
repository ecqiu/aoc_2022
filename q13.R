library(data.table)
library(jsonlite)
# library(microbenchmark)
# test=fread('/home/eqiu/code_projects/aoc_2022/data/input_13',header=F,fill=T)#,nrows=9,sep='\t'


conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_13',open="r")
linn <-readLines(conn)

pair_input=linn[1:3]

pair_a=pair_input[1]
pair_b=pair_input[2]


# fromJSON(linn[1],simplifyVector=F)

# pair_a[comma_indices]

pair_to_list=function(pair){
   
   if( !is.na(as.numeric(pair)) ){
      return(as.numeric(pair))
   }
   if( pair=='[]'){
      return(list())
   }
   
   split_pair=strsplit(pair,'')[[1]]
   comma_indices=which(split_pair==',')   
   comma_indices=c(comma_indices,length(split_pair))
   last_index=1
   
   outputs=c()
   
   for(i in comma_indices){
      entry=split_pair[(last_index+1):(i-1)]
      if(sum(entry=='[')==sum(entry==']')){
         outputs=c(outputs,paste0(split_pair[(last_index+1):(i-1)],collapse=''))
         last_index=i
      }
   }
   
   
   out=list()
   for(i in 1:length(outputs)){
      out[[i]]=pair_to_list(outputs[[i]])
   }
   
   return(out)
}

compare_pairs=function(a,b){
   if(is.numeric(a) & is.numeric(b)){
      if(a<b){return('True')}
      if(a>b){return('False')}
      if(a==b){return('Continue')}
   }else{ 
      if(is.list(a) & !is.list(b)){
         b=list(b)
      }
      if(!is.list(a) & is.list(b)){
         a=list(a)
      }
      
      if(length(a)==0){
         if(length(b)==0){
            return('Continue')
         }else{
            return('True')
         }
      }
      
      for(i in 1:length(a)){
         if(length(b)<i){
            return('False')
         }
         val=compare_pairs(a[[i]],b[[i]])
         if(val!='Continue'){
            return(val)
         }
      }
      if(length(b)>length(a)){
         return('True')
      }
      return('Continue')
   }
}

# pair_a='[1,[2,[3,[4,[5,6,7]]]],8,9]'
# pair_b='[1,[2,[3,[4,[5,6,0]]]],8,9]'
# 
# # pair_a='[1,1,3,1,1]'
# # pair_b='[1,1,5,1,1]'
# 
# check_a=pair_to_list(pair_a)
# check_b=pair_to_list(pair_b)
# 
# test=compare_pairs(check_a,check_b)

#p1
out_table=data.table(i=1:150)
for(k in 1:150){
   print(k)
   pair_input=linn[(3*k-2):(3*k)]
   
   pair_a=pair_input[1]
   pair_b=pair_input[2]
   
   # check_a=pair_to_list(pair_a)#later found out it's valid json so can use fromJSON... :(, this code does work tho
   # check_b=pair_to_list(pair_b)
   
   check_a=fromJSON(pair_input[1],simplifyVector=F)
   check_b=fromJSON(pair_input[2],simplifyVector=F)
   
   test=compare_pairs(check_a,check_b)
   out_table[k,result:=test]
}
sum(out_table[result=='True']$i)

#p2
packets=c(linn[linn!=''],'[[2]]','[[6]]')
ordered=F
count=0
while(!ordered){#bubba sort
   count=count+1
   print(count)
   ordered=T
   for(i in 1:(length(packets)-1)){
      # is_order=compare_pairs(pair_to_list(packets[i]),pair_to_list(packets[i+1]))
      is_order=compare_pairs(fromJSON(packets[i],simplifyVector=F),fromJSON(packets[i+1],simplifyVector=F))
      
      if(is_order=='False'){
         packets_copy=copy(packets)
         packets[i]=packets_copy[i+1]
         packets[i+1]=packets_copy[i]
         
         ordered=F
      }
   }
}
prod(which(packets%in%c('[[2]]','[[6]]')))




