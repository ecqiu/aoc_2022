library(data.table)
options(scipen=999)
# test=fread('/home/eqiu/code_projects/aoc_2022/data/input_25',header=F,fill=T)#,nrows=9,sep='\t'

conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_25',open="r")
linn <-readLines(conn)
close(conn)


snafu_to_normal=function(x){
  vec=rev(strsplit(x,split='')[[1]])
  
  vec_digit=gsub('-','-1',vec)
  vec_digit=gsub('=','-2',vec_digit)
  vec_digit=as.numeric(vec_digit)
  
  power=0
  out=0
  for(i in 1:length(vec)){
    out=out+5^(i-1)*vec_digit[i]
  }
  return(out) 
}


normal_to_snafu=function(x){
  digits=c()
  while(x>0){
    digits=c(digits,x%%5)
    x=x%/%5
  }
  
  i=1
  
  while(i <=length(digits)){
    if(digits[i]>2){
      if(i==length(digits)){
        digits=c(digits,0)
      }
      digits[i]=digits[i]-5
      digits[i+1]=digits[i+1]+1
    }
    i=i+1
  }
  
  digits_c=as.character(digits)
  
  digits_c=gsub('-2','=',digits_c)
  digits_c=gsub('-1','-',digits_c)
  digits_c=paste(rev(digits_c),collapse='')
  
  return(digits_c)  
}


ans=0
for(l in linn){
  ans=ans+snafu_to_normal(l)
}
print(normal_to_snafu(ans))

snafu_to_normal('1=-0-2')
normal_to_snafu(314159265)

linn

