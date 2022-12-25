library(data.table)
# library(microbenchmark)
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_14',header=F,fill=T)#,nrows=9,sep='\t'


conn <- file('/home/eqiu/code_projects/aoc_2022/data/input_14',open="r")
linn <-readLines(conn)



#init rocks
rock_mat=matrix(0,nrow=1000,ncol=1000)
for(h in 1:length(linn)){

   line_info=linn[h]
   splitted_str_split=strsplit(line_info,'->')[[1]]
   
   for(i in 1:(length(splitted_str_split)-1) ){
      dex1=as.numeric(strsplit(splitted_str_split[i],',')[[1]])
      dex2=as.numeric(strsplit(splitted_str_split[i+1],',')[[1]])
      
      rock_mat[ dex1[1]:dex2[1], (dex1[2]:dex2[2] +1)]=1
   }

}

bottom=max(which(colSums(rock_mat)>0))
bottom2=bottom+2


rock_mat[,bottom2]=1#only required for p2 but can leave uncommented for p1 if you want

sand_count=0
break_flag=F
while(!break_flag){
   sand_count=sand_count+1
   print(sand_count)
   sand_pos=c(500,1)
   stopped=F
   while(!stopped){
      # p1
      if(sand_pos[2]>=bottom){
         stopped=T
         break_flag=T
      }
      
      # p2
      # if(rock_mat[500,1]==2 ){
      #    stopped=T
      #    break_flag=T
      # }
      
      d=rock_mat[sand_pos[1],(sand_pos[2]+1) ]
      dl=rock_mat[sand_pos[1]-1,(sand_pos[2]+1) ]
      dr=rock_mat[sand_pos[1]+1,(sand_pos[2]+1) ]
      
      if(d==0){
         sand_pos=sand_pos+c(0,1)
      }else if(dl==0){
         sand_pos=sand_pos+c(-1,1)
      }else if(dr==0){
         sand_pos=sand_pos+c(1,1)
      }else{
         rock_mat[sand_pos[1],sand_pos[2]]=2
         stopped=T
      }
   }
}
sum(rock_mat==2)

# old_stuff=which(rock_mat==2)
# new_stuff=which(rock_mat==2)



#drop some sand