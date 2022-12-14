library(data.table)
  
test=fread('/home/eqiu/code_projects/aoc_2022/data/input_2',header=F)

X=Rock
Y=Paper
Z=Scissors


0,3,6
1,2,3


agg=test[,.N,by=.(V1,V2)]

agg[,`:=`(s1=0,s2=0)]

agg[V2=='X',s1:=1]
agg[V2=='Y',s1:=2]
agg[V2=='Z',s1:=3]

agg[V1=='A'&V2=='X',s2:=3 ]
agg[V1=='B'&V2=='Y',s2:=3 ]
agg[V1=='C'&V2=='Z',s2:=3 ]

agg[V1=='A'&V2=='Y',s2:=6 ]
agg[V1=='B'&V2=='Z',s2:=6 ]
agg[V1=='C'&V2=='X',s2:=6 ]


agg[,sum(N*(s1+s2))]

#
# X=lose
# Y=draw
# Z=win

agg=test[,.N,by=.(V1,V2)]
agg[,`:=`(s1=0,s2=0)]
agg[V2=='X',s1:=0]
agg[V2=='Y',s1:=3]
agg[V2=='Z',s1:=6]

agg[V1=='A'&V2=='Y',s2:=1]
agg[V1=='B'&V2=='X',s2:=1]
agg[V1=='C'&V2=='Z',s2:=1]

agg[V1=='B'&V2=='Y',s2:=2]
agg[V1=='C'&V2=='X',s2:=2]
agg[V1=='A'&V2=='Z',s2:=2]

agg[V1=='C'&V2=='Y',s2:=3]
agg[V1=='A'&V2=='X',s2:=3]
agg[V1=='B'&V2=='Z',s2:=3]

agg[,sum(N*(s1+s2))]
