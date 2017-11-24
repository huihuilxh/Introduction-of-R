# Introduction-of-R
### R ####

#######  向量 ############ 
## 向量的赋值
x=c(10.4, 5.6, 3.1, 6.4, 21.7)

y=c(x,0,x) 
y

v=assign("x", c( 5.6, 3.1, 6.4, 21.7))
v

## 向量运算
x=c(-1, 0, 2)
y=c(3, 8, 2)
v=2*x+y+1
v

x*y
x/y
x^2
y^x

5%/%3  ##商
5%%3   ##取余

exp(x)
sqrt(y)

x=c(10, 6, 4, 7, 8)
min(x)
max(x)
range(x)

which.max(x)
which.min(x)

sum(x)
prod(x)
length(x)

median(x)
mean(x) 

## 等差数列
x=2.312:6
x
y=4:7.6  
y
z=1:30
z

v=seq(-5, 5, 0.2)
v

x=seq(-10,10,0.02)
x
y=x^2
y
plot(x,y)

w=rep(3, 100)
w
x=c(1,2,3)
x
u=rep(x, 10) 
u

## 逻辑语言
1>2
3<=3
1==3
1!=3

T&F
T|F
!T

all(1:7>3) 
any(1:7>3) 

## 缺失数据
x=c(1:3, NA)
is.na(x)

## 字符型向量
x=c("Height", "Weight")
y=paste("My", "name", "is") 
y

cat("My name is","\n") 

## 向量下标运算
x=c(1,4,7)
x[2]

y=1:100
v=c(3,5,9,10)
y[v] 
y[-v]


## 矩阵生成和运算
A=matrix(1:15, nrow=3, ncol=5);A
B=matrix(1:15, nrow=3, ncol=5, byrow=T);B

A+B
A-B
A*B
A/B 

t(A)
det(A)

## 向量内积，外积
x=1:5
y=2*1:5

x%*%y
crossprod(x,y)
tcrossprod(x,y) 

x%o%y
outer(x, y) 

############ 矩阵相关运算  #############
## 乘法 
A=matrix(1:9, nrow=3);A
B=matrix(9:1, nrow=3);B 
A%*%B  

## 对角阵 
v=rep(1, 5);v
I5=diag(v);I5   
a=diag(A);a   

## 逆 
A=matrix(1:9, nrow=3, byrow=T);A[3,3]=10;A
b=rep(1,3);b
x=solve(A,b);x
D=solve(A);D

## 特征根与特征向量 
Sm=eigen(A);Sm 
Sm$value
Sm$vector

## choleskey 分解
A=diag(4)+1
cA=chol(A);cA
t(cA)%*%cA 

##  奇异值分解
A=matrix(1:18, nrow=3, ncol=6)
sA=svd(A);sA 
sA$u%*%diag(sA$d)%*%t(sA$v)

## QR分解
A=matrix(1:16,4,4)
qA=qr(A);qA

## 矩阵Kronecker积
A=matrix(1:4,2,2)
B=matrix(rep(1,4),2,2)
kronecker(A,B) 

## 行列式
det(A) 

## 维数 
dim(A)
nrow(A)
ncol(A) 

##合并
A;B
cbind(A, B)
rbind(A, B) 

## 拉直
as.vector(A)
as.vector(B)

## 矩阵按行列计算
A=matrix(1:12, nrow=3, ncol=4)
rowSums(A)
rowMeans(A)
colSums(A)
colMeans(A) 

apply(A, 1, sum)
apply(A, 2, mean)
apply(A, 1, var)
apply(A, 2, sd)

## 数组命名
X=matrix(1:6, ncol=2)
colnames(X)=c("First", "Second")
rownames(X)=c("one", "two", "three") 
X

############# 数据框   ###################################### 
df=data.frame(
        Name=c("Alice", "Becka", "James", "Jeffrey", "John"), 
        Sex=c("F", "F", "M", "M", "M"), 
        Age=c(13, 13, 12, 13, 12),
        Height=c(56.5, 65.3, 57.3, 62.5, 59.0),
        Weight=c(84.0, 98.0, 83.0, 84.0, 99.5)
        )
print(df)

df$Weight
df$Age

############ 数据读取  ##################################### 
data1=read.table("EDUC_SCORES.txt", header=F);data1 
data2=read.csv("educ_scores.csv");data2

########### 写数据 ######################################## 
write.table(df, file="data1.txt")
write.csv(df, file="data2.csv") 


############ if else ############
np=2
if(np==1){a=1} else{a=2}

############# 循环 ###########
B=matrix(1:12, nrow=3, ncol=4);B
A=matrix(0, nrow=3, ncol=4);A
for (i in 1:3)
{
  for (j in 1:4)
  {
   A[i,j]=1/B[i,j]
  }
} 
print(A)


######### 函数 ##############
twosam=function(y1, y2) 
{
n1=length(y1); n2=length(y2)
yb1=mean(y1);  yb2=mean(y2)
s1=var(y1);    s2=var(y2)
s=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
tst=(yb1 - yb2)/sqrt(s*(1/n1+1/n2))
return(tst)
}

source("twosam.R")
A=c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 
       79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
B=c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95,
       79.97)

twosam(A,B)  

