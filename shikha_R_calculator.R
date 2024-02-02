rm(list=ls())
#____________________________MATH & STAT FUNCTIONS______________________________
fact<-function(c)
{ f=1
for(d in 1:c){f=f*d}
return(f)
}
power<-function(a, b)
{ g=1
pow=1
if(b==0)
{ pow =1 } else
{ while(g<=b){
  pow = pow*a
  g=g+1}
}
return(pow)
}
sum<-function(vec,b)
{ s=0
for(j in 1:b){s=s+(vec[j])}
return(s)
}
average<-function(vec,b)
{ h<-sum(vec,b)
h<-h/b
return(h)
}
SS<-function(vec,b)
{ s=0
for(j in 1:b){ s=s+((vec[j])*(vec[j])) }
return(s)
}
var<-function(vec,n)
{e<-average(vec,n)
f<-SS(vec,n)
f<-f/n
c<-f-(e*e)
return(c)
}
mom<-function(vec,n,r,av)
{
  a=0
  for(k in 1:n){a=a+(power(((vec[k])-(av)),r))}
  z=a/n
  return(z)
}
corr<-function(vec1,vec2,vec12,n)
{
  e<-average(vec1,n)
  C=sqrt(var(vec1,n))
  f<-average(vec2,n)
  D=sqrt(var(vec2,n))
  h<-average(vec12,n)
  #CORRELATION
  O=(h-(e*f))/(C*D)
  return(O)
}
# _________________________ Simulation Functions _______________________________
uniform<-function(){
  print(paste("how many random numbers you want to generate :  "))
  n<-as.integer(readline(prompt=" n =  "))
  print("enter the min value ")
  mini=as.double(readline())
  print("enter the max value")
  maxi=as.double(readline())
  rn<-runif(n,min=mini,max=maxi)
  return(rn)
}
normal<-function(){
  print(paste("how many random numbers you want to generate :   "))
  n<-as.integer(readline(prompt=" n =  "))
  print("enter the value of mean meu :")
  meu<-as.double(readline())
  print("enter the value of s.d. :")
  sigma<-as.double(readline())
  rn<-rnorm(n, mean = meu, sd = sigma)
  return(rn)
}
expo<-function(){
  print(paste("how many random numbers you want to generate :  "))
  n<-as.integer(readline(prompt=" n =  "))
  lam<-as.double(readline(prompt="enter the lambda(>0)(rate parameter)= "))
  rn<- rexp(n, rate = lam)
  return(rn)
}
poisson<-function(){
  print(paste("how many random numbers you want to generate :   "))
  n<-as.integer(readline(prompt=" n =  "))
  print("enter the value of lambda")
  lam<-as.double(readline())
  rn<-rpois(n,lambda =lam)
  return(rn)
}
midsq<-function(){
  print(paste("how many random numbers you want to generate :   "))
  n<-as.integer(readline(prompt=" n =  "))
  print(paste("enter the value of SEED :"))
  seed<-as.double(readline(prompt = "SEED = "))
  x<-seed
  deta<-c()
  for(i in 1:n){
    b<-x*x
    c<-b%/%100
    d<-c%%10000
    deta<-c(deta,d)
    if(i>1){
      for(j in 1:(i-1)){
        if(deta[i]==deta[j]){
          cat("No. of UNIQUE Random numbers that can be generate = ",i-1,"\n")
          ndeta<-deta[1:(i-1)]
          deta<-ndeta
          break
        } 
      }
    }
    x<-d
  }
  return(deta)  
}

bneedle<-function(){
  print(paste("enter the value of n,length(l) and distance(d) b/w needles "))
  n<-as.double(readline(prompt = "n = "))
  l<-as.double(readline(prompt = "l = "))
  d<-as.double(readline(prompt = "d = "))
  count<-0
  for (i in 1:n){
    y<-runif(1,min = 0,max = d/2)
    a<-runif(1,min = 0,max = pi)
    if(y<=(l/2)*sin(a)){
      count<-count+1
    }
  }
  p<-count/n
  pie<-(2*l)/(p*d)
  return(paste("pie = ",pie))
}
# ____________________NUMERICAL FUNCTIONS___________________________________
bisec<-function(){
  fx<-readline(prompt="Enter the equation in terms of x  : ")
  fx<-parse(text =fx)
  a0<-as.integer(readline(prompt="Enter lower limit of interval for finding root : "))
  b0<-as.integer(readline(prompt="Enter upper limit of interval for finding root : "))
  e<-as.double(readline(prompt="Enter error tolerence : "))
  fm=1
  k=0
  cat("\n s.no \t|\t a0 \t\t\t|\t b0 \t\t\t|\t  x \n")
  while(abs(fm)>e){
    x=(a0+b0)/2
    fm=eval(fx)
    x=a0
    fa=eval(fx)
    x=b0
    fb=eval(fx)
    
    cat(k," \t|\t ",a0,"    \t\t|\t ",b0,"    \t\t|\t  ",x,   "\n")
    if(fa*fm<0){
      b0=(a0+b0)/2
    }else if(fb*fm<0){
      a0=(a0+b0)/2
    }else
    {
      cat("root not found in the interval")
    }
    k=k+1
  }
  
  cat(" Root of the equation is " ,(a0+b0)/2,"\n & No. of iterations are ",k,"\n")
}




regulafalsi<-function(fx){
  fx<-readline(prompt="Enter the equation : ")
  fx<-parse(text =fx)
  a0<-as.integer(readline(prompt="Enter lower limit of interval : "))
  b0<-as.integer(readline(prompt="Enter upper limit of interval : "))
  e<-as.double(readline(prompt="Enter error tolerence : "))
  fm=1
  k=0
  cat("\n s.no \t|\t a0 \t\t\t  | b0 \t\t\t       |  x \n")
  while(abs(fm)>e){
    x=a0
    fa=eval(fx)
    x=b0
    fb=eval(fx)
    x=((a0*fb)-(b0*fa))/(fb-fa)
    fm=eval(fx)
    cat(k," \t|\t ",a0,"    \t\t  |  ",b0,"    \t\t      |   ",  x,  "\n")
    if(fa*fm<0){
      b0=x
    }else if(fb*fm<0){
      a0=x
    }else
    {
      cat("root not exists in the interval")
    }
    k=k+1
  }
  
  cat(" Root of the equation is " ,(a0+b0)/2,"\n & No. of iterations are ",k,"\n")
}



Secant<-function(fx){
  fx<-readline(prompt="Enter the equation in terms of x : ")
  fx<-parse(text =fx)
  a<-as.integer(readline(prompt="Enter lower limit of interval : "))
  b<-as.integer(readline(prompt="Enter upper limit of interval : "))
  e<-as.double(readline(prompt="Enter error tolerence : "))
  fm=1
  k=0
  cat("\n s.no \t|\t a \t\t\t  | b \t\t\t           |  x \n")
  while(abs(fm)>e){
    x=a
    fa=eval(fx)
    x=b
    fb=eval(fx)
    x=((a*fb)-(b*fa))/(fb-fa)
    fm=eval(fx)
    cat(k," \t|\t ",a,"    \t\t  |  ",b,"    \t\t      |   ",  x,  "\n")
    a=b
    b=x
    k=k+1
  }
  
  cat("Root of the equation is " ,x,"\n & No of iterations are ",k,"\n")
  
}

newton<-function(fx){
  fx<-readline(prompt="Enter the equation : ")
  fx<-parse(text =fx)
  fde<-D(fx,'x')
  b<-as.double(readline(prompt="Enter a approximation of root for finding root : "))
  e<-as.double(readline(prompt="Enter error tolerence : "))
  fm=1
  k=0
  cat("\n s.no \t|\t b \t\t\t  | x \t\t\t              |  fm \n")
  while(abs(fm)>e){
    x=b
    fa=eval(fx)
    fb=eval(fde)
    x=b-(fa/fb)
    fm=eval(fx)
    cat(k," \t|\t ",b,"    \t\t  |  ",x,"    \t\t      |   ",  fm,  "\n")
    b=x
    k=k+1
  }
  
  cat("Root of the equation is " ,x," \n &  No of iterations are",k,"\n")
  
}



#_______________________________________________________________________________
repeat{
  cat("--------------------------------------  CALCULATOR --------------------------------------")
  cat("\n Select Mode:")
  cat("\n\t 1 : MATH \n\t 2 : STAT  \n\t 3 : SIMULATION \n\t 4 : NUMERICAL \n Enter Your Choice : ")
  p=as.integer(readline(prompt="Choice=  "))
  switch(p,"1"={
    cat("\n\t\t\t YOU ARE IN MATHEMATICAL MODE \n Select Operation  \n\t 1 : Addition \n\t 2 : Subtraction \n\t 3 : Multiplication  \n\t 4 : Division   \n\t 5 : Factorial   \n\t 6 : Permutation and Combination \n\t 7 : Power \n Enter Your Choice: ")
    p=as.integer(readline(prompt="Choice=  "))
    switch (p,
            "1"={cat("\n how many numbers you want to add : ")
              n=as.integer(readline())
              cat("\n enter all numbers:\n")
              s=0
              for(i in 1:n)
              {
                x=scan()
                s=s+x
              }
              cat("\n Addition of given numbers = ",s)},
            "2"={cat("\n how many numbers you want to operate : ")
              n=as.integer(readline())
              cat("Enter 1st number from which you subtract numbers=")
              a=as.double(readline(prompt="Number 1= "))
              b=0
              for(i in 2:n)
              {
                cat("\n Number",i,"= ")
                x= scan()
                b=b+x
              }
              s=a-b
              cat("\nSubtraction of given numbers = ",s)
            },
            "3"={cat("\n how many numbers you want to multiply : ")
              n=as.integer(readline())
              m=1
              for(i in 1:n)
              {
                cat("\n enter number",i,"=")
                x=scan()
                m=m*x
              }
              cat("\n Multiply of given numbers = ",m)
            },
            "4"={cat("\n Enter dividend =")
              b=as.double(readline())
              cat("\n Enter divisor = ")
              a=as.double(readline())
              m=b/a
              cat("\n",b,"/",a,"=",m)
            },
            "5"={cat("\n Enter number to calculate its Factorial =")
              n=as.integer(readline())
              m= fact(n)
              cat("\n", n,"! =",m)
            },
            "6"={cat("\n Enter the value of n = ")
              n=as.integer(readline())
              cat("\n Enter the value of r = ")
              i=as.integer(readline()) 
              c=n-i
              x=fact(n)
              a=fact(i)
              d=fact(c)
              C=(x)/(a*d)
              D=(x)/(d)
              cat("\n",n,"C",i,"=",C,"\n",n,"P",i,"=",D)
            },
            "7"={cat("\n Enter the value of base = ")
              x=as.integer(readline())
              cat("\n Enter the value of power = ")
              i=as.integer(readline())
              C=power(x,i)
              cat("\n",x,"^",i,"=",C)
            }
    )
  },
  "2"={cat("\n\t\t\t YOU ARE IN STATISTICAL MODE: \n 1 : 1-dimentional data  \n 2 : 2-dimentional data \n Enter Your Choice : ")
    p=as.integer(readline(prompt="Choice=  "))
    if(p==1)
    {
      cat("How many numbers you want to enter : ")
      n=as.integer(readline())
      cat("Enter the numbers for calculation : \n ")
      vec<-c()
      for(i in 1:n)
      {
        q=as.double(readline())
        vec=c(vec,q)
      }
      cat("\n Your data is = ",vec)
      repeat{
        cat("\n Select Operation:  \n\t 1 : Average \n\t 2 : variance \n\t 3 : Standard Deviation \n\t 4 : Range \n\t 5 : Median  \n\t 6 : Central Moments \n\t 7 : Skewness and Kurtosis   \n Enter your Choice : ")
        p=readline(prompt="Choice=  ")
        p=as.integer(p)
        switch(p,
               "1"={cat("Average calculation")
                 e=sum(vec,n)
                 cat("\nAverage is= ",e/n)
               },
               "2"={cat("Variance calculation")
                 e=sum(vec,n)
                 e=e/n
                 b=SS(vec,n)
                 b=b/n
                 C=b-(e*e)
                 cat("\n Variance is= ",C)
               },
               "3"={cat("Standard Deviation calculation")
                 e=sum(vec,n)
                 e=e/n
                 b=SS(vec,n)
                 b=b/n
                 C=b-(e*e)
                 C=sqrt(C)
                 cat("\n Standard Deviation is= ",C)
               },
               "4"={cat("Range calculation")
                 for(j in 1:n)
                 {
                   if(j>1)
                   {
                     if(vec[j]>e)
                       e=vec[j]
                     else if(vec[j]<b)
                       b=vec[j]
                   }
                   else
                   {
                     e=vec[j]
                     b=vec[j]
                   }
                 }
                 C=(e-b)
                 cat("\nRange is: ",C)
               },
               "5"={cat("\n Median Calculation")
                 for(i in 1:(n-1))
                 {
                   for(j in i:n)
                   {
                     if(vec[j]<vec[i])
                     {
                       b=vec[j]
                       vec[j]=vec[i]
                       vec[i]=b
                     }
                   }
                 }
                 if(n%%2==0)
                 {
                   n1=n/2
                   C=((vec[n1])+(vec[n1+1]))/2
                   cat("\nMedian is: ",C)
                 }
                 else
                 {
                   n1=n%/%2
                   C=vec[n1+1]
                   cat("\nMedian is: ",C)
                 }
               },
               "6" ={
                 cat("\n Central Moments Calculation")
                 cat("\n Want Central Moments upto order :")
                 c=as.integer(readline())
                 e=sum(vec,n)
                 e1=e/n
                 cat("\n Average = ",e1)
                 for(j in 1:c)
                 {
                   g= mom(vec,n,j,e1)
                   cat("\n meu",j,"=",g)
                   
                 }
               },
               "7" = {cat("Skewness and Kurtosis Calculation")
                 e=sum(vec , n)
                 e=e/n
                 cat("\n Average = ",e)
                 g= mom (vec,n,2,e)
                 cat("\n meu2 =  ",g)
                 h= mom (vec,n,3,e)
                 cat("\n meu3 =  ",h)
                 f=mom(vec,n,4,e)
                 cat("\n meu4 =  ",f)
                 skew = (h*h)/(g*g*g)
                 gamma1<- sqrt(skew)
                 kur =(f/(g*g))
                 gamma2<- (kur-3)
                 cat("\n Skewness: beta1 =  ",skew,"\n Kurtosis:  beta2 =  ",kur,"\n Skewness: gamma1 =  ",gamma1,"\n Kurtosis:  gamma2 =  ",gamma2)
               }
        )
        cat("\nDo you want to calculate something else with the same data ? If yes then press 1 else 2:")
        ch<-readline(prompt="Enter Choice = ")
        ch<-as.integer(ch)
        if(ch!=1){
          break}
      }
    }else if(p==2)
    {
      cat("How Many Numbers you Want to enter:  ")
      n=as.integer(readline())
      cat("\nEnter the Numbers for X: \n ")
      vec1<-c()			
      for(i in 1:n)
      {
        q=as.double(readline())
        vec1=c(vec1,q)
      }
      cat("\nEnter the Numbers for Y: \n ")
      vec2<-c()	
      for(j in 1:n)
      {
        l=as.double(readline())
        vec2=c(vec2,l)
      }
      vec12<-c()
      for(z in 1:n)
      {vec12[z]=(vec1[z]*vec2[z])}
      repeat{
        cat("\n Select operation: \n\t 1 : Correlation Coefficient \n\t 2 : Regression Lines \n Enter your choice : ")
        p=as.integer(readline())
        switch(p,
               "1"={
                 O<-corr(vec1,vec2,vec12,n)
                 cat("\n Correlation (R) is: ",O)
               },
               "2"={
                 O<-corr(vec1,vec2,vec12,n)
                 y<-average(vec2,n)
                 x<-average(vec1,n)
                 sy<-sqrt(var(vec2,n))
                 sx<-sqrt(var(vec1,n))
                 byx<-(O*sy)/sx
                 bxy<-(O*sx)/sy
                 cat("\n Regression line Y on X is : \n",y-(x*byx),"+",byx,"X")
                 cat("\n Regression line X on Y is : \n",x-(y*bxy),"+",bxy,"Y")
               }
        )
        cat("\nDo you want to calculate something else with the same data ? If yes then press 1 else 2:")
        ch<-readline(prompt="Enter Choice = ")
        ch<-as.integer(ch)
        if(ch!=1){
          break}
      }
    }
  },
  "3"={
    cat("\n\t\t\t YOU ARE IN SIMULATION MODE \n Select Operation  \n Random Number Generation \n\t 1 : Using MID SQUARE METHOD   \n\t 2 : FROM UNIFORM DISTRIBUTION \n\t 3 : FROM NORMAL DISTRIBUTION \n\t 4 : FROM EXPONENTIAL DISTRIBUTION \n\t 5 : POISSON DISTRIBUTION \n\t 6 : VALUE ofPI by BUFFEN NEEDLE EXPERIMENT \n Enter Your Choice: ")
    p=as.integer(readline(prompt="Choice=  ")) 
    x1<-switch(p,
               "1" = midsq(), 
               "2" =uniform(), "3"=  normal(),   "4"= expo(),"5"= poisson(), "6"= bneedle())
    print(x1)
  },
  "4"={
    cat("\n\t\t\t YOU ARE IN NUMERICAL MODE \n Select Operation  \n\t 1 : BISECTION METHOD   \n\t 2: SECANT METHOD  \n\t 3 : REGULA FALSI METHOD  \n\t 4 : NEWTON RAPHSON \n Enter Your Choice: ")
    p=as.integer(readline(prompt="Choice=  "))
    x<-switch(p,bisec(),Secant(),regulafalsi(),newton())
  }
  )
  cat("\n Wanna Restart ? If yes then press 1 else 2:")
  rs<-readline(prompt="Enter Choice = ")
  rs<-as.integer(rs)
  if(rs!=1){
    break}
}
