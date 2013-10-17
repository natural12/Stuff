y=print(1:100)
x=as.matrix(y)

for (i in 1:100)
{if (x[i]%%15==0){
    print ("FizzBuzz")
  
  }else{    
    if (x[i]%%5==0){
      print ("Buzz") 
      
    }else{      
      if (x[i]%%3==0){
        print ("Fizz")
        
      }else{
        print (x[i])
      }
    }
      
  }
  
}




x=runif(1000,0,2*pi)
y=runif(1000,0,1)
u=y*cos(x)
v=y*sin(x)

plot(u,v)

r=sqrt(u^2+v^2)
plot(r)

library(car) 
scatterplot(u~v)


sentence="Hello, my name is Bob. I am a statistician. I like statistics very much."

words=strsplit(sentence,"")[[1]]

Split=strsplit(sentence,split = NULL)
Num=length(Split[[1]])

for (k in 1:Num){
 
  Name=sprintf("out_%02d.txt",k)
  write.table(as.character(Split[[1]][k]),Name,row.name=FALSE,col.name=FALSE)
}



str=""
for (i in 1:Num){
  x=scan(sprintf("out_%02d.txt",i),what="")
  str=paste(str,x,sep="")
  
}




for (i in 1:72){
write(x[i], file="out_.txt")
}

x <- matrix(1:10, ncol = 5)
write(t(x))
write(x, "", sep = "\t")

