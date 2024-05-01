#Build a new matrix
print("Please input an integer: ")
x<-as.integer(readline())
print("Please input another integer: ")
y<-as.integer(readline())
z<-matrix(nrow=x,ncol=y)

#Input parameters into matrix
z[1,1]<-1
z[2,1]<-1
i<-2

#Build Bell Number 
while(i<=x){
  for(j in 2:i){
    z[i,j]<-sum(z[i,j-1],na.rm=TRUE)+sum(z[i-1,j-1],na.rm=TRUE)
  }
  if(i<x){
    z[i+1,1]<-sum(z[i,i],na.rm=TRUE)
  }
  i<-i+1
}
#Show the result of Bell Number
print(z)

#Data Specification of assorted matters
Combination.1.vec<-c("2-3-5","1-1-2-2-3-7")
Position.1.vec<-c("START:[3,1],END:[3,3]","START:[1,1],END:[4,2]")
Sum_of_elements.1.vec<-c(10,16)
Matter.1.vec<-c("Water","Oxygen")

#Data Frame 1
Bell.Number.1.df<-data.frame(Combination=Combination.1.vec,Position=Position.1.vec,Sum_of_elements=Sum_of_elements.1.vec,Matter=Matter.1.vec)
print(Bell.Number.1.df)

#Data Specification of assorted matters
Combination.2.vec<-c("1-1-2","2-3-5","2-5-7","3-7-10")
Position.2.vec<-c("START:[1,1],END:[2,2]","START:[2,2],END:[3,3]","START:[3,1],END:[4,2]","START:[3,2],END:[4,3]")
Rank_of_Hypotenuse_of_Bell_number.2.vec<-c(1,1,3,2) 
Sum_of_elements.2.vec<-c(2,6,10,14)
Matter.2.vec<-c("Helium","Carbon","Water","Nitrogen")

#Data Frame 2
Bell.Number.2.df<-data.frame(Combination=Combination.2.vec,Position=Position.2.vec,Sum_of_elements=Sum_of_elements.2.vec,R.H.B.=Rank_of_Hypotenuse_of_Bell_number.2.vec,Matter=Matter.2.vec) # Ex.R.H.B.[1]=[1,2,5,15,..]
print(Bell.Number.2.df)

#Data Specification of assorted matters
Combination.3.vec<-c("5-7-10","15-27-37")
Position.3.vec<-c("START:[3,3],END:[4,3]","START:[4,4],END:[5,4]")
Sum_of_elements.3.vec<-c(22,79)
Matter.3.vec<-c("Titanium","Gold")

#Data Frame 3
Bell.Number.3.df<-data.frame(Combination=Combination.3.vec,Position=Position.3.vec,Sum_of_elements=Sum_of_elements.3.vec,Matter=Matter.3.vec)
print(Bell.Number.3.df)