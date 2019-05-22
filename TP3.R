Ax<-runif(100, 0, 1)
Ay<-runif(100, 0, 1)

Bx<-rnorm(100, 4, 1)
By<-rnorm(scale(100, 4, 1))

Cx<-rnorm(100, 0.5, sqrt(2))
Cy<-rnorm(100, 6, sqrt(2))

A<-matrix(nrow = 300, ncol = 2)

for(i in 1:100) {
  A[i, 1]<-Ax[i]
  A[i, 2]<-Ay[i]
  A[i+100, 1]<-Bx[i]
  A[i+100, 2]<-By[i]
  A[i+200, 1]<-Cx[i]
  A[i+200, 2]<-Cy[i]
}

A<-rbind(cbind(Ax, Ay), cbind(Bx, By), cbind(Cx, Cy))
plot(A)
points(Ax, Ay, col="red")
points(Bx, By, col="blue")
points(Cx, Cy, col="green")

D<-dist(A, method = "euclidean")
D<-as.matrix(D)

#A<-cbind(c(0,0,0.25,4,4.5,5), c(1,1.5,1,2,3,1))
rownames(A)<-c(1:nrow(A))
#D<-dist(A, method = "euclidean")
#D<-as.matrix(D)

while(nrow(D) >= 4) {
  min<-D[2,1]
  di<-2
  dj<-1
  for(i in 2:nrow(D)) {
    for(j in 1:(i-1)) {
      if(min > D[i, j]) {
        min<-D[i, j]
        di<-i
        dj<-j
      }
    }
  }
  moyX<-(A[di,1] + A[dj,1]) / 2
  moyY<-(A[di,2] + A[dj,2]) / 2
  A[dj,1]<-moyX
  A[dj,2]<-moyY
  rownames(A)[dj]<-paste(rownames(A)[dj], rownames(A)[di])
  A<-A[-di,]
  D<-dist(A, method = "euclidean")
  D<-as.matrix(D)
}
rownames(A)
points(A, col="purple")