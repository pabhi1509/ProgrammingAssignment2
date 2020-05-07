## "makeCacheMatrix <- function(x = matrix())" 
## This function gets a matrix as an input, set the value of the
## matrix, get the value of the matrix,set the inverse Matrix and get the 
## inverse Matrix. The Matrix object can cache its own object

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i <<- inverse
  getinverse<-function() i
  list(set = set,
       get = get, 
       setinverse = setinverse, 
       getinverse=getinverse)

}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
## input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
## In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
## and set the invertible  matrix by using the solve function.
## In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
## after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
## and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
#####TEsTING#######################################
##M1<-matrix(c(1,2,3,4),2,2)
##M1
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##M2<- makeCacheMatrix(M1)
##cacheSolve(M2)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
