## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inve<-NULL #store cached inverse matrix
  
  #Set new matrix, reset cached inverse
  set<-function(y){
    x<<-y
    inve<<-NULL
    
  }
  #get the current matrix
  get <- function() x
  #set inverse in cache
  setinverse<- function(inverse) inve <<- inverse
  # get cached inverse
  getinverse<- function() inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinverse() #getting cached inverse
  if (!is.null(inve)){
    print("getting cached inverse")
    return(inve)
  }
  data <- x$get() #get matrix data
  inve <- solve(data,...) 
  x$setinverse(inve) #cache inverse
  inve # return inverse
               
  
}
