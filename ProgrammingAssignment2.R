#There are two functions makeCacheMatrix,makeCacheMatrix
#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated,then the cachesolve should retrieve the inverse from the cache.
#makeCacheMatrix consists of set, get, setinv, getinv

makeCacheMatrix <- function(x=matrix()){
  inv<- NULL                           #initializing inverse as NULL
  set<- function(y){
      x<<-y
      inv<<-NULL
  }
  get<- function()x                   #function to get matrix x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set= set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


#This is used to get the cache data
cachesolve<- function(x, ...)              #gets cache data
  {
  inv<- x$getinverse()
  if(!is.null(inv)){                  #checking whether inverse is NULL
          message("getting cached data!")
          return(inv)                 #returns inverse value
  }
  data<- x$get()
  inv<-solve(data,...)                #calculates inverse value
  x$setinverse(inv)
  inv                                 #Return a matrix that is the inverse of 'x'
}
f<-makeCacheMatrix(matrix(1:4,2,2))
f$get()
f$getinverse()
cachesolve(f)
f$getinverse()
