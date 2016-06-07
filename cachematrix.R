## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function can be used to create a matrix, set/get the inverse of a matrix
#This function returns a list of these subfunctions

makeCacheMatrix <- function(x = matrix()) {
  #Set the inverse to NULL initially
  inv<-NULL
  
  #Function that sets the data matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #Function that gets the data matrix
  get<-function() x
  
  #Function that sets the inverse of the matrix
  setinv<-function(inver) inv<-inver 
  
  #Function that gets the inverse of the matrix
  getinv<-function() inv
  
  #The returned list of various functions
  list(set=set,get=get,setinverse=setinv,getinverse=getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  #Check if the inverse already exists
  inv<-x$getinverse()
  
  #If yes, get the cached inverse
  if(!is.null(inv)){
    message("Obtaining cached inverse of the matrix")
    return(inv)
  }
  #If not, get the data matrix
  matrx<-x$get()
  
  #Compute the inverse of the matrix
  inv<-solve(matrx)
  
  #Store the inverse
  x$setinverse(inv)
  
  #Return the inverse
  inv
}
