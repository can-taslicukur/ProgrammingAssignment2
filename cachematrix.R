#These functions create a cacheMatrix. Since computing the inverse of a matrix is computationally expensive,
#caching inverse of a matrix reduces the amount of computation. Its optional to set a inverse of a function and
#if you set it, there is no need to computation, if you don't set it then its automatically calculated.



##This function enables user to create cacheMatrix.
##Its both useful for setting and getting Matrix's attributes.
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL #set inverse of the matrix to null at the beginning
  
  set <- function(y){ #this function sets x to y
    x<<-y
    inv<<-NULL
  }
  
  get<-function(x) x #this function returns x
  
  setsolve<-function(s) inv <<- s #this function sets inverse of a matrix to given s
  
  getsolve<-function() inv #this function returns inverse of a matrix
  
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve) #return all attributes of the matrix

}


## This function checks if you already set the inverse of the matrix.
## If you already set it, then it gets the given inverse
## If you didnt set it, it automatically calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  
  if (!is.null(inv)) { #If inverse already exists, return the given inverse
    message("getting cached data")
    return(inv)
  }
  
  #If inverse does not exist, calculate the inverse and return it
  mdat<-x$get()
  inv<-solve(mdat)
  x$setsolve(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
