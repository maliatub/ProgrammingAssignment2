## These two functoin makeCacheMatrix and cacheSolve get spicial matirx and  and check if its invers is avialble in enviroment
## it will use it and show it out. If it's not avaiblle it will calculate it use the solve()
## 

## The makeCacheMatrix take a matrix as argumen and retrun it invers and set it as invers

makeCacheMatrix <- function(x = matrix()) {
  
  myinv <- NULL 
  set <- function(y)
    x <<-y
  myinv <<-NULL
  get <-function()x
  setInv <- function(solve) myinv <<- solve
  getInv <- function() myinv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## CacheSolve take a matix as an argumnet and check if it is available it will show the message telling it used a cached data, other wise it will callcualte it.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myinv <- x$getInv()
  if (!is.null(myinv)){
    message("Get chached data!")
    return (myinv)
    
  }
  data <-x$get()
  myinv <- solve(data, ...)
  x$setInv(myinv)
  myinv
}
