## function creates wrapper over matrix which is able to keep calculated inverted matrix value

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL;
  set <- function(y) {
    x <<- y;
    inv_matrix <- NULL;
  }
  get <- function() x
  get_inv <- function() inv_matrix
  set_inv <- function(y) inv_matrix <<- y
  list(
    set = set,
    get = get,
    get_inv = get_inv,
    set_inv = set_inv
  )
}


## computes inverse matrix if it's not computed, otherwise returns value from cache

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inv()
  if(!is.null(inv_matrix)) {
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$set_inv(inv_matrix)
  return(inv_matrix)
}
