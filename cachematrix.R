## Ces deux fonctions permettent de mettre en cache l'inverse d'une matrice et d'appeler le résultat plus tard. 
##cela permet de gagner du temps si le calcul est gros et que l'on doit rappeler le résultat à plusieurs reprises.

## makeCacheMatrix permet de définir les fonctions qui écrivent en cache et appelle les résultats

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  #definir la fonction qui met en cache x
  set<-function(y){
    x<<-y
    m<<-NULL
  
  }
  #definir la fonction qui appelle x
  get<-function()x
  #definir la fonction qui calcule l'inverse de x et la met en cache
  setsolve<-function(solve) m<<-solve
  #definir la fonction qui appelle l'inverse de x
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve permet de vérifier si le calcul de l'inverse a déjà été réalisé pour ne pas avoir à le refaire, 
## ramène le résultat lorsqu'il est en cache ou le calcule si ce n'est pas le cas

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m<-x$getsolve()
  # test si l'inverse de x a deja été calculé, si oui retouner m
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  # si non calculer l'inverse
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
