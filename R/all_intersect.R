#' all_intersect
#'
#' @param geom ()
#'
#' @return the geometry that contain the intersect of all geom contain in param
#' @import sf
#'
all_intersect<-function(geom){
  geom_intersect = sf::st_intersection(x=geom[[1]], y=geom[[2]])


  return(geom_intersect)
}
