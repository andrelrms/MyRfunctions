
centroid <- function(x, pos = 'both'){
  unlist_geom <- unlist(x)
  n_elem <- length(unlist_geom)
  x_coord <- median(unlist_geom[1:(n_elem/2)])
  y_coord <- median(unlist_geom[(n_elem/2 + 1):n_elem])
  ret_coord <- c(x_coord,y_coord)
  if(pos=='x'){
    ret_coord <- x_coord
  }
  if(pos=='y'){
    ret_coord <- y_coord
  }

  return(ret_coord)
}


addUnits <- function(n) {
  # credits to https://5harad.com/mse125/r/visualization_code.html
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        'too big!'
                                 ))))
  return(labels)
}
