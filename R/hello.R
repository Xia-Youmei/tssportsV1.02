# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {

  print("Hello, world!")
}


#' Add 100 to the given number
#'
#' @param x a number, float or int
#'
#' @return a number
#' @export
#'
#' @examples
#' add100(5)
#' add100(-90)
add100=function(x){
  x+100
}


#' Get the proportion of each number in an array
#'
#' @param x an array of numbers
#'
#' @return an array of proportions
#' @export
#'
#' @examples
#' prop(c(1,2,3,4,5))
prop=function(x){
  return(x/sum(x))
}

#' Add 10000 to the given number
#'
#' @param x a number, float or int
#'
#' @return a number
#' @export
#'
#' @examples
#' add10000(5)
#' add10000(-90)
add10000=function(x){
  x+10000
}

#' Add 10000 to the given number
#'
#' @param x a number, float or int
#'
#' @return a number
#' @export
#'
#' @examples
#' add10000(5)
#' add10000(-90)
addchu10000=function(x){
  x/10000
}
