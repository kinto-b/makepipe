#'@title One
#'@description The first script in a pipeline. This one doesn't have any
#'  dependencies.
#'@targets target1
#'@force TRUE
#'@makepipe
NULL

makepipe::make_register(5, 'five')

#'@title One
#'@description A second declaration
#'@targets target1
#'@force TRUE
#'@makepipe
NULL
