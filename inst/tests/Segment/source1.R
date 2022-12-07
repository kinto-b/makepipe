#'@title One
#'@description The first script in a pipeline. This one doesn't have any
#'  dependencies.
#'@targets "one.txt"
#'@force TRUE
#'@makepipe
NULL

makepipe::make_register(5, 'five')
