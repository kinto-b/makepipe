mt <- read.csv(system.file("tests", "mtcars_src.csv", package = "makepipe"), check.names = FALSE)
makepipe::make_register(5, 'five')
write.csv(mt,file.path(system.file("tests", package = "makepipe"), "mtcars.csv"), row.names = FALSE)
