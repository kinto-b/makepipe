mt <- readRDS(system.file("tests", "mtcars.Rds", package = "makepipe"))
makepipe::make_register(5, 'five')
write.csv(mt,file.path(system.file("tests", package = "makepipe"), "mtcars.csv"))
