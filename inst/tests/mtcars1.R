mt <- readRDS(system.file("tests", "mtcars.Rds", package = "makepipe"))
write.csv(mt,file.path(system.file("tests", package = "makepipe"), "mtcars.csv"))
