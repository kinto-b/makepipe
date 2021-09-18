mt <- read.csv(system.file("tests", "mtcars.csv", package = "makepipe"))
write.table(
  mt,
  file.path(system.file("tests", package = "makepipe"), "mtcars.txt"),
  sep = "|"
)
