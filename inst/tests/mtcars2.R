mt <- read.csv(system.file("tests", "mtcars.csv", package = "makepipe"), check.names = FALSE, row.names = NULL)
write.table(
  mt,
  file.path(system.file("tests", package = "makepipe"), "mtcars.txt"),
  sep = "|",
  row.names = FALSE
)
