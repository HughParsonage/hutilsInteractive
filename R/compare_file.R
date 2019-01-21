compare_file <- function(file, repo = "~/grattan") {
  get_wd <- getwd()
  setwd(repo)
  current <- fread(file)
  system("git checkout HEAD~")
  previous <- fread(file)
  system("git checkout master")
  setwd(get_wd)
  rbindlist(list(current = current,
                 previous = previous),
            use.names = FALSE,
            fill = TRUE,
            idcol = "id") %>%
    dcast(... ~ id) %>%
    .[current != previous]
}
