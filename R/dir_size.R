#' Size of directories
#' @param folder A directory
#' @param Recurse Whether to recurse through \code{folder} into subdirectories.
#' @return Size of folder and subfolders
#' @export

dir_size <- function(folder, Recurse = FALSE) {
  stopifnot(.Platform$OS.type == "windows")
  get_wd <- getwd()
  on.exit(setwd(get_wd))
  setwd(folder)
  res <-
    if (Recurse) {
      shell('powershell -command "$fso = new-object -com Scripting.FileSystemObject; gci -Recurse -Directory | select @{l=\'Size\'; e={$fso.GetFolder($_.FullName).Size}},FullName | sort Size -Descending | ft @{l=\'Size [MB]\'; e={\'{0:N2}    \' -f ($_.Size / 1MB)}},FullName"',
            intern = TRUE)
    } else {
      shell('powershell -command "$fso = new-object -com Scripting.FileSystemObject; gci -Directory | select @{l=\'Size\'; e={$fso.GetFolder($_.FullName).Size}},FullName | sort Size -Descending | ft @{l=\'Size [MB]\'; e={\'{0:N2}    \' -f ($_.Size / 1MB)}},FullName"',
            intern = TRUE)
    }

  res <- res[nzchar(res)]
  res <- res[!startsWith(res, "----")]

  # Need to trick read_fwf
  is_dup <- length(res) == 2
  new_res <- if (is_dup) c(res[-1], res[-1]) else res[-1]
  out <- readr::read_fwf(new_res,
                         col_positions = readr::fwf_empty(new_res,
                                                          col_names = c("Size [MB]", "FullName"),
                                                          skip_empty_rows = TRUE))
  if (is_dup) {
    out <- out[!duplicated(out), ]
  }
  setwd(get_wd)
  out


}
