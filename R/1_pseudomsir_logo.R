#' @title pseudomsir_logo
#' @description pseudomsir_logo
#' @author Xiaotao Shen
#' \email{shenxt@@stanford.edu}
#' @importFrom mzR openMSfile
#' @importFrom MSnbase readMSData
#' @importFrom ProtGenerics peaks rtime
#' @importFrom crayon yellow red green bold bgRed
#' @importFrom dplyr group_by mutate_if sample_n ungroup select
#' @importFrom dplyr mutate select distinct left_join summarise
#' @importFrom magrittr %>%
#' @importFrom grDevices png colorRampPalette dev.off
#' @importFrom graphics image par
#' @importFrom stats rnorm
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_wider
#' @export
#' @return logo
#' @examples
#' pseudomsir_logo()

pseudomsir_logo <- function() {
  cat(crayon::green("Thank you for using pseudomsir_logo!\n"))
  message(crayon::green("Version", pseudomsir_version, "(", update_date, ')\n'))
  cat(crayon::green("Bug fixing\n"))
  cat(crayon::green("More information: searching 'deeppseudomsi'.\n"))
  cat(crayon::green(
    c(
      "                           _       __  __  _____ _____ _____  ",
      "                          | |     |  \\/  |/ ____|_   _|  __ \\ ",
      "  _ __  ___  ___ _   _  __| | ___ | \\  / | (___   | | | |__) |",
      " | '_ \\/ __|/ _ \\ | | |/ _` |/ _ \\| |\\/| |\\___ \\  | | |  _  / ",
      " | |_) \\__ \\  __/ |_| | (_| | (_) | |  | |____) |_| |_| | \\ \\ ",
      " | .__/|___/\\___|\\__,_|\\__,_|\\___/|_|  |_|_____/|_____|_|  \\_\\",
      " | |                                                          ",
      " |_|                                                          "
    )
    
  ), sep = "\n")
}

pseudomsir_version <-
  utils::packageVersion(pkg = "pseudomsir")
update_date <-
  as.character(Sys.time())

#' # library(cowsay)
#' #https://onlineasciitools.com/convert-text-to-ascii-art
#' # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
#' # art <- readLines("logo.txt")
#' # dput(art)
