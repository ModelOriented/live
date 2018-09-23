.onAttach <- function(...) {
  mlr::configureMlr()
  packageStartupMessage("Welcome to live v", utils::packageVersion("live"), ". Live is a part of DALEXverse. Find out more at https://github.com/pbiecek/DALEX.")
}
