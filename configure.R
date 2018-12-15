library(tidyverse)

formatter <- function(x) {
  x <- format(x)
  gsub(" ", "0", x)
}

# first pass set.seed(922)
set.seed(8698)
combos <-
  expand.grid(
    n = c(500, 1000),
    extras = c(0, 25, 50, 100, 150, 200),
    corr = 0,
    seed = sample.int(10000, 50)
  ) %>%
  mutate(
    prefix =
      paste0("effects_",
             formatter(n), "_",
             formatter(extras), "_",
             formatter(seed))) %>%
  # randomize within seed
  group_by(seed) %>%
  slice(sample(1:n()))

code <- read.table(
  "template.R",
  sep = "\n",
  stringsAsFactors = FALSE,
  strip.white = FALSE, blank.lines.skip = FALSE,
  comment.char = "", quote = "")[,1]

for(i in 1:nrow(combos)) {
  simNum <- formatter(1:nrow(combos))
  tmp_code <- gsub("SEED", combos$seed[i], code)
  tmp_code <- gsub("EXTRAS", combos$extras[i], tmp_code)
  tmp_code <- gsub("SAMPSIZE", combos$n[i], tmp_code)
  tmp_code <- gsub("CORR", combos$corr[i], tmp_code)
  cat(paste(tmp_code, collapse = "\n"),
      file = paste0("files/", combos$prefix[i], ".R"))
}

cmds <- paste0(
  "R CMD BATCH --vanilla ",
  paste0(combos$prefix, ".R"),
  "\nsleep 15")

cmds <- paste0(
  "# /bin/bash\n\n",
  paste(cmds, collapse = "\n", sep = ""),
  "\n"
)
cat(cmds, file = "files/launch.sh", sep = "")


randomized <- combos$prefix
targets <- paste0(randomized, ".RData")
files <- paste0(randomized, ".R")
over <- length(targets) %% 3
if (over > 0) targets <- c(targets, rep("", 3 - over))
deps <- matrix(targets, nrow = 3)
deps <- apply(deps, 2, function(x) paste("\t", paste(x, collapse = " "), "\\\n"))

makeDeps <- paste(deps, collapse = "")
makeDeps <- substring(makeDeps, 3)
makeDeps <- substring(makeDeps, 1, nchar(makeDeps) - 2)

makeEntries <-
  paste0(
    targets,
    ": ", files, " ",
    "\n\t @date '+ %Y-%m-%d %H:%M:%S: + ", files, "'",
    "\n\t @$(RCMD) BATCH --vanilla ", files,
    "\n\t @date '+ %Y-%m-%d %H:%M:%S: - ", files, "'\n\n"
  )


cat(
  paste(
    "SHELL = /bin/bash\n",
    "R    ?= R \n",
    "RCMD  =@$(R) CMD\n",
    "TIMESTAMP = $(shell  date '+%Y-%m-%d-%H-%M')\n",
    "here=${PWD}/..\n",
    "all: ",
    makeDeps,
    "\n\n",
    paste(makeEntries, collapse = "")
    ),
  file = "files/makefile"
)


if (!interactive())
  q("no")
