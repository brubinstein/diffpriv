## Replaces function pkgdown::build_site with one that copies vignette PDFs
## from inst/doc/ (produced after devtools::build_vignettes() is called), to a
## new subdirectory docs/articles. Only those vignette PDFs listed in the articles
## argument are copied over. E.g. might pass "diffpriv.pdf" as an argument.
## Useful in concert with manual navbar items linking to these newly-copied PDFs,
## as specified in the _pkgdown.yml config file.
##
## Typical workflow:
## Clean and Rebuild
## devtools::document()
## devtools::build_vignettes()
## [manually update pkg version (and if necessary min R version) in README.rmd]
## knit README.rmd
## source this file
## build_site(articles = c("diffpriv.pdf", "bernstein.pdf")
## a Check

library(pkgdown)

build_site <- function (pkg = ".", path = "docs", examples = TRUE, run_dont_run = FALSE,
          mathjax = TRUE, preview = interactive(), seed = 1014, encoding = "UTF-8",
          articles = c())
{
  #old <- set_pkgdown_env("true")
  #on.exit(set_pkgdown_env(old))
  pkg <- as_pkgdown(pkg)
  path <- rel_path(path, pkg$path)
  init_site(pkg, path)
  build_home(pkg, path = path, encoding = encoding)
  build_reference(pkg, lazy = FALSE, examples = examples, run_dont_run = run_dont_run,
                  mathjax = mathjax, seed = seed, path = file.path(path,
                                                                   "reference"), depth = 1L)
  #build_articles(pkg, path = file.path(path, "articles"), depth = 1L,
  #               encoding = encoding)
  build_news(pkg, path = file.path(path, "news"), depth = 1L)
  #if (preview) {
  #  preview_site(path)
  #}
  ### New bits for copying articles
  if (length(articles) > 0) {
    message("Copying articles")
    vpath <- file.path(rel_path("inst", pkg$path), "doc")
    vpathNew <- file.path(path, "articles")
    dir.create(vpathNew, showWarnings = FALSE)
    for (article in articles) {
      message("  Article: ", article)
      oldPath <- file.path(vpath, article)
      if (file.exists(oldPath)) {
        file.copy(oldPath, file.path(vpathNew, article))
      } else {
        warning("File ", article, " not found.")
      }
    }
  }
  ## End articles
  invisible(TRUE)
}
