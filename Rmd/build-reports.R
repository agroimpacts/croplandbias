# build-reports.R
# Builds output Rmd files

library(lmisc)
require(knitr)
require(markdown)

p_root <- proj_root("SAcropland")
p_vignettes <- full_path(p_root, "SAcropland/vignettes")

# Create .md, .html, and .pdf files
knit("vignettes/overview.Rmd", output = "vignettes/overview.md")
markdownToHTML("vignettes/overview.md", "vignettes/overview.html", 
               options=c("use_xhml"))

knit("vignettes/compare-landcover.Rmd", 
     output = "vignettes/compare-landcover.md")
markdownToHTML("vignettes/compare-landcover.md", 
               "vignettes/compare-landcover.html", options=c("use_xhml"))

knit("vignettes/landcover-bias-plots.Rmd", 
     output = "vignettes/landcover-bias-plots.md")
markdownToHTML("vignettes/landcover-bias-plots.md", 
               "vignettes/landcover-bias-plots.html", options=c("use_xhml"))
