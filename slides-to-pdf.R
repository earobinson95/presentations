# install.packages("remotes")
# remotes::install_github("jhelvy/xaringanBuilder")

library(xaringanBuilder)

build_pdf("Other/2021NerdNite-Human-Perception-of-Statistical-Charts/index.html", complex_slides = FALSE, partial_slides = FALSE)
build_pdf("Other/2021NerdNite-Human-Perception-of-Statistical-Charts/index.Rmd", complex_slides = TRUE, partial_slides = TRUE)


# install.packages("webshot")
library(webshot)
install_phantomjs()

file_name <- "Other/2021NerdNite-Human-Perception-of-Statistical-Charts/index.html"
webshot(file_name, "2021NerdNite-Human-Perception-of-Statistical-Charts.pdf")
