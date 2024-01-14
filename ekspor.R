library(shinylive)

#' Mengekspor aplikasi Shiny dalam folder "apl"
#' ke folder "docs".
shinylive::export(appdir = "apl",
                  destdir = "docs")

# Menguji aplikasi dalam folder "docs".
httpuv::runStaticServer("docs")
