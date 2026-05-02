# =============================================================================
#  bda_knitr_setup.R
#  Shared knitr configuration for Business Data Analytics course units.
#  -----------------------------------------------------------------------------
#  Sourced by every Unit's lecture.Rnw inside the <<setup, include=FALSE>>=
#  chunk:
#
#    <<setup, include=FALSE>>=
#    library(reticulate)
#    reticulate::use_virtualenv("~/path/to/.venv", required = TRUE)  # adjust
#    source("../../style_references/bda_knitr_setup.R")
#    @
#
#  render_sweave() replaces the default knitr framed/highlighting wrappers,
#  which conflict with minted's catcode handling (any "#" inside an R comment
#  triggers a build error otherwise). Sinput / Soutput environments are
#  redirected to minted / Verbatim through hooks. Python chunks are dispatched
#  through reticulate; the host system needs numpy / scipy / matplotlib in the
#  selected environment.
# =============================================================================

library(knitr)

knitr::render_sweave()

knitr::opts_chunk$set(
  fig.path   = "figs/",
  fig.align  = "center",
  fig.width  = 6.4,           # tuned for the 257mm x 144.5mm slide format
  fig.height = 3.0,
  out.width  = "0.78\\linewidth",
  dev        = "cairo_pdf",
  echo       = TRUE,
  message    = FALSE,
  warning    = FALSE,
  prompt     = FALSE,
  comment    = "##"
)

# Source hook: render every chunk through minted, defaulting to language "r"
# when no engine option is supplied. Optional chunk options:
#   linenos        : TRUE/FALSE, override the global linenos setting
#   firstnumber    : integer or "last" (continue numbering from previous block)
#   highlightlines : string such as "{5-7,10}" to shade specific lines
knitr::knit_hooks$set(source = function(x, options) {
  lang <- options$engine
  if (is.null(lang) || lang == "R") lang <- "r"

  mopts <- character()
  if (!is.null(options$linenos)) {
    mopts <- c(mopts, if (isTRUE(options$linenos)) "linenos" else "linenos=false")
  }
  if (!is.null(options$firstnumber)) {
    mopts <- c(mopts, paste0("firstnumber=", options$firstnumber))
  }
  if (!is.null(options$highlightlines)) {
    mopts <- c(mopts, paste0("highlightlines=", options$highlightlines))
  }
  opts_str <- if (length(mopts)) paste0("[", paste(mopts, collapse = ","), "]") else ""

  paste0("\n\\begin{minted}", opts_str, "{", lang, "}\n",
         paste(x, collapse = "\n"),
         "\n\\end{minted}\n")
})

# Output hooks: route stdout, messages, and warnings through fancyvrb's
# Verbatim, which coexists with minted more reliably than knitr's defaults.
knitr::knit_hooks$set(output = function(x, options) {
  paste0("\n\\begin{Verbatim}[fontsize=\\small,frame=leftline,framesep=2mm]\n",
         x,
         "\\end{Verbatim}\n")
})
knitr::knit_hooks$set(message = function(x, options) {
  paste0("\n\\begin{Verbatim}[fontsize=\\small,frame=leftline]\n",
         x, "\\end{Verbatim}\n")
})
knitr::knit_hooks$set(warning = function(x, options) {
  paste0("\n\\begin{Verbatim}[fontsize=\\small,frame=leftline]\n",
         x, "\\end{Verbatim}\n")
})
