# Common setup for all BDA slide decks
# Source this in every QMD: ```{r setup, include=FALSE} source("_common.R") ```

# Output truncation hook: use #| output.lines: N to limit output
# Preserves knitr's default formatting (code fences) by delegating to the
# original hook after truncation.
local({
  default_hook <- knitr::knit_hooks$get("output")
  knitr::knit_hooks$set(output = function(x, options) {
    if (!is.null(n <- options$output.lines)) {
      lines <- xfun::split_lines(x)
      if (length(lines) > n) {
        lines <- c(head(lines, n), "#> ... [output truncated]")
      }
      x <- paste(lines, collapse = "\n")
    }
    default_hook(x, options)
  })
})
