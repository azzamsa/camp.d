# Themes

## Modus Theme

## Material Palenight

```lisp
;; palenight palette
(red         "#ff5370")
(orange      "#f78c6c")
(green       "#c3e88d")
(teal        "#44b9b1")
(yellow      "#ffcb6b")
(blue        "#82aaff")
(dark-blue   "#7986E7")
(magenta     "#c792ea")
(violet      "#bb80b3")
(cyan        "#89DDFF")
(dark-cyan   "#80cbc4")

(accent-1 blue)
(fg-prompt accent-1)

;; code
(builtin blue)
(constant orange)
(fnname blue)
(keyword cyan)
(string green)
(variable yellow)
(type magenta)

(fg-heading-0 accent-1)
(fg-heading-1 accent-1)
(fg-heading-2 magenta)
(fg-heading-3 violet)
(fg-heading-4 dark-blue)))
```

## Snazzy

```lisp
;; Snazzy palette
(red        "#ff5c57")
(orange     "#ff9f43")
(yellow     "#f3f99d")
(green      "#5af78e")
(magenta    "#ff6ac1")
(cyan       "#9aedfe")
(blue       "#57c7ff")

(accent-0 blue) ;; magit files
(accent-1 magenta) ;; cursor
(accent-2 orange)
(accent-3 yellow)

;; Prompt mappings
(fg-prompt accent-1)

;; Code mappings (order inspired by doom-dracula)
(builtin orange)
(constant cyan)
(fnname green)
(keyword magenta)
(string yellow)
(variable violet)
(type violet)

;; Heading mappings
(fg-heading-0 accent-1)
(fg-heading-1 accent-1)
(fg-heading-2 orange)
(fg-heading-3 green)
(fg-heading-4 green)))
```

## Dracula

```lisp
;; dracula palette
(red        "#ff5555")
(orange     "#ffb86c")
(green      "#50fa7b")
(teal       "#0189cc")
(yellow     "#f1fa8c")
(blue       "#61bfff")
(dark-blue  "#0189cc")
(magenta    "#ff79c6")
(violet     "#bd93f9")
(cyan       "#8be9fd")

(accent-1 magenta)
(fg-prompt accent-1)

;; Code
(builtin orange)
(constant cyan)
(fnname green)
(keyword magenta)
(string yellow)
(type violet)
(variable violet)

;; Heading mappings
(fg-heading-0 accent-1)
(fg-heading-1 accent-1)
(fg-heading-2 orange)
(fg-heading-3 green)
(fg-heading-4 green)))
```
