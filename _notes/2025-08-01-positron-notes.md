---
title: "Positron notes"
date: 2025-07-01
tags: [r, positron, vscode, tooling]
---

Getting up and running with R is pretty straightforward: install R,
install RStudio, install packages from CRAN, and off you go. Python is
more, uh, nonlinear: Install an environment manager and have it cordon
off little subversions of Python and sublibraries, install some IDE or
work in the browser 🤷. I'm not a Python guy; I don't know the easy route
to get up and running. I know that there is some amount of configuration 
overhead. 

This contrast between R and Python also describes the different
experiences between RStudio and Positron. Why does it feel like I'm
always configuring stuff in Positron? Well, it's because it inherits
from VSCode and VSCode is for *everything* and you can't support
*everything* without being extremely configurable or "hackable". Plus, I
don't know what I want from Positron quite yet, and RStudio has had
like 10 year headstart on Positron, so I'm going to remain charitable
and supportive of the team.

That said, here are keyboard shortcuts I added recently. I would have
added [WrapRmd](https://github.com/tjmahr/WrapRmd) but they need to [fix
some things](https://github.com/posit-dev/positron/issues/8771).

```json
// Place your key bindings in this file to override the defaults
[
  {
    // targets::tar_load() a symbol
    "key": "ctrl+shift+alt+l",
    "command": "workbench.action.executeCode.console",
    "when": "editorTextFocus",
    "args": {
        "langId": "r",
        "code": "targets::rstudio_addin_tar_load()",
        "focus": false
    }
  },
  {
    // Preview result of knitr::knit() on selection
    "key": "ctrl+shift+alt+k",
    "command": "workbench.action.executeCode.console",
    "when": "editorTextFocus",
    "args": {
        "langId": "r",
        "code": "WrapRmd::knit_selection_addin()",
        "focus": false
    }
  }
]
```

