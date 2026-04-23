---
title: "MFA installation: do not change the conda solver"
date: 2024-01-01
tags: [mfa, conda, tooling]
---

My colleague and I independently ran into an issue where the Montreal
Forced Aligner installation with conda seemed to take forever during the
solving environment stage. We both tried to fix this problem by changing
the default solver. This change did lead to improved installation, but
the MFA could not run at all. The correct thing to do is **do nothing**.
Just use the default solver and wait it out.
