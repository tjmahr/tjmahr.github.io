---
title: "Windows Terminal settings for Miniconda"
date: 2022-06-06
tags: [windows, terminal, miniconda, tooling]
---

Here is how to add the Miniconda prompt to Windows Terminal. Analogous
steps probably will work for an Anaconda prompt.

First, we need to know where the Miniconda prompt lives. 

  - Find the desired Anaconda/Miniconda prompt in the Start Menu. Right
    click \> Open file location
    
  - Select the shortcut for the Anaconda/Miniconda prompt. Right click \> Properties.
  
Look at the target field. For my Miniconda shortcut, I have:

```
%windir%\System32\cmd.exe "/K" C:\Users\trist\miniconda3\Scripts\activate.bat C:\Users\trist\miniconda3
```

We don't need to hardcode in my user profile path. I also don't think
that last path is necessary, so this can simplify to:

```
%windir%\System32\cmd.exe /K %USERPROFILE%\miniconda3\Scripts\activate.bat
```

In Windows Terminal, create a terminal profile:

  - open Settings \> Add a new profile \> New empty profile.
    
#### Interactive setup

We can use the Windows Terminal app to set up the miniconda prompt.
Things to change include:

  - Name
  - Command line: Use the target field from above. In my case, I paste
    in `%windir%\System32\cmd.exe /K
    %USERPROFILE%\miniconda3\Scripts\activate.bat`
  - Starting directory: I select "Use parent process directory".
  - Icon: I use the snake emoji 🐍 but [this
    page](https://dev.to/azure/easily-add-anaconda-prompt-in-windows-terminal-to-make-life-better-3p6j)
    has a hint about using a .ico file.

#### JSON

Instead of the interactive setup, we can open the JSON settings file
(Settings \> Open JSON file) and modify/paste in the settings. Here are
my settings. Here the `guid` field was created by Windows Terminal so we should
use the one it provides for us.


``` json
{
  "commandline": "%windir%\\System32\\cmd.exe /K %USERPROFILE%\\miniconda3\\Scripts\\activate.bat",
  "guid": "{2679ff34-f6b9-5fcd-9b81-08b50df61bae}",
  "icon": "\ud83d\udc0d",
  "name": "Miniconda",
  "startingDirectory": null
}
```
