#!/usr/bin/env mdsh

```python
from baker.y import echo, cowsay, lolcat

# echo "I'm supposed to be rainbow, but whatever. Hello!" | cowsay -f meow | lolcat

echo.bake_all_(_frozen = True)
hello = echo("I'm supposed to be rainbow, but whatever. Hello!") | cowsay( f = "meow") | lolcat()
hello(_run = True, _frozen = False)

 _________________________________
/ I'm supposed to be rainbow, but \
\                whatever. Hello! /
 ---------------------------------
  \
   \ ,   _ ___.--'''`--''//-,-_--_.
      \`"' ` || \\ \ \\/ / // / ,-\\`,_
     /'`  \ \ || Y  | \|/ / // / - |__ `-,
    /@"\  ` \ `\ |  | ||/ // | \/  \  `-._`-,_.,
   /  _.-. `.-\,___/\ _/|_/_\_\/|_/ |     `-._._)
   `-'``/  /  |  // \__/\__  /  \__/ \
        `-'  /-\/  | -|   \__ \   |-' |
          __/\ / _/ \/ __,-'   ) ,' _|'
         (((__/(((_.' ((___..-'((__,'

```

<!--
**shadowrylander/shadowrylander** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.

Here are some ideas to get you started:

- 🔭 I’m currently working on ...
- 🌱 I’m currently learning ...
- 👯 I’m looking to collaborate on ...
- 🤔 I’m looking for help with ...
- 💬 Ask me about ...
- 📫 How to reach me: ...
- 😄 Pronouns: ...
- ⚡ Fun fact: ...
-->

<!-- saku start -->



<!-- saku end -->
