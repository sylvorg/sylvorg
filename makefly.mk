.RECIPEPREFIX := |
.DEFAULT_GOAL := pre-init

pre-init:
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/<<username>>/.emacs.d.git home/.emacs.d
|git -C $(mkfileDir)/home/.emacs.d checkout main
|git -C $(mkfileDir) submodule add --depth 1 -f https://github.com/<<username>>/siluam.git home/siluam
|git -C $(mkfileDir)/home/siluam checkout main
