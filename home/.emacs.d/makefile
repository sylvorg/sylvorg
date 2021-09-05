# Adapted From: https://www.systutorials.com/how-to-get-the-full-path-and-directory-of-a-makefile-itself/
.RECIPEPREFIX := |
.DEFAULT_GOAL := emacs

mkfilePath := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfileDir := $(dir $(mkfilePath))
test := emacs --bg-daemon=test
killTest := emacsclient -s test -e "(kill-emacs)"

init:
|-sudo cp $(mkfileDir)/git-subtree $$(git --exec-path)/

subinit:
|git -C $(mkfileDir) submodule update --init --depth 1
|git -C $(mkfileDir) submodule sync
# |git -C $(mkfileDir) submodule foreach 'git -C $$toplevel config submodule.$$name.ignore all'
|cd $(mkfileDir)lib/org; make all; make autoloads

pull: init
|git -C $(mkfileDir) pull
|git -C $(mkfileDir) subtree pull-all

add:
|git submodule foreach git stash
|git -C $(mkfileDir) add .

commit:
|-git -C $(mkfileDir) commit --allow-empty-message -am ""

cammit: add commit

push-only: cammit
|-git -C $(mkfileDir) push

push: push-only init
|git -C $(mkfileDir) subtree prune
|-git -C $(mkfileDir) subtree push-all

tangle-setup:
|cp $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh
|chmod +x $(mkfileDir)/org-tangle.sh $(mkfileDir)/backup-tangle.sh

tangle: tangle-setup
|yes yes | fd . $(mkfileDir) \
    -HId 1 -e org \
    -E testing.aiern.org \
    -E resting.aiern.org \
    -x $(mkfileDir)/backup-tangle.sh
|yes yes | fd . $(mkfileDir)/siluam \
    -HIe org \
    -x $(mkfileDir)/backup-tangle.sh
|fd . $(mkfileDir) \
    -HIe sh \
    -E .local \
    -E lib \
    -E var \
    -x chmod +x

subtree-prep: tangle push-only

pre-test: subinit

test: pre-test
|emacs

test-doom: pre-test
|emacs --doom

test-graphene: pre-test
|emacs --graphene

test-nano: pre-test
|emacs --nano

pest: pre-test
|emacs -p

update-test: pre-test
|emacs --update

no-config-test:
|emacs -Q

test-and-kill-pre: pre-test
|-emacsclient -s test -e "(kill-emacs)"

test-and-kill: test-and-kill-pre
|$(test)
|$(killTest)

test-new-and-kill: test-and-kill-pre
|$(test) -Q
|$(killTest)

test-update-and-kill: test-and-kill-pre
|$(test) --update
|$(killTest)

test-update-doom-and-kill: test-and-kill-pre
|$(test) --udoom
|$(killTest)

test-update-graphene-and-kill: test-and-kill-pre
|$(test) --graphene --update
|$(killTest)

test-update-nano-and-kill: test-and-kill-pre
|$(test) --nano --update
|$(killTest)

delete-doom:
|rm -rf $(mkfileDir)/profiles/doom/.local

delete:
|rm -rf $(mkfileDir)/profiles/damascus/.local

delete-graphene:
|rm -rf $(mkfileDir)/profiles/graphene/.local

delete-nano:
|rm -rf $(mkfileDir)/profiles/nano/.local

emacs: tangle test
remacs: delete tangle test-update-and-kill test
doom-remacs: delete-doom tangle test-update-doom-and-kill test-doom
graphene-remacs: delete-graphene tangle test-update-graphene-and-kill test-graphene
nano-remacs: delete-nano tangle test-update-nano-and-kill test-nano
super-push: tangle push
super-push-only: tangle push-only
