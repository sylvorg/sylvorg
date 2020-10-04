from os import path as os_path
from sys import path as sys_path
from datetime import datetime

# Xonsh
$AUTO_CD = True
$AUTO_PUSHD = True
$AUTO_SUGGEST = True
$AUTO_SUGGEST_IN_COMPLETIONS = True
$BASH_COMPLETIONS = ["/home/shadowrylander/.p/xonsh/bash-completion/bash-completion"]
$COMPLETION_IN_THREAD = True
$COMPLETIONS_CONFIRM = True
$DOTGLOB = True
$FUZZY_PATH_COMPLETION = True
$MOUSE_SUPPORT = True
$PRETTY_PRINT_RESULTS = True
$PROMPT_TOOLKIT_COLOR_DEPTH = "DEPTH_24_BIT"
$SHELL_TYPE = "prompt_toolkit"
$UPDATE_COMPLETIONS_ON_KEYPRESS = True
$UPDATE_OS_ENVIRON = True
$VI_MODE = True
$XONSH_AUTOPAIR = True
$XONSH_CACHE_EVERYTHING = True
$XONSH_DEBUG = True
$XONSH_SHOW_TRACEBACK = True

# Xonsh Prompt
$PROMPT_FIELDS["prompt_end"] = "Wheee! 😹 "
# $PROMPT = "{BOLD_#E5004D}{env_name} {BOLD_#FF4081}{prompt_end}"
$PROMPT = lambda: $(starship prompt)
# $RIGHT_PROMPT = "{BOLD_#E5004D}{env_name} || {BOLD_#FF4081}{prompt_end} || {BOLD_#FC9F71}{user}@{hostname} || {cwd}"
$BOTTOM_TOOLBAR = "{BOLD_#E5004D} {env_name}{BOLD_#FC9F71} || {BOLD_#E5004D} {prompt_end} {BOLD_#FC9F71} || {BOLD_#E5004D} {user}@{hostname} {BOLD_#FC9F71} || {BOLD_#E5004D} {cwd} "

# Path
sys_path.insert(0, "")
extra_paths = [
    f"{home}/.nimble/bin",
    f"{home}/go/bin",
    "/usr/lib/node_modules",
]
$PATH += extra_paths
if os_path.exists(fp("~/git-annex.linux")):
   $PATH += fp("~/git-annex.linux")

# Shell
# $EDITOR = "emacsclient --socket-name=spacemacsd -t"
$EDITOR = "vim"
$TERM = "xterm-kitty"
$GPG_TTY=$(tty)

# Etc
# $DIRENV_WARN_TIMEOUT = "100y"
$LESSOPEN = "| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
$LESS = " -R "

# Linuxbrew
# $LINUXBREWHOME = "/home/linuxbrew/.linuxbrew"
# $PATH.append("$LINUXBREWHOME/bin")
# $MANPATH = ["$LINUXBREWHOME/man"]
# $PKG_CONFIG_PATH = ["$LINUXBREWHOME/lib64/pkgconfig", "$LINUXBREWHOME/lib/pkgconfig"]
# $LD_LIBRARY_PATH = ["$LINUXBREWHOME/lib64", "$LINUXBREWHOME/lib"]