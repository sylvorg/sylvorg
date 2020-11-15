# From: http://gegenokitaro.github.io/tuts/2015/05/14/qtile-ricchan/
#
# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget
from libqtile.dgroups import simple_key_binder

mod = "mod4"

keys = [
    # Switch between windows in current stack pane
    Key(
        [mod], "j",
        lazy.layout.down()
    ),
    Key(
        [mod], "k",
        lazy.layout.up()
    ),

    # Move windows up or down in current stack
    Key(
        [mod, "control"], "j",
        lazy.layout.shuffle_down()
    ),
    Key(
        [mod, "control"], "k",
        lazy.layout.shuffle_up()
    ),

    # Switch window focus to other pane(s) of stack
    Key(
        [mod], "space",
        lazy.layout.next()
    ),

    Key(
        [mod], "Left",
        lazy.screen.prevgroup()
    ),

    Key(
        [mod], "Right",
        lazy.screen.nextgroup()
    ),

    # Swap panes of split stack
    Key(
        [mod, "shift"], "space",
        lazy.layout.rotate()
    ),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "shift"], "Return",
        lazy.layout.toggle_split()
    ),
    Key([mod], "Return", lazy.spawn("urxvt")),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.nextlayout()),
    Key([mod], "w", lazy.window.kill()),

    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),
    Key([mod], "r", lazy.spawncmd()),
]

groups = [
    Group(" urxvt "),
    Group(" web ", matches=[Match(wm_class=["Firefox"])]),
    Group(" blender "),
    Group(" inkscape "),
    Group(" gimp "),
    Group(" doc "),
]

dgroups_key_binder = simple_key_binder("mod4")


layouts = [
    layout.TreeTab(
            font='Cartograph CF Light Italic',
            name="tree tab",
            bg_color="#222222",
            inactive_bg="#AB5DEE",
            panel_width=150,
            margin_left=0,
            margin_y=0,
            sections=['TreeTab'],
            section_left=0,
            padding_x=4,
            active_bg="#FFB86C",
            rounded=False,
        ),
    layout.MonadTall(
            name="xmonad tall",
            ratio=0.5,
            border_width=8,
            border_focus="#335260",
            border_normal="#69B2B8",
        ),
    layout.Stack(
            num_stacks=2,
            border_width=8,
            border_focus="#335260",
            border_normal="#69B2B8",
        ),
    layout.Floating(
            name="floating",
            border_width=8,
            border_focus="#335260",
            border_normal="#69B2B8",
        )
]

floating_layout = layout.Floating(
            name="floating",
            border_width=8,
            border_focus="#69B2B8",
            border_normal="#335260",
        )

widget_defaults = dict(
    font='Cartograph CF Light Italic',
    fontsize=12,
    background="#222222",
    markup=True,
)

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                        borderwidth=0,
                        margin=0,
                        padding=6,
                        active="FFFFFF",
                        inactive="FFB86C",
                        highlight_method="block",
                        this_current_screen_border="#AB5DEE",
                        invert_mouse_wheel=True,
                        rounded=False,
                    ),
                widget.Prompt(),
                widget.CurrentLayout(
                        background="#E11B22",
                    ),
                widget.Spacer(),
                #widget.WindowName(),
                widget.TextBox("testing", name="default"),
                widget.Systray(),
                widget.Clock(format=' %I:%M %p '),
            ],
            24,
            background="#335260",
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
