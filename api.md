#### Top level API

Name               | Args                                | Return value
-------------------|-------------------------------------|--------------------
bind               | key, mods                           | nil, callback, ...
listen             | event_name                          | 0, callback, ...
relaunch_config    |                                     | nil
clipboard_contents |                                     | string
focused_window     |                                     | id
visible_windows    |                                     | array of ids
all_windows        |                                     | array of ids
main_screen        |                                     | id
all_screens        |                                     | array of ids
running_apps       |                                     | array of ids
alert              | msg, duration_sec                   | nil
log                | msg                                 | nil
choose_from        | list, title, lines_tall, chars_wide | 0, idx

#### Window

Name                | Args         | Return value
--------------------|--------------|--------------------
title               |              | string
set_frame           | {x, y, w, h} | nil
set_top_left        | {x, y}       | nil
set_size            | {w, h}       | nil
frame               |              | {x, y, w, h}
top_left            |              | {x, y}
size                |              | {w, h}
maximize            |              | nil
minimize            |              | nil
un_minimize         |              | nil
app                 |              | id
screen              |              | id
focus_window        |              | nil
focus_window_left   |              | nil
focus_window_right  |              | nil
focus_window_up     |              | nil
focus_window_down   |              | nil
normal_window?      |              | bool
minimized?          |              | bool

#### App

#### Screen
