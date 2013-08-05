### Zephyros Protocol

* Clients connect to Zephyros via TCP
* All messages are simple JSON arrays
* Messages in both directions are encoded as `json.length.to_s + "\n" + json`
* Each message to Zephyros will be [msg_id, receiver_id, method, *args]
* Each response from Zephyros will be [msg_id, value]
    * Each response can be matched to its request by msg_id
    * Every message will get at least one response
    * Methods with callbacks (i.e. 'bind' and 'listen') will get multiple responses
* All methods at the top level take 0 as the receiver_id
* Any resource (Window, Screen, App) within responses will be returned as a number uniquely identifying this resource:
    * This number should be sent as the receiver for method calls to this object
    * Each resource is garbage-collected by Zephyros after 30 seconds


### API

#### Top level

Name               | Args                                | Return value
-------------------|-------------------------------------|--------------------
bind               | key, [modifier, ...]                | nil, followed by: callback, ...
listen             | event_name                          | nil, followed by: callback, ...
relaunch_config    |                                     |
clipboard_contents |                                     | string
focused_window     |                                     | [window_id](#window)
visible_windows    |                                     | [[window_id](#window), ...]
all_windows        |                                     | [[window_id](#window), ...]
main_screen        |                                     | [screen_id](#screen)
all_screens        |                                     | [[screen_id](#screen), ...]
running_apps       |                                     | [[app_id](#app), ...]
alert              | msg, duration_sec                   |
log                | msg                                 |
choose_from        | list, title, lines_tall, chars_wide | 0, followed by: chosen index or nil if canceled

#### Window

Name                         | Args         | Return value
-----------------------------|--------------|--------------------
title                        |              | string
set_frame                    | {x, y, w, h} |
set_top_left                 | {x, y}       |
set_size                     | {w, h}       |
frame                        |              | {x, y, w, h}
top_left                     |              | {x, y}
size                         |              | {w, h}
maximize                     |              |
minimize                     |              |
un_minimize                  |              |
app                          |              | [app_id](#app)
screen                       |              | [screen_id](#screen)
focus_window                 |              | bool
focus_window_left            |              |
focus_window_right           |              |
focus_window_up              |              |
focus_window_down            |              |
normal_window?               |              | bool
minimized?                   |              | bool
other_windows_on_same_screen |              | [[window_id](#window), ...]
other_windows_on_all_screens |              | [[window_id](#window), ...]

#### App

Name            | Args | Return value
----------------|------|--------------------
visible_windows |      | [[window_id](#window), ...]
all_windows     |      | [[window_id](#window), ...]
title           |      | string
hidden?         |      | bool
show            |      |
hide            |      |
kill            |      |
kill9           |      |

#### Screen

Name                          | Args         | Return value
------------------------------|--------------|--------------
frame_including_dock_and_menu | {x, y, w, h} |
frame_without_dock_or_menu    | {x, y, w, h} |
previous_screen               |              | [screen_id](#screen)
next_screen                   |              | [screen_id](#screen)

#### Events

Event name          | Callback parameter list
--------------------|-------------------------
window_created      | [[window_id](#window)]
window_minimized    | [[window_id](#window)]
window_unminimized  | [[window_id](#window)]
window_moved        | [[window_id](#window)]
window_resized      | [[window_id](#window)]
app_launched        | [[appdow_id](#window)]
app_died            | [[app_id](#app)]
app_hidden          | [[app_id](#app)]
app_shown           | [[app_id](#app)]
screens_changed     | []
