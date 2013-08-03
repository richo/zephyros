package zephyros_go

import (
	"encoding/json"
)

type api float64
var API api = 0



func (self api) Bind(key string, mods []string, fn func()) {
	wrapFn := func(b []byte) { fn() }
	send(float64(self), wrapFn, true, "bind", key, mods)
}

func (self api) Alert(msg string, dur int) {
	send(float64(self), nil, false, "alert", msg, dur)
}

func (self api) RelaunchConfig() {
	send(float64(self), nil, false, "relaunch_config")
}

func (self api) ClipboardContents() string {
	var buf string
	bytes := send(float64(self), nil, false, "clipboard_contents")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self api) FocusedWindow() window {
	var buf window
	bytes := send(float64(self), nil, false, "focused_window")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self api) VisibleWindows() []window {
	var buf []window
	bytes := send(float64(self), nil, false, "visible_windows")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self api) AllWindows() []window {
	var buf []window
	bytes := send(float64(self), nil, false, "all_windows")
	json.Unmarshal(bytes, &buf)
	return buf
}
