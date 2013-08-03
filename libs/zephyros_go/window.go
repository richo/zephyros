package zephyros_go

import (
	"encoding/json"
)

type Window float64

func (self Window) Title() string {
	var buf string
	bytes := send(float64(self), nil, false, "title")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self Window) Frame() Rect {
	var buf Rect
	bytes := send(float64(self), nil, false, "frame")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self Window) SetFrame(f Rect) {
	send(float64(self), nil, false, "set_frame", f)
}

func (self Window) Size() Size {
	var buf Size
	bytes := send(float64(self), nil, false, "size")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self Window) SetSize(f Size) {
	send(float64(self), nil, false, "set_size", f)
}

func (self Window) TopLeft() TopLeft {
	var buf TopLeft
	bytes := send(float64(self), nil, false, "top_left")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self Window) SetTopLeft(f TopLeft) {
	send(float64(self), nil, false, "set_top_left", f)
}
