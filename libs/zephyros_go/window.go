package zephyros_go

import (
	"encoding/json"
)

type window float64

func (self window) Title() string {
	var buf string
	bytes := send(float64(self), nil, false, "title")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self window) Frame() Rect {
	var buf Rect
	bytes := send(float64(self), nil, false, "frame")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self window) SetFrame(f Rect) {
	send(float64(self), nil, false, "set_frame", f)
}

func (self window) Size() Size {
	var buf Size
	bytes := send(float64(self), nil, false, "size")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self window) SetSize(f Size) {
	send(float64(self), nil, false, "set_size", f)
}

func (self window) TopLeft() TopLeft {
	var buf TopLeft
	bytes := send(float64(self), nil, false, "top_left")
	json.Unmarshal(bytes, &buf)
	return buf
}

func (self window) SetTopLeft(f TopLeft) {
	send(float64(self), nil, false, "set_top_left", f)
}
