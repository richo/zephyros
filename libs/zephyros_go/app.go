package zephyros_go

import (
	"encoding/json"
)

type App float64

func (self App) Title() string {
	var buf string
	bytes := send(float64(self), nil, false, "title")
	json.Unmarshal(bytes, &buf)
	return buf
}
