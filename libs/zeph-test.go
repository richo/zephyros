package main

import (
	"fmt"
	. "./zephyros_go"
)

func main() {
	API.Bind("d", []string{"cmd", "shift"}, func() {
		API.Alert("LIKE", 1)

		win := API.FocusedWindow()
		fmt.Println(win.Title())

		// win := send(API, "visible_windows")
		// title := send(win.([]interface{})[0].(float64), "title")
		// fmt.Println(title)

		// {
		// 	win := send(API, "focused_window")
		// 	frame := send(win.(float64), "frame").(map[string]interface{})
		// 	w := frame["w"].(float64)
		// 	w -= 10
		// 	frame["w"] = w
		// 	send(win.(float64), "set_frame", frame)

		// 	fmt.Println(frame)
		// }

		// send(API, "choose_from", []string{"foo", "bar"}, "title", 20, 20, func(i interface{}) {
		// 	fmt.Println("inner!", i)
		// })
	})

	ListenForCallbacks()
}