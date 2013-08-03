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

		f := win.TopLeft()
		f.X += 10
		win.SetTopLeft(f)


		// send(API, "choose_from", []string{"foo", "bar"}, "title", 20, 20, func(i interface{}) {
		// 	fmt.Println("inner!", i)
		// })
	})

	// API.Listen("app_launched", func(app App) {
	// 	API.Alert(app.Title(), 1)
	// })

	ListenForCallbacks()
}