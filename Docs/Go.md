## Zephyros - Go API

Sample file:

```go
package main

import (
	"fmt"
	. "/Applications/Zephyros.app/Contents/Resources/libs/zephyros_go"
)

func main() {
	API.Bind("d", []string{"cmd", "shift"}, func() {
		API.Alert("LIKE", 1)

		win := API.FocusedWindow()
		fmt.Println(win.Title())

		f := win.TopLeft()
		f.X += 10
		win.SetTopLeft(f)

		API.ChooseFrom([]string{"foo", "bar"}, "title", 20, 20, func(i int) {
			fmt.Println(i)
		})
	})

	API.Listen("app_launched", func(app App) {
		API.Alert(app.Title(), 1)
	})

	ListenForCallbacks()
}
```

Run: `go run myscript.go`
