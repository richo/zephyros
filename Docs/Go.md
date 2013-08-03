## Zephyros - Go API

Sample file:

```go
package main

import (
	. "../..//Applications/Zephyros.app/Contents/Resources/libs/zephyros_go"
)

func main() {
	API.Bind("D", []string{"Cmd", "Shift"}, func() {
		API.Alert("hello world", 1)
	})

	ListenForCallbacks()
}
```

Run: `go run myscript.go`
