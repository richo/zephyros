## Zephyros - Go API

#### Sample script

```go
package main

import (
	. "../../Applications/Zephyros.app/Contents/Resources/libs/zephyros_go"
)

func main() {
	API.Bind("D", []string{"Cmd", "Shift"}, func() {
		API.Alert("hello world", 1)
	})

	ListenForCallbacks()
}
```

#### Run

```bash
go run myscript.go
```

#### API Docs

[Here.](http://godoc.org/github.com/sdegutis/zephyros/libs/zephyros_go)
