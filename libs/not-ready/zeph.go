package main

import (
	"fmt"
	"net"
	"bufio"
	"encoding/json"
	"strconv"
	"strings"
	"io"
)

func connect() net.Conn {
	conn, _ := net.Dial("tcp", "localhost:1235")
	return conn
}

var c net.Conn = connect()

var respChans = make(map[float64]chan []byte)

func listenForCallbacks() {
	reader := bufio.NewReader(c)
	for {
		numBytes, _ := reader.ReadString('\n')
		numBytes = strings.Trim(numBytes, "\n")
		i, _ := strconv.ParseUint(numBytes, 10, 64)

		buf := make([]byte, i)
		io.ReadFull(reader, buf)

		var msg []interface{}
		json.Unmarshal(buf, &msg)

		id := msg[0]
		respChans[id.(float64)] <- buf
	}
}


var msgidChan chan float64 = make(chan float64)

func init() {
	go func() {
		var i float64 = 0
		for {
			i++
			msgidChan <- i
		}
	}()
}





func send(recv float64, fn func([]byte), infinite bool, method string, args ...interface{}) []byte {
	msgid := <- msgidChan

	ch := make(chan []byte, 10) // probably enough
	respChans[msgid] = ch

	msg := []interface{}{msgid, recv, method}
	val, _ := json.Marshal(append(msg, args...))
	jsonstr := string(val)
	fmt.Fprintf(c, "%v\n%v", len(jsonstr), jsonstr)

	if fn == nil {
		resp := <-ch
		delete(respChans, msgid)
		return resp
	}

 	go func() {
		<-ch // ignore

		if infinite {
			for { fn(<-ch) }
		} else {
			fn(<-ch)
		}

		delete(respChans, msgid)
	}()

	return nil
}



type api float64
type window float64

func (self api) bind(key string, mods []string, fn func()) {
	wrapFn := func(b []byte) { fn() }
	send(float64(self), wrapFn, true, "bind", key, mods)
}

func (self api) alert(msg string, dur int) {
	send(float64(self), nil, false, "alert", msg, dur)
}

func (self api) focusedWindow() window {
	var buf struct {
		id float64
		val float64
	}
	bytes := send(float64(self), nil, false, "focused_window")
	json.Unmarshal(bytes, &buf)
	return window(buf.val)
}

var API api = 0


func main() {
	API.bind("d", []string{"cmd", "shift"}, func() {
		API.alert("LIKE", 1)

		win := api.focusedWindow()
		fmt.Println(win)

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

	listenForCallbacks()
}
