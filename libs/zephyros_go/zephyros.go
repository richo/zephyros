package zephyros_go

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

func ListenForCallbacks() {
	reader := bufio.NewReader(c)
	for {
		numBytes, _ := reader.ReadString('\n')
		numBytes = strings.Trim(numBytes, "\n")
		// fmt.Println("bytes", numBytes)
		i, _ := strconv.ParseUint(numBytes, 10, 64)

		buf := make([]byte, i)
		io.ReadFull(reader, buf)

		// fmt.Printf("%#v\n", string(buf))

		var msg []interface{}
		json.Unmarshal(buf, &msg)

		// fmt.Println(msg)

		id, obj := msg[0].(float64), msg[1]
		bytes, _ := json.Marshal(obj)
		respChans[id] <- bytes
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

var API api = 0

type Rect struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
	W float64 `json:"w"`
	H float64 `json:"h"`
}

type Size struct {
	W float64 `json:"w"`
	H float64 `json:"h"`
}

type TopLeft struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
}





func (self api) Bind(key string, mods []string, fn func()) {
	wrapFn := func(b []byte) { fn() }
	send(float64(self), wrapFn, true, "bind", key, mods)
}

func (self api) Alert(msg string, dur int) {
	send(float64(self), nil, false, "alert", msg, dur)
}

func (self api) FocusedWindow() window {
	var buf float64
	bytes := send(float64(self), nil, false, "focused_window")
	json.Unmarshal(bytes, &buf)
	return window(buf)
}




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
