package zen

import "core:sync"

@(private = "file")
Global :: struct {
	/* Value that a program exits with. Defaults to zero. */
	exit_code: uint,

	/* Path to the running file. */
	path:      string,

	/* Directory the running file is in. */
	dirname:   string,
	mutex:     sync.Mutex,
}

@(private = "file")
g: Global = {}

@(init)
zen_initialize :: proc "contextless" () {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.exit_code = 0
	g.path = ""
	g.dirname = ""
}

zen_update_dirname :: proc(new: string) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.dirname = new
}

zen_update_path :: proc(new: string) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.path = new
}

zen_update_exit_code :: proc(new: uint) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.exit_code = new
}

zen_get_dirname :: proc() -> string {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.dirname
}

zen_get_path :: proc() -> string {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.path
}

zen_get_exit_code :: proc() -> uint {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.exit_code
}
