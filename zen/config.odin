package zen

import "core:sync"

@(private = "file")
Global :: struct {
	/* Value that a program exits with. Defaults to zero. */
	exit_code:    int,

	/* Path to the file invoked by the interpreter. */
	root_path:    string,

	/* Directory the invoked file is in. */
	root_dirname: string,

	/* Is the REPL being run? */
	repl:         bool,

	// zen is not multithreaded, so why the hell do I have a mutex here? Well,
	// its for the future, just in case it DOES become multithreaded
	mutex:        sync.Mutex,
}

@(private = "file")
g: Global = {}

@(init)
zen_initialize :: proc "contextless" () {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.exit_code = 0
	g.root_path = ""
	g.root_dirname = ""
}

zen_update_dirname :: proc(new: string) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.root_dirname = new
}

zen_update_path :: proc(new: string) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.root_path = new
}

zen_update_exit_code :: proc(new: int) {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	g.exit_code = new
}

zen_get_dirname :: proc() -> string {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.root_dirname
}

zen_get_path :: proc() -> string {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.root_path
}

zen_get_exit_code :: proc() -> int {
	sync.lock(&g.mutex)
	defer sync.unlock(&g.mutex)

	return g.exit_code
}
