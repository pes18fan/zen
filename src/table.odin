package zen

import "core:fmt"
import "core:mem"
import "core:strings"

@(private = "file")
MAX_LOAD :: 0.6

@(private = "file")
Entry :: struct {
	key:   ^ObjString,
	value: Value,
}

Table :: struct {
	count:    int,
	capacity: int,
	entries:  []Entry,
}

init_table :: proc() -> Table {
	return Table{count = 0, capacity = 0, entries = make([]Entry, 0)}
}

free_table :: proc(table: ^Table) {
	delete(table.entries)
}

table_get :: proc(table: ^Table, key: ^ObjString) -> (Value, bool) {
	if table.count == 0 {return nil_val(), false}

	entry := find_entry(table.entries, table.capacity, key)
	if entry.key == nil {return nil_val(), false}

	return entry.value, true
}

table_set :: proc(table: ^Table, key: ^ObjString, value: Value) -> bool {
	if f32(table.count + 1) > f32(table.capacity) * MAX_LOAD {
		capacity := grow_capacity(table.capacity)
		adjust_capacity(table, capacity)
	}

	entry := find_entry(table.entries, table.capacity, key)
	is_new_key := entry.key == nil
	if is_new_key && is_nil(entry.value) {
		table.count += 1
	}

	entry.key = key
	entry.value = value
	return is_new_key
}

@(private = "file")
find_entry :: proc(entries: []Entry, capacity: int, key: ^ObjString) -> ^Entry {
	index := key.hash % u32(capacity)
	tombstone: ^Entry = nil

	for {
		entry := &entries[index]

		if entry.key == nil {
			if is_nil(entry.value) {
				return tombstone if tombstone != nil else entry
			} else {
				if tombstone == nil do tombstone = entry
			}
		} else if entry.key == key {
			return entry
		}

		index = (index + 1) % u32(capacity)
	}
}

@(private = "file")
adjust_capacity :: proc(table: ^Table, capacity: int) {
	entries := make([]Entry, capacity)
	for i in 0 ..< capacity {
		entries[i].key = nil
		entries[i].value = nil_val()
	}

	table.count = 0
	for i in 0 ..< table.capacity {
		entry := &table.entries[i]
		if entry.key == nil {continue}

		dest := find_entry(entries, capacity, entry.key)
		dest.key = entry.key
		dest.value = entry.value
		table.count += 1
	}

	delete(table.entries)
	table.entries = entries
	table.capacity = capacity
}

table_delete :: proc(table: ^Table, key: ^ObjString) -> bool {
	if table.count == 0 do return false

	entry := find_entry(table.entries, table.capacity, key)
	if entry.key == nil do return false

	entry.key = nil
	entry.value = bool_val(true)
	return true
}

/* Copy all entries in one table to another. */
table_add_all :: proc(from: ^Table, to: ^Table) {
	for i in 0 ..< from.capacity {
		entry := &from.entries[i]

		if entry.key != nil {
			table_set(to, entry.key, entry.value)
		}
	}
}

table_find_string :: proc(table: ^Table, str: string, hash: u32) -> ^ObjString {
	if table.count == 0 {return nil}

	index := hash % u32(table.capacity)
	for {
		entry := &table.entries[index]

		if entry.key == nil {
			if is_nil(entry.value) {return nil}
		} else if (entry.key.len == len(str) &&
			   entry.key.hash == hash &&
			   strings.compare(entry.key.chars, str) == 0) {
			return entry.key
		}

		index = (index + 1) % u32(table.capacity)
	}
}

@(private = "file")
grow_capacity :: proc(capacity: int) -> int {
	return capacity < 8 ? 8 : capacity * 2
}

/* Print out a string representation of the table. For debug purposes. */
table_stringify :: proc(table: ^Table) {
	for i in 0 ..< table.capacity {
		entry := &table.entries[i]
		if entry.key != nil {
			fmt.eprintf("key: %s, value: %v\n", entry.key.chars, entry.value)
		}
	}
}
