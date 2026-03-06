package anima

import "core:mem/virtual"

// A wrapper around unsigned integers, which prints the integer as a number of 
// bytes in human readable format (i.e. with a B/KB/MB/etc prefix).
Bytes :: distinct uint

Statistics :: struct {
	tokens:                    uint,
	codecs:                    uint,
	codec_evaluations:         uint,
  xml_tags:                  uint,
  xml_attrs:                 uint,
  pages:                     uint,
  directories_visited:       uint,
  files_generated:           uint,

  system_arena:              Bytes,
  site_forever_arena:        Bytes,
  codec_memo_arena:          Bytes,
  codec_arena:               Bytes,
  xml_internal_arena:        Bytes,
  xml_builder_arena:         Bytes,
  parser_internal_arena:     Bytes,
  parser_output_arena:       Bytes,
  parser_codec_output_stack: Bytes,
  parser_codec_state_stack:  Bytes,
}

// Destroy an arena, saving the stats of how much memory it used up
arena__clear :: proc(bytes: ^Bytes, arena: ^virtual.Arena) {
  bytes^ = max(Bytes(arena.total_used), bytes^)
  virtual.arena_free_all(arena)
}

// Destroy an arena, saving the stats of how much memory it used up
arena__destroy :: proc(bytes: ^Bytes, arena: ^virtual.Arena)  {
  bytes^ = max(Bytes(arena.total_used), bytes^)
  virtual.arena_destroy(arena)
}
