package desirae

import "core:os"
import "core:flags"
import "core:log"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"

package_name: string
src_root: string
out_root: string
builder: strings.Builder

process_dir :: proc(directory: string) {
  file, err := os.open(directory)
  log.assertf(err == nil, "Failed to read dir %v: %v", directory, err)
  defer {
    err := os.close(file)
    log.assertf(err == nil, "Failed to close dir %v: %v", directory, err)
  }

  iter := os.read_directory_iterator_create(file)
  defer os.read_directory_iterator_destroy(&iter)

  for info in os.read_directory_iterator(&iter) {
    if path, err := os.read_directory_iterator_error(&iter); err != nil {
      log.panic("Failed to read file %v: %v", path, err)
    }

    path := info.fullpath

    // NOTE: we do not handle symlinks
    #partial switch info.type {
    case .Directory:
      process_dir(directory)
    case .Regular:
      if os.ext(path) == ".odin" {
        process_file(path)
      }
    }
  }
}

is_ident_rune :: proc(r: rune) -> bool {
  return unicode.is_alpha(r) || unicode.is_digit(r) || r == '›' || r == '_'
}

process_file :: proc(path: string) {
  log.debugf("Processing '%v'", path)
  bytes, err := os.read_entire_file_from_path(path, context.allocator)
  log.assertf(err == nil, "Failed to read file %v: %v", path, err)

  content := string(bytes)
  strings.builder_reset(&builder)

  found_package_line := false
  for line in strings.split_iterator(&content, "\n") {
    if !found_package_line && strings.has_prefix(line, "package") {
      fmt.sbprintf(&builder, "package %v", package_name)
      found_package_line = true
      continue
    }

    // Attempt to parse the line as as a declaration
    decl: {
      i := 0
      for r in line[i:] {
        (is_ident_rune(r)) or_break
        i += utf8.rune_size(r)
      }

      name := line[:i]
      (len(name) > 0) or_break decl

      for r, w in line[i:] {
        (r == ' ') or_break
        i += utf8.rune_size(r)
      } 

      if !strings.has_prefix(line[i:], "::") do break decl

      // We found a declaration!
      log.debugf("Found declaration %v!", name)
    }

    strings.write_string(&builder, line)
    strings.write_rune(&builder, '\n')
	}

  log.assert(src_root != "")
  strings.builder_replace_all(&builder, "$src", src_root)
  strings.builder_replace_all(&builder, "›", "__")

  relative, rerr := os.get_relative_path(src_root, path, context.allocator)
  log.assertf(
    rerr == nil, "Failed to make %v relative to %v: %v", path, src_root, rerr
  )

  full_path, jerr := os.join_path({out_root, relative}, context.allocator)
  log.assert(jerr == nil)

  dir, _ := os.split_path(full_path)
  if !os.exists(dir) {
    err := os.make_directory_all(dir)
    log.assertf(err == nil, "Failed to create dir %v: %v", dir, err)
  }

  err = os.write_entire_file_from_string(full_path, strings.to_string(builder))
  log.assertf(err == nil, "Failed to write file %v: %v", full_path, err)
}

main :: proc() {
  context.logger = log.create_console_logger()
  context.allocator = context.temp_allocator

  Options :: struct {
    out_dir: string `args:"pos=0,required" usage:"Output directory"`,
    src_dirs: [dynamic]string `args:"name=src,required=1" usage:"Source directories"`,
    package_name: string `args:"name=package,required" usage="Package name"`
  }

  opt: Options
	flags.parse_or_exit(&opt, os.args, .Odin)

  log.debug(opt)

  package_name = opt.package_name
  out_root = opt.out_dir
  for src in opt.src_dirs {
    src, err := os.get_absolute_path(src, context.allocator)
    log.assertf(err == nil, "Failed to make %v absolute: %v", src, err)
    src_root = src
    process_dir(src)
  }
}
