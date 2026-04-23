#+private file
package anima

import "core:fmt"
import "core:strings"

// Other ad-hoc pretty printers
// {{{ Parsing errors
@(private="package")
pretty_error :: proc(error: Error) -> string {
  builder: strings.Builder
  strings.builder_init_none(&builder, context.temp_allocator)
  fmt.sbprintf(&builder, "%v: %v", error.loc, error.msg)
  return strings.to_string(builder)
}
// }}}
