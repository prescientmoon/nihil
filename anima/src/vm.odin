#+private file
package anima
//
// import "core:encoding/endian"
// import "core:log"
// import "core:mem"
//
// // {{{ VM implementation
// Op :: enum u8 {
//   CT_NOOP = 0,
//   CT_JUMP,   // Set the instruction pointer
//   CT_JIT,    // Jumps if the given value is non-zero
//   CT_JIF,    // Jumps if the given value is zero
//   CT_CALL,   // Swap the stack top with the current instruction pointer
//   AL_POP,    // Discard the top of the stack
//   AL_IMM,    // Push a new constant onto the stack (consumes 8 bytes)
//   AL_COPY,   // Copy the value at a given offset from the top of the stack
//   AL_AND,    // Bitwise AND
//   AL_ADD,    // Wrapping integer addition
//   AL_GT,     // Logical greater than
//   SR_WHEN,   // Gets the current tick
//   SR_ANCHOR, // Gets the current instruction pointer
//   SR_COL,    // Gets the last column we parsed at
//   SR_ATE,    // True when the last parser consumed
//   SR_LAST,   // Gets the last tick we consumed at, including spaces
//   SR_LASTNS, // Gets the last tick we consumed at, excluding spaces
//   SR_PAR,    // Sets the paragraph flag
//   SR_SETIDT, // Sets the current indentation
//   SR_GETIDT, // Gets the current indentation
//   WP_GET,    // Get the world pointer
//   WP_SET,    // Set the world pointer
//   WP_DEREF,  // Dereference a pointer, allocating if it is null
//   WP_PUSH,   // Intrepret the world pointer as an exparr and push to it
//   WP_WRITE,  // Write N=1/2/4/8 bytes to the world pointer
//   WP_READ,   // Read  N=1/2/4/8 bytes from the world pointer
//   PS_ERR,
//   PS_TEXT,
//   PS_SPACE,
//   PS_APP,    // Parse an apparition with the given content
//   PS_TOK,    // Parse a spcific token kind
//   PS_RAW,
//   PS_TIME,
//   PS_BYTE,
// }
//
// VM_Flag :: enum {
//   Consumed,
//   Paragraph,
// }
//
//
// // The byte order observed by the VM
// BYTE_ORDER: endian.Byte_Order : .Little
// VM_STACK_SIZE :: 1024
//
// VM :: struct {
//   site:             ^Site,
//   tick:             u64,
//   last_consumption: u64,
//   instruction:      u64,
//   indentation:      u64,
//   column:           u64,
//   stack:            [dynamic; VM_STACK_SIZE]u64,
//   world_pointer:    rawptr,
//   flags:            bit_set[VM_Flag],
// }
//
// spush :: #force_inline proc(vm: ^VM, data: u64) {
//   append(&vm.stack, data)
// }
//
// spop :: #force_inline proc(vm: ^VM) -> u64 {
//   return pop(&vm.stack)
// }
//
// run :: proc(vm: ^VM, bytecode: []byte) {
//   for {
//     (int(vm.instruction) < len(bytecode)) or_break // can be optimized
//     op := Op(bytecode[vm.instruction])
//     switch op {
//     case .SR_WHEN:
//       spush(vm, vm.tick)
//       vm.instruction += 1
//     case .SR_ANCHOR:
//       spush(vm, vm.instruction)
//       vm.instruction += 1
//     case .SR_COL:
//       spush(vm, vm.column)
//       vm.instruction += 1
//     case .SR_ATE:
//       spush(vm, u64(.Consumed in vm.flags))
//       vm.instruction += 1
//     case .SR_LAST:
//       spush(vm, vm.last_consumption)
//       vm.instruction += 1
//     case .SR_PAR:
//       if spop(vm) == 0 do vm.flags -= { .Paragraph }
//       else do vm.flags += { .Paragraph }
//       vm.instruction += 1
//     case .SR_SETIDT:
//       vm.indentation = spop(vm)
//       vm.instruction += 1
//     case .SR_GETIDT:
//       spop(vm, vm.indentation)
//       vm.instruction += 1
//     case .CT_NOOP:
//       vm.instruction += 1
//     case .CT_JUMP:
//       vm.instruction = spop(vm)
//     case .CT_JIT:
//       if spop(vm) != 0 do vm.instruction = spop(vm)
//       else do vm.instruction += 1
//     case .CT_JIF:
//       if spop(vm) == 0 do vm.instruction = spop(vm)
//       else do vm.instruction += 1
//     case .CT_CALL:
//       loc := spop(vm)
//       spush(vm, vm.instruction + 1)
//       vm.instruction = loc
//     case .AL_IMM:
//       v, ok := endian.get_u64(bytecode[vm.instruction + 1:], BYTE_ORDER)
//       log.assert(ok)
//       spush(vm, v)
//       vm.instruction += 9
//     case .AL_COPY:
//       offset := spop(vm)
//       spush(vm, vm.stack[len(vm.stack) - 1 - int(offset)])
//       vm.instruction += 1
//     case .AL_AND:
//       a := spop(vm)
//       b := spop(vm)
//       spush(vm, a & b)
//       vm.instruction += 1
//     case .AL_ADD:
//       a := spop(vm)
//       b := spop(vm)
//       spush(vm, a + b)
//       vm.instruction += 1
//     case .AL_GT:
//       a := spop(vm)
//       b := spop(vm)
//       spush(vm, u64(a > b))
//       vm.instruction += 1
//     case .WP_GET:
//       spush(vm, u64(uintptr(vm.world_pointer)))
//       vm.instruction += 1
//     case .WP_SET:
//       vm.world_pointer = rawptr(uintptr(spop(vm)))
//       vm.instruction += 1
//     case .WP_DEREF:
//       size   := spop(vm)
//       align  := spop(vm)
//       ptr    := cast(^rawptr)vm.world_pointer
//
//       if ptr^ == nil {
//         layout := Layout{ uint(size), uint(align) }
//         alloc  := site__alloc(vm.site)
//         ptr    ^= mem__alloc(layout, alloc)
//       }
//
//       vm.world_pointer = ptr^
//       vm.instruction += 1
//     case .WP_PUSH:
//       ptr    := cast(^Exparr__Repr)rawptr(uintptr(spop(vm)))
//       size   := spop(vm)
//       align  := spop(vm)
//       FCE    := spop(vm)
//       layout := Layout{ uint(size), uint(align) }
//       slot   := exparr__repr__push(ptr, uint(FCE), layout)
//       mem.copy(slot, vm.world_pointer, int(layout.size))
//       vm.world_pointer = ptr
//       vm.instruction += 1
//     case .WP_WRITE:
//       amount := spop(vm)
//       switch amount {
//       case 1: (cast(^u8) vm.world_pointer)^ =  u8(spop(vm))
//       case 2: (cast(^u16)vm.world_pointer)^ = u16(spop(vm))
//       case 4: (cast(^u32)vm.world_pointer)^ = u32(spop(vm))
//       case 8: (cast(^u64)vm.world_pointer)^ = u64(spop(vm))
//       }
//       vm.instruction += 1
//     case .WP_READ:
//       amount := spop(vm)
//       switch amount {
//       case 1: spush(vm, u64(mem.reinterpret_copy(u8,  vm.world_pointer)))
//       case 2: spush(vm, u64(mem.reinterpret_copy(u16, vm.world_pointer)))
//       case 4: spush(vm, u64(mem.reinterpret_copy(u32, vm.world_pointer)))
//       case 8: spush(vm, u64(mem.reinterpret_copy(u64, vm.world_pointer)))
//       }
//       vm.instruction += 1
//     case .PS_ERR:
//     case .PS_TEXT:
//     case .PS_SPACE:
//     case .PS_APP:
//     case .PS_TOK:
//     case .PS_RAW:
//     case .PS_TIME:
//     case .PS_BYTE:
//     }
//   }
// }
// // }}}
//
// Place :: distinct u64 // instruction pointer
// Label :: distinct u64
// Region :: struct { start, end: Label }
//
// MAX_LABELS          :: 1024
// MAX_REGIONS         :: 1024
// INSTRUCTION_BYTES    :: 1024
// MAX_LABEL_CONSTANTS :: 1024
//
// ANIMA_VM_DEBUG :: #config(ANIMA_VM_DEBUG, false)
//
// when ANIMA_VM_DEBUG {
//   Assembler_Debug_Data :: struct {
//     label_names:  [dynamic;MAX_LABELS]string,
//     comments:     [dynamic;INSTRUCTION_BYTES]string,
//   }
// } else {
//   Assembler_Debug_Data :: struct {}
// }
//
// Assembler :: struct {
//   site:            ^Site,
//   labels:          [dynamic;MAX_LABELS]Place,
//   regions:         [dynamic;MAX_ERGIONS]Region,
//   instructions:    [dynamic;INSTRUCTION_BYTES]byte,
//   label_constants: [dynamic;MAX_LABEL_CONSTANTS]Label_Constant,
//   // How deep is the stack right now? (relative to the start of the
//   // "function call")
//   depth: int,
//   using _: Assembler_Debug_Data,
// }
//
// // A constant containing a label. Will get filled up once the label gets
// // resolved.
// Label_Constant :: struct {
//   label: Label,
//   at:    Place,
// }
//
// // Represents a piece of input that we can use as the smallest unit of syntax.
// // These are the kind of signals the parsing will be guided by.
// Atom :: union {
//   string,     // Apparition with the given content
//   Token_Kind, // Token with the given data
// }
//
// Codec__Field__Kind :: enum { Flag, Exparr, Once, Some, Maybe }
//
// Codec__Field :: struct {
//   name:  string,
//   at:    Atom,
//   kind:  Codec__Field__Kind,
//   codec: ^Codec,
// }
//
// MAX_FIELDS :: 32
// Codec__Struct :: struct {
//   type: typeid,
//   fields: [dynamic; MAX_FIELDS]Codec__Field,
// }
//
// Codec__Variant :: struct {
//   branch: typeid,
//   at:     Atom,
//   codec:  ^Codec,
// }
//
// Codec__Union :: struct {
//   type: typeid,
//   variants: [dynamic; MAX_FIELDS]Codec__Variant,
// }
//
// Codec__Primitive :: enum {
//   Text, Space, Raw, Time, Byte,
// }
//
// Codec :: union {
//   Codec__Primitive,
//   Codec__Struct,
//   Codec__Union,
// }
//
// @rodata
// OP_STACK_DIFF := [Op]int {
//   .CT_NOOP = 0,
//   .CT_JUMP = -1,
//   .CT_JIT = -2,
//   .CT_JIF = -2,
//   .CT_CALL = -1, // The called code is expected to pop the IP off the stack
//   .AL_POP =  -1,
//   .AL_IMM =  1,
//   .AL_COPY = 0,
//   .AL_AND =  -1,
//   .AL_ADD =  -1,
//   .AL_GT =   -1,
//   .SR_WHEN =  1,
//   .SR_ANCHOR = 1,
//   .SR_COL =    1,
//   .SR_ATE =    1,
//   .SR_LAST =   1,
//   .SR_LASTNS = 1,
//   .SR_PAR =    1,
//   .SR_SETIDT = -1,
//   .SR_GETIDT = 1,
//   .WP_GET =    1,
//   .WP_SET =    -1,
//   .WP_DEREF =  -2,
//   .WP_PUSH =   -3,
//   .WP_WRITE =  -2,
//   .WP_READ =   0,
//   .PS_ERR =    -2,
//   .PS_TEXT =   0,
//   .PS_SPACE =  0,
//   .PS_APP =    -1,
//   .PS_TOK =    -1,
//   .PS_RAW =    0,
//   .PS_TIME =   0,
//   .PS_BYTE =   0,
// }
//
// op :: proc(a: ^Assembler, i: Op, comment := "", diff := 0) -> Place {
//   a.depth += OP_STACK_DIFF[op] + diff
//   place := Place(len(a.instructions))
//   push(a.instructions, byte(i))
//   when ANIMA_VM_DEBUG {
//     push(&a.comments, comment)
//     log.assert(len(a.comments) == len(a.instructions))
//   }
//
//   return place
// }
//
// // Generate code for pushing a single constant onto the stack
// constant :: proc(a: ^Assembler, #any_int v: u64, comment: string) -> Place {
//   place := op(a, .AL_IMM, comment)
//   encoded: [8]byte
//   endian.put_u64(&encoded, BYTE_ORDER, v)
//   append(a.instructions, ..encoded)
//   return place
// }
//
// // Generate code for pushing a reference to an odin string onto the stack
// string :: proc(a: ^Assembler, str: string) {
//   rstr := transmute(mem.Raw_String)str
//   constant(a, str.len, "string length")
//   constant(a, uintptr(str.data), "string data")
// }
//
// label :: proc(a: ^Assembler, label: Label) -> Place {
//   place := constant(a, 0, a.label_names[label])
//   push(&a.label_constants, Label_Constant{label, Place(u64(place) + 1)})
//   return op
// }
//
// mklabel :: proc(a: ^Assembler, name: string) -> Label {
//   label := Label(len(a.labels))
//   push(a.labels, {}) // not resolved yet!
//
//   when ANIMA_VM_DEBUG {
//     push(&a.label_names, name)
//     log.assert(len(a.label_names) == len(a.labels))
//   }
//
//   return label
// }
//
// resolve :: proc(a: ^Assembler, label: Label) -> {
//   log.assert(a.labels[label] == {})
//   a.labels[label] = Instruction(len(a.instructions))
//   when ANIMA_VM_DEBUG {
//     op(a, .CT_NOOP, a.label_names[label])
//   }
// }
//
// comment :: proc(a: ^Assembler, fstr: string, args: ..any) -> string {
//   return fmt.aprintf(
//     fstr,
//     ..args,
//     allocator = site__alloc(a.site)
//   ) when ANIMA_VM_DEBUG else ""
// }
//
// assemble_atom :: proc(a: ^Assembler, atom: Atom) {
//   switch inner in atom {
//   case Token_Kind:
//     // maybe include a token kind label here?
//     constant(a, u64(inner), "The kind of token to parse")
//     op(a, .PS_TOK, "parse a token")
//   case string:
//     // TODO: how do we push strings?
//   case nil:
//     log.panic("Cannot assemble nil atom")
//   }
// }
//
// @(deferred_in_out=ASSEMBLER_DEPTH_SCOPE_END)
// ASSEMBLER_DEPTH_SCOPE :: proc(a: ^Assembler, diff: int) -> (depth: int) {
//   depth = a.depth
//   a.depth = 0
// }
//
// ASSEMBLER_DEPTH_SCOPE_END :: proc(a: ^Assembler, diff, depth: int) {
//   log.assert(a.depth == diff, "wrong depth diff")
//   a.depth = depth
// }
//
// assemble_codec :: proc(
//   a: ^Assembler,
//   codec: Codec,
//   // Immediately stop on occurrences of this token
//   closer: Token_Kind = .None,
// ) {
//   switch inner in codec {
//   case Codec__Primitive:
//     switch inner {
//     case .Text:  op(a, .PS_TEXT)
//     case .Space: op(a, .PS_SPACE)
//     case .Raw:   op(a, .PS_RAW)
//     case .Time:  op(a, .PS_TIME)
//     case .Byte:  op(a, .PS_BYTE)
//     }
//   case Codec__Struct:
//     site__frame(a.site)
//
//     pre_loop_depth := a.depth
//     defer log.assert(a.depth == pre_loop_depth)
//
//     comment := fmt.aprintf(
//       "Struct %v",
//       inner.type,
//       allocator = site__alloc(a.site)
//     ) when ANIMA_VM_DEBUG else ""
//
//     loop_start  := mklabel(a, comment(a, "Struct %v", inner.type))
//     loop_cleanup := mklabel(a, comment(a, "Cleanup %v", inner.type))
//     loop_next := mklabel(a, comment(a, "Next %v", inner.type))
//     loop_end    := mklabel(a, comment(a, "Out of the loop for %v", inner.type))
//
//     op(.SR_WHEN)
//     tick_start := a.depth
//
//     // Completion tracking
//     field_tracking_map := make(
//       map[string]uint,
//       site__alloc(a.site, .Stack)
//     )
//
//     for field in inner.fields {
//       if field.name not_in field_tracking_map {
//         field_tracking_map[field.name] = len(field_tracking_map)
//       }
//     }
//
//     field_tracking_slot_count := (len(field_tracking_map) + 63) / 64
//     field_tracking_slot_map := make(
//       []uint,
//       field_tracking_slot_count,
//       site__alloc(a.site, .Stack)
//     )
//
//     for i in 0..<field_tracking_slot_count {
//       constant(a, 0, "Completion tracking slot")
//       field_tracking_slot_map[i] = a.depth
//     }
//
//     for field in inner.fields {
//       starting_depth := a.depth
//       defer log.assert(a.depth == starting_depth)
//
//       iter_end  := mklabel(a, comment(a, "End of %v", field.name))
//       if field.at == {} {
//         assemble_codec(a, inner.codec)
//       } else {
//         assemble_atom(a, field.at)
//         op(a, .SR_ATE)
//         label(a, iter_end)
//         op(a, .CT_JIF, "Failed to parse leader, moving to the next field")
//
//         bracket_start := mklabel(a, "Start of bracketed branch")
//         bang_start    := mklabel(a, "Start of the bang branch")
//         indent_start  := mklabel(a, "Start of indented branch")
//         alt_end       := mklabel(a, "End of alternatives")
//
//         label(a, bracket_start)
//         op(a, .CT_JUMP, "Jump over the content and to the first branch")
//
//         // Content
//         content_start := mklabel(a, "Content start")
//         resolve(a, content_start)
//         assemble_codec(a, inner.codec)
//         op(a, .CT_JUMP, "Return from content")
//
//         // Bracket branch
//         resolve(a, bracket_start)
//
//         // {
//         assemble_atom(a, .LCurly)
//         op(a, .SR_ATE)
//         label(a, bang_start)
//         op(a, .CT_JIF, "Failed to parse {, maybe a bang will work")
//
//         // { ... }
//         label(a, content_start)
//         op(a, .CT_CALL, "Call the content parser")
//
//         // }
//         after_rcurly_error := mklabel(a, "After the } error")
//         assemble_atom(a, .RCurly)
//         op(a, .SR_ATE)
//         label(a, after_rcurly_error)
//         op(a, .CT_JIT, "Skip the error code if the } was parsed correctly")
//         string(a, "Expected }.")
//         op(a, .PS_ERR)
//         resolve(a, after_rcurly_error)
//         label(a, alt_end)
//         op(a, .CT_JUMP, "Skip to the next loop iteration")
//
//         // Bang branch
//         resolve(a, bang_start)
//         assemble_atom(a, .Bang)
//         op(a, .SR_ATE)
//         label(a, indent_start)
//         op(a, .CT_JIF, "Failed to parse bang, indentation it is")
//         label(a, content_start)
//         op(a, .CT_CALL, "Call the content parser")
//         label(a, alt_end)
//         op(a, .CT_JUMP, "Skip to the next loop iteration")
//
//         // Indented branch
//         resolve(a, indent_start)
//         op(a, .SR_GETIDT, "Save old indentation")
//         op(a, .SR_COL)
//         op(a, .SR_SETIDT, "Bump indentation")
//         label(a, content_start)
//         op(a, .CT_CALL, "Call the content parser")
//         op(a, .SR_SETIDT, "Restore old indentation")
//
//         resolve(a, alt_end)
//       }
//
//       constant(a, a.depth - tick_start)
//       op(a, .AL_COPY, "The iteration start tick")
//       op(a, .SR_LAST)
//       op(a, .AL_GT, "Have we made any progress since?")
//       label(a, loop_next)
//       op(a, .CT_JIT, "If yes, skip to the next iteration")
//       resolve(a, iter_end)
//     }
//
//     // return address + tick + slots
//     cleanup_diff_total := 1 + 1 + len(field_tracking_slot_count)
//
//     label(a, loop_end)
//     label(a, loop_cleanup)
//     op(a, .JUMP, a, "Clean up and leave the loop", -cleanup_diff_total)
//
//     resolve(a, loop_next)
//     label(a, loop_start)
//     label(a, loop_cleanup)
//     op(a, .JUMP, a, "Clean up and continue the loop", -cleanup_diff_total)
//
//     {
//       ASSEMBLER_DEPTH_SCOPE(a, -cleanup_diff_total)
//       resolve(a, loop_cleanup)
//       op(a, .AL_POP, "Pop the starting tick")
//       label(a, loop_start)
//       op(a, .AL_JUMP, "Return from loop cleanup")
//     }
//
//     resolve(a, loop_end)
//   }
// }
//
// parse :: proc(p: ^Parser, codec: Codec, closer: Token_Kind) {
//   switch inner in codec {
//   case Codec__Primitive:
//     ...
//   case Codec__Plex:
//     slots: u64
//
//     for {
//       tick := p.tick
//       if p.can_import {
//         import_data: ...
//         p.wp = &import_data
//         parse(p, import_codec)
//         if p.last > tick {
//           source := read_file(...)
//           p.source = source
//           parse(p, codec) // pass the slots, somehow
//         }
//       }
//
//       for field in inner.fields {
//         if field.has_tag {
//           set_tag(p.wp, field.tag)
//           // zero out the data too, I guess
//         }
//         p.wp += field.offset
//
//         if field.at == {} {
//           parse(p, inner.codec, closer)
//         } else if field.opener != closer && expect(p, field.opener) {
//           parse(p, inner.codec, field.closer)
//           if !expect(p, field.closer) {
//             error(p, "Expected ...")
//           }
//         } else if expect(p, .Apparition, field.at) {
//           // Try brackets
//           if expect(p, .LCurly) {
//             parse(p, inner.codec, .RCurly)
//             if !expect(p, .RCurly) {
//               error(p, "Expected }")
//             }
//           } else if expect(p, .Bang) {
//             parse(p, inner.codec)
//           } else {
//             old := p.indentation
//             p.indentation = p.col
//             parse(p, inner.codec, closer)
//             p.indentation = old
//           }
//         }
//
//         if p.last > tick do break
//       }
//
//       if p.last <= tick do break
//       if inner.once do break
//     }
//   }
// }
