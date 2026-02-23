package anima

import "core:time"

// Apparition trees
// {{{ Types
Parsing_Error :: struct {
	tok: Token,
	msg: string,
}

Apt_Node_Data :: struct {
	tok:    Token,
	kind:   enum {
		Node,
		Leaf,
	},
	// These fields only exist for non-leaf nodes
	raw:    string,
	errors: Exparr(Parsing_Error),
}

Apt_Builder :: Tree_Builder(Apt_Node_Data)
Apt_Storage :: Tree(Apt_Node_Data)
Apt_Old :: Tree_Node(Apt_Node_Data) // [ap]parition [t]tree
Linked_Apf :: Chain_Link(^Apt_Old) // [ap]parition [f]orest
Exp_Apf :: Exparr(Apt_Old)

Apt_Leaf :: struct {
	tok: Token_Of(enum {
			Word,
			Space,
			Newline,
		}),
}

Apt_Node :: struct {
	backslash: Token,
	name:      Exp_Apf,
	body:      Exp_Apf,
}

Apt_Error :: struct {
	raw:    string,
	errors: Exparr(Parsing_Error),
}

Apt :: struct {
	tok:    Token,
	kind:   enum {
		Node,
		Leaf,
	},
	// These fields only exist for non-leaf nodes
	raw:    string,
	errors: Exparr(Parsing_Error),
}
// }}}
// {{{ Variable system discussion
// Does it get resolved in a single parser pass?
// - if yes:
//   - declarations must be in order
//   - then macros have to be unhygienic
//   - bracketed definitions are not possible
// - if no:
//   - proper scoping can be implemented
//   - declarations/references can be removed from the apparition tree in
//     a separate pass (heck, even comments can be removed like that!)
//
// If I do want to type-check everything, then the implementation would look
// like this:
// 1. lexing
// 2. parsing to an apparition tree
// 3. comment removal
// 4. pick out "def" apparitions, and save them in their scope in the
//    apparition tree
// 5. for each level of the apparition tree, typecheck the apparitions
//    themselves:
//    - the ambiance is not checked yet
//    - check that every apparition is valid inside its context
//    - when encountering an "use" token, ensure the types match, and that
//      the correct arguments are given
//
// Example:
// \def make-row \ty row
//   \arg third-cell \ty block
//   \return
//     \cell First cell
//     \cell Second cell
//     \cell \use third-cell
//     \-- "delay" can be used to resolve something at instantiation time
//     \cell \delay \use fourth-cell
//
// Ok, I'm getting sidetracked... As cool as implementing this would be, I
// really don't need any of this for a personal website. And if, in the future,
// I decide I do, I can simply add it in then.
//
// For now, all I really feel the need for is a simple, per-page yet global,
// order-independent (oof), unhygienic way of declaring variables.
//
// One can even emulate functions (in a scuffed manner) this way, although this
// requires support for local variables:
//
// \def \as make-row
//   \cell First cell
//   \cell Second cell
//   \cell \use third-cell
//
// \table
//   \row \def{\local \as third-cell foo} \use make-row
//   \row \def{\local \as third-cell bar} \use make-row
//
// The order-independence *does* mean we need an apparition tree, although
// this can be accomplished.
//
// Still, for the sake of simplicity, I won't implement locals right now, which
// means one won't be able to emulate functions like above (not like that way
// of emulating locals is particularly pretty either).
//
// Parsing to an apparition tree first also means bracketed definitions are now
// legal! (since the bracket balancing is handled by the tree without knowing
// which apparitions are allowed, and in which context)
// }}}
// {{{ Comment removal
apt_remove_comments :: proc(into: ^Apt_Builder, from: Apt_Old) {
	switch from.data.kind {
	case .Leaf:
		tb_leaf(into, from.data)
	case .Node:
		(from.data.tok.content != "--") or_break
		tb_node_begin(into, from.data)
		apf_remove_comments(into, from.children)
		tb_node_end(into)
	}
}

apf_remove_comments :: proc(into: ^Apt_Builder, apf: Linked_Apf) {
	for next := apf.next; next != nil; next = next.siblings.next {
		apt_remove_comments(into, next^)
	}
}
// }}}

// Fully packaged markup
// {{{ Block
Table :: struct {
	caption: ^Inline_Markup,
	head:    ^Table_Row,
	rows:    Exparr(Table_Row, 3),
}

Table_Row :: struct {
	cells: Exparr(Table_Cell, 3),
}

Table_Cell :: struct {
	content: ^Inline_Markup,
	bg:      string, // TODO: make this more strongly checked?
}

Block_Markup :: struct {
	kind:    enum {
		None = 0,
		Paragraph,
		Heading,
		Table,
		Image,
		Figure,
		Table_Of_Contents,
		Codeblock,
		IList,
		BList,
		Aside,
		Embed_Description,
		Blockquote,
		Page_Index,
		Thematic_Break,
		Many,
		// We keep link defs around in the source, since they might get included in
		// the output for low-end formats like gemini
		Linkdef,
		// We keep these around for the same reason as link definitions
		Fndef,
	},
	using _: struct #raw_union {
		paragraph:  ^Inline_Markup `raw_union_tag:"kind=Paragraph"`,
		heading:    ^Heading `raw_union_tag:"kind=Heading"`,
		table:      Table `raw_union_tag:"kind=Table"`,
		image:      struct {
			alt:    ^Inline_Markup,
			source: string,
		} `raw_union_tag:"kind=Image"`,
		figure:     struct {
			caption: ^Inline_Markup,
			content: ^Block_Markup,
		} `raw_union_tag:"kind=Figure"`,
		linkdef:    ^Linkdef `raw_union_tag:"kind=Linkdef"`,
		fndef:      ^Fndef `raw_union_tag:"kind=Fndef"`,
		codeblock:  struct {
			lang:   string,
			source: string,
		} `raw_union_tag:"kind=Codeblock"`,
		blist:      struct {
			items:   Exparr(Block_Markup, 3),
			ordered: bool,
		} `raw_union_tag:"kind=BList"`,
		ilist:      struct {
			items:   Exparr(Inline_Markup, 3),
			ordered: bool,
		} `raw_union_tag:"kind=IList"`,
		aside:      struct {
			id:        string,
			character: string,
			title:     ^Inline_Markup,
			content:   ^Block_Markup,
			collapse:  bool,
		} `raw_union_tag:"kind=Aside"`,
		blockquote: ^Block_Markup `raw_union_tag:"kind=Blockquote"`,
		page_index: struct {
			filters: Page_Filters,
		} `raw_union_tag:"kind=Page_Index"`,
		many:       Exparr(Block_Markup, 3) `raw_union_tag:"kind=Many"`,
	},
}
// }}}
// {{{ Inline
Inline_Markup :: struct {
	kind:    enum {
		None = 0,
		Space,
		Text,
		Emph,
		Strong,
		Strikethrough,
		Mono,
		Quote,
		Datetime,
		Date,
		Link,
		Fn,
		Icon,
		Ellipsis,
		LaTeX,
		Many,
	},
	using _: struct #raw_union {
		// text, fn, icon, LaTeX
		raw:   string `raw_union_tag:"kind=Text,Space,Fn,Icon,LaTeX"`,
		// emph, strong, strikethrough, mono, quote
		inner: ^Inline_Markup `raw_union_tag:"kind=Emph,Strong,Strikethrough,Mono,Quote"`,
		// date, datetime
		time:  struct {
			compact: bool,
			time:    time.Time,
		} `raw_union_tag:"kind=Date,Datetime"`,
		link:  struct {
			id:    string,
			label: ^Inline_Markup,
		} `raw_union_tag:"kind=Link"`,
		many:  Exparr(Inline_Markup, 3) `raw_union_tag:"kind=Many"`,
	},
}
// }}}
// {{{ Parsing discussion
// NOTE: this is outdated, and is only here for historical purposes :)
//
// Parser for markup:
// - keep pulling text and emitting text fragments
// - when an apparition is encountered, match on it:
//   - metadata aparitions need to be handled by now (what if they're nested?)
//   - markup apparitions emit chunks
// - when in a "block" context, paragraphs need to be emitted
// - again, we collect metadata pre-entively
// - I think we need metadata to be attached somewhat locally, in order to get
//   inclusions to work nicely
// - The metadata included in each page needs to be collected *somehow*,
//   *somewhere*. This includes
// - Actually, we might get away with not collecting it locally, but keeping
//   track of scoping rules
// - Back to parsing... We always have a "working paragraph", that we only close
//   when encountering two or more consecutive newlines
// - If the last paragraph is empty at the end of a block, it is destroyed.
//   Otherwise, it gets closed automatically.
// - Indentation is only important when parsing code blocks, but those are their
//   own topic.
// }}}
