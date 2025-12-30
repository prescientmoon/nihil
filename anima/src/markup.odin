package anima

import "core:time"

Block_Markup :: struct {
	kind:    enum {
		Paragraph,
		Heading,
		Table,
		Figure,
		Toc,
		Codeblock,
		UList,
		OList,
		Aside,
		Embed_Description,
		Blockquote,
		Page_Index,
		Thematic_Break,
		// We keep link defs around in the source, since they might get included in
		// the output for low-end formats like gemini
		Linkdef,
		// We keep these around for the same reason as link definitions
		Fndef,
	},
	using _: struct #raw_union {
		paragraph:  ^Inline_Markup,
		heading:    ^Heading,
		table:      struct {
			caption: ^Inline_Markup,
			head:    Exparr(^Inline_Markup, 3),
			rows:    Exparr(Exparr(^Inline_Markup, 3), 3),
		},
		image:      struct {
			alt: ^Inline_Markup,
			src: string,
		},
		figure:     struct {
			caption: ^Inline_Markup,
			content: ^Block_Markup,
		},
		linkdef:    ^Linkdef,
		fndef:      ^Fndef,
		codeblock:  struct {
			lang:    string,
			content: string,
		},
		list:       struct {
			items: Exparr(Block_Markup, 3),
		},
		aside:      struct {
			id:        string,
			character: string,
			title:     ^Inline_Markup,
			content:   ^Block_Markup,
			collapse:  bool,
		},
		blockquote: ^Block_Markup,
		page_index: struct {
			filters: Page_Filters,
		},
	},
}

Inline_Markup :: struct {
	kind:    enum {
		Text,
		Emph,
		Strong,
		Strikethrough,
		Mono,
		Datetime,
		Date,
		Link,
		Fn,
		Quote,
		Icon,
		Ellipsis,
		LaTeX,
	},
	using _: struct #raw_union {
		// text, fn, icon, LaTeX
		raw:   string,
		// emph, strong, strikethrough, mono, quote
		inner: ^Inline_Markup,
		// date, datetime
		time:  struct {
			compact: bool,
			time:    time.Time,
		},
		link:  struct {
			id:    string,
			label: ^Inline_Markup,
		},
	},
}
