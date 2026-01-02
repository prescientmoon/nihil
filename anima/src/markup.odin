package anima

import "core:time"

Block_Markup :: struct {
	kind:    enum {
		None = 0,
		Paragraph, // done
		Heading, // done
		Table,
		Image, // done
		Figure,
		Toc, // done
		Codeblock,
		UList,
		OList,
		Aside, // done
		Embed_Description, // done
		Blockquote, // done
		Page_Index,
		Thematic_Break, // done
		Many, // done
		// We keep link defs around in the source, since they might get included in
		// the output for low-end formats like gemini
		Linkdef,
		// We keep these around for the same reason as link definitions
		Fndef,
	},
	using _: struct #raw_union {
		paragraph:  ^Inline_Markup `raw_union_tag:"kind=Paragraph"`,
		heading:    ^Heading `raw_union_tag:"kind=Heading"`,
		table:      struct {
			caption: ^Inline_Markup,
			head:    Exparr(^Inline_Markup, 3),
			rows:    Exparr(Exparr(^Inline_Markup, 3), 3),
		} `raw_union_tag:"kind=Table"`,
		image:      struct {
			alt: ^Inline_Markup,
			src: string,
		} `raw_union_tag:"kind=Image"`,
		figure:     struct {
			caption: ^Inline_Markup,
			content: ^Block_Markup,
		} `raw_union_tag:"kind=Figure"`,
		linkdef:    ^Linkdef `raw_union_tag:"kind=Linkdef"`,
		fndef:      ^Fndef `raw_union_tag:"kind=Fndef"`,
		codeblock:  struct {
			lang:    string,
			content: string,
		} `raw_union_tag:"kind=Codeblock"`,
		list:       struct {
			items: Exparr(Block_Markup, 3),
		} `raw_union_tag:"kind=UList,OList"`,
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
