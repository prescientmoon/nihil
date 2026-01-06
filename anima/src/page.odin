package anima

import "core:time"

Relative_Path :: distinct string
Os_Path :: distinct string

Heading :: struct {
	id:       string,
	contents: Inline_Markup,
	// Normally between 1 and 6. A value of 0 indicates a non-existent heading.
	level:    uint,
}

Fndef :: struct {
	id:       string,
	contents: Block_Markup,
}

Linkdef :: struct {
	id:     string,
	label:  Inline_Markup,
	target: string,
}

Page_Config :: struct {
	hidden:             bool,
	draft:              bool,
	compact:            bool,
	sitemap_priority:   f32, // 0: non-existent
	sitemap_changefreq: string,
}

Page_Filters :: struct {
	// The directory of which to include the direct children of
	children:    Relative_Path,

	// The directory of which to include the (possibly indirect) children of
	descendants: Relative_Path,

	// Whether to include hidden pages
	hidden:      bool,
}

// Defines a RSS feed that's going to be a part of the webstie. Such definitions
// can appear anywhere and affect any other pages. There's no hierarchy, so this
// is (in a sense) "global". Although such definitions *could* cause "spooky
// action at a distance", I'm the only user of anima, so it's not something I
// have to lose sleep over.
Rss_Feed :: struct {
	id:         string,
	summary:    Block_Markup,

	// The location to place the RSS feed at in the output directory
	at:         Relative_Path,

	// The items that are part of the actual XML feed
	items:      Page_Filters,

	// The pages the feed will appear as part of the <meta> tags of
	appears_on: Page_Filters,
}

// Manually written page changelog entry. In the past, I would attempt to derive
// these from Git history, but I no longer belive git histories are sacred.
Change :: struct {
	at:   time.Time,
	body: Block_Markup,
}

Page :: struct {
	path:       Os_Path, // Where is this file currently at?
	route:      Relative_Path, // Where are we planning to place this?
	raw:        string, // The raw file-contents, as read off disk
	content:    Block_Markup,
	word_count: uint,
	config:     Page_Config,
	toc:        Exparr(Heading), // The first heading always dictates the title
	footnotes:  Exparr(Fndef),
	links:      Exparr(Linkdef),
	feeds:      Exparr(Rss_Feed),
	changelog:  Exparr(Change),
}

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
