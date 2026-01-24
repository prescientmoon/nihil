package anima

import "core:time"

Absolute_Path :: distinct string
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
  redirects:       Exparr(Absolute_Path)
}

Page_Filters :: struct {
	// The directory of which to include the direct children of
	children:    Absolute_Path,

	// The directory of which to include the (possibly indirect) children of
	descendants: Absolute_Path,

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

// Arbitrary markup saved into a variable using the \def apparition. Can be
// unhygienically inserted anywhere using the \use apparition.
Markup_Declaration :: struct {
	name:     string,
	// Contains what remains of the contents after removing the macro's metadata
	// (thins like the \as apparition, which states the name of the declaration).
	//
	// The roots are part of the storage below, but everything else is merely a
	// reference to the nodes in the pre-existing tree.
	contents: Apt_Storage,
}

Page :: struct {
	path:         Os_Path, // Where is this file currently at?
	route:        Relative_Path, // Where are we planning to place this?
	raw:          string, // The raw file-contents, as read off disk
	apt:          Apt_Storage,
	declarations: Exparr(Markup_Declaration),
	content:      Block_Markup,
	word_count:   uint,
	config:       Page_Config,
	toc:          Exparr(Heading), // The first heading always dictates the title
	footnotes:    Exparr(Fndef),
	links:        Exparr(Linkdef),
	feeds:        Exparr(Rss_Feed),
	changelog:    Exparr(Change),
}
