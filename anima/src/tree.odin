package anima

Chain_Link :: struct($V: typeid) {
	prev: V,
	next: V,
}

Tree_Node :: struct($Node: typeid) {
	siblings: Chain_Link(^Tree_Node(Node)),
	parent:   ^Tree_Node(Node),
	data:     Node,
	// "closes" the circular chain of siblings. That is, the "next" value will
	// point to the first child, and the "prev" value will point to the last
	// one.
	children: Chain_Link(^Tree_Node(Node)),
}

Tree :: struct($Node: typeid) {
	nodes: Exparr(Tree_Node(Node)),
	roots: Chain_Link(^Tree_Node(Node)),
}

Tree_Builder :: struct($Node: typeid) {
	tree:   ^Tree(Node),
	parent: ^Tree_Node(Node),
}

@(private = "file")
tb_insert :: proc(builder: ^Tree_Builder($N)) -> ^Tree_Node(N) {
	node: Tree_Node(N) = {
		parent = builder.parent,
	}

	node_ptr := exparr_push(&builder.tree.nodes, node)
	chain_link := builder.parent == nil ? &builder.tree.roots : &builder.parent.children

	if chain_link.next == nil {
		chain_link.next = node_ptr
		chain_link.prev = node_ptr
	} else {
		chain_link.prev.siblings.next = node_ptr
		chain_link.prev = node_ptr
	}

	return node_ptr
}

tb_leaf :: proc(builder: ^Tree_Builder($N), data: N) -> ^N {
	node_ptr := tb_insert(builder)
	node_ptr.data = data
	return &node_ptr.data
}

tb_node_begin :: proc(builder: ^Tree_Builder($N), data: N) -> ^N {
	node_ptr := tb_insert(builder)
	node_ptr.data = data
	builder.parent = node_ptr
	return &node_ptr.data
}

tb_node_end :: proc(builder: ^Tree_Builder($N)) {
	builder.parent = builder.parent.parent
}

tb_make :: proc(tree: ^Tree($Node)) -> (builder: Tree_Builder(Node)) {
	builder.tree = tree
	return builder
}
