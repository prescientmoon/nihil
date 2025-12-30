package anima

Chain_Link :: struct($V: typeid) {
	prev: V,
	next: V,
}

Tree_Node :: struct($Node, $Edge, $Leaf: typeid) {
	siblings: Chain_Link(^Tree_Node(Node, Edge, Leaf)),
	parent:   ^Tree_Node(Node, Edge, Leaf),
	edge:     Edge, // The edge from the parent to the node
	kind:     enum {
		Node,
		Leaf,
	},
	using _:  struct #raw_union {
		node: struct {
			data:     Node,
			// "closes" the circular chain of siblings. That is, the "next" value will
			// point to the first child, and the "prev" value will point to the last
			// one.
			children: Chain_Link(^Tree_Node(Node, Edge, Leaf)),
		},
		leaf: Leaf,
	},
}

Tree :: struct($Node, $Edge, $Leaf: typeid) {
	nodes: Exprarr(Tree_Node(Node, Edge, Leaf)),
	roots: Chain_Link(^Tree_Node(Node, Edge, Leaf)),
}

Tree_Builder :: struct($Node, $Edge, $Leaf: typeid) {
	tree:   ^Tree(Node, Edge, Leaf),
	parent: ^Tree_Node(Node, Edge, Leaf),
}

tb_insert :: proc(builder: ^Tree_Builder($N, $E, $L), edge: E) -> ^Tree_Node(N, E, L) {
	node: Tree_Node(N, E, L) = {
		parent = builder.parent,
		edge   = edge,
	}

	node_ptr := exparr_push(&builder.tree.nodes, node)
	chain_link := builder.parent == nil ? &builder.tree.roots : &builder.parent.node.children

	if chain_link.next == nil {
		chain_link.next = node_ptr
		chain_link.prev = node_ptr
	} else {
		chain_link.prev.siblings.next = node_ptr
		chain_link.prev = node_ptr
	}

	return node_ptr
}

tb_leaf :: proc(builder: ^Tree_Builder($N, $E, $L), edge: E, leaf: L) {
	node_ptr := tb_insert(builder, edge)
	node_ptr.kind = .Leaf
	node_ptr.leaf = leaf
}

tb_node_begin :: proc(builder: ^Tree_Builder($N, $E, $L), edge: E, node: N) {
	node_ptr := tb_insert(builder, edge)
	node_ptr.kind = .Node
	node_ptr.node.data = node
	builder.parent = node_ptr
}

tb_node_end :: proc(builder: ^Tree_Builder($N, $E, $L), edge: E, node: N) {
	builder.parent = builder.parent.parent
}
