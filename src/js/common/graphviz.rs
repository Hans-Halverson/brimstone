use std::fmt;

use hashbrown::HashMap;

/// A small, general-purpose Graphviz DOT file builder.
pub struct DotGraphBuilder {
    name: String,
    nodes: HashMap<String, DotNode>,
    edges: Vec<DotEdge>,
    graph_attributes: Vec<(String, String)>,
    node_default_attributes: Vec<(String, String)>,
    edge_default_attributes: Vec<(String, String)>,
    graph_label_text_align: DotTextAlign,
}

/// A node in a DotGraphBuilder. Nodes are identified by their `id` field.
pub struct DotNode {
    id: String,
    attributes: Vec<(String, String)>,
    text_align: DotTextAlign,
}

/// A directed edge in a DotGraphBuilder.
pub struct DotEdge {
    from: String,
    to: String,
    attributes: Vec<(String, String)>,
}

#[derive(Clone, Copy)]
pub enum DotTextAlign {
    Left,
    Center,
}

impl DotGraphBuilder {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            nodes: HashMap::new(),
            edges: vec![],
            graph_attributes: vec![],
            node_default_attributes: vec![],
            edge_default_attributes: vec![],
            graph_label_text_align: DotTextAlign::Center,
        }
    }

    /// Add and return a node with the given id.
    pub fn add_node(&mut self, id: &str) -> &mut DotNode {
        let node = DotNode {
            id: id.to_owned(),
            attributes: vec![],
            text_align: DotTextAlign::Center,
        };

        let had_entry = self.nodes.insert(id.to_owned(), node).is_some();
        assert!(!had_entry, "Duplicate graphviz node id");

        self.nodes.get_mut(id).unwrap()
    }

    /// Add and return an edge between two node ids.
    pub fn add_edge(&mut self, from: &str, to: &str) -> &mut DotEdge {
        self.edges
            .push(DotEdge { from: from.to_owned(), to: to.to_owned(), attributes: vec![] });
        self.edges.last_mut().unwrap()
    }

    /// Get a mutable reference to a node by id.
    pub fn get_node(&mut self, id: &str) -> &mut DotNode {
        self.nodes.get_mut(id).unwrap()
    }

    /// Set a graph-level attribute.
    pub fn set_graph_attribute(&mut self, key: &str, value: &str) -> &mut Self {
        self.graph_attributes
            .push((key.to_owned(), value.to_owned()));
        self
    }

    /// Set a default attribute applied to every node.
    pub fn set_node_default_attribute(&mut self, key: &str, value: &str) -> &mut Self {
        self.node_default_attributes
            .push((key.to_owned(), value.to_owned()));
        self
    }

    /// Set a default attribute applied to every edge.
    pub fn set_edge_default_attribute(&mut self, key: &str, value: &str) -> &mut Self {
        self.edge_default_attributes
            .push((key.to_owned(), value.to_owned()));
        self
    }

    /// Set the text alignment of the graph's label.
    pub fn set_graph_label_text_align(&mut self, text_align: DotTextAlign) -> &mut Self {
        self.graph_label_text_align = text_align;
        self
    }
}

impl DotNode {
    /// Attach an attribute to this node.
    pub fn attribute(&mut self, key: &str, value: &str) -> &mut Self {
        self.attributes.push((key.to_owned(), value.to_owned()));
        self
    }

    /// Set text alignment for this node's label.
    pub fn text_align(&mut self, text_align: DotTextAlign) -> &mut Self {
        self.text_align = text_align;
        self
    }
}

impl DotEdge {
    /// Attach an attribute to this edge.
    pub fn attribute(&mut self, key: &str, value: &str) -> &mut Self {
        self.attributes.push((key.to_owned(), value.to_owned()));
        self
    }
}

impl fmt::Display for DotGraphBuilder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "digraph {} {{", quote(&self.name))?;

        for (key, value) in &self.graph_attributes {
            let text_align = if key == "label" {
                self.graph_label_text_align
            } else {
                DotTextAlign::Center
            };

            writeln!(f, "  {key}={};", quote_with_text_align(value, text_align))?;
        }

        if !self.node_default_attributes.is_empty() {
            write!(f, "  node")?;
            write_attributes(f, &self.node_default_attributes)?;
            writeln!(f, ";")?;
        }

        if !self.edge_default_attributes.is_empty() {
            write!(f, "  edge")?;
            write_attributes(f, &self.edge_default_attributes)?;
            writeln!(f, ";")?;
        }

        // Iterate over nodes sorted by their keys for deterministic ordering
        let mut node_entries = self.nodes.iter().collect::<Vec<_>>();
        node_entries.sort_by_key(|(key, _)| *key);

        for (_, node) in node_entries {
            write!(f, "  {}", quote(&node.id))?;
            write_node_attributes(f, node)?;
            writeln!(f, ";")?;
        }

        for edge in &self.edges {
            write!(f, "  {} -> {}", quote(&edge.from), quote(&edge.to))?;
            write_attributes(f, &edge.attributes)?;
            writeln!(f, ";")?;
        }

        write!(f, "}}")
    }
}

fn write_attributes(f: &mut fmt::Formatter<'_>, attributes: &[(String, String)]) -> fmt::Result {
    if attributes.is_empty() {
        return Ok(());
    }

    write!(f, " [")?;
    for (i, (key, value)) in attributes.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{key}={}", quote(value))?;
    }
    write!(f, "]")
}

fn write_node_attributes(f: &mut fmt::Formatter<'_>, node: &DotNode) -> fmt::Result {
    if node.attributes.is_empty() {
        return Ok(());
    }

    write!(f, " [")?;
    for (i, (key, value)) in node.attributes.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }

        // Node text alignment applies only to label attribute
        let text_align = if key == "label" {
            node.text_align
        } else {
            DotTextAlign::Center
        };

        write!(f, "{key}={}", quote_with_text_align(value, text_align))?;
    }
    write!(f, "]")
}

/// Wrap a string in double quotes and escape the necessary characters for the DOT language.
fn quote(str: &str) -> String {
    quote_with_text_align(str, DotTextAlign::Center)
}

fn quote_with_text_align(str: &str, text_align: DotTextAlign) -> String {
    let mut out = String::with_capacity(str.len() + 2);
    out.push('"');

    for c in str.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => match text_align {
                DotTextAlign::Left => out.push_str("\\l"),
                DotTextAlign::Center => out.push_str("\\n"),
            },
            _ => out.push(c),
        }
    }

    out.push('"');
    out
}

pub const DOTFILE_EXTENSION: &str = "dot";
