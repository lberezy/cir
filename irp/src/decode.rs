use super::{Expression, Irp, Vartable};
use std::char;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;
#[allow(unused_imports)]
use std::path::{Path, PathBuf};

// This the decoder nfa (non-deterministic finite automation)
#[derive(PartialEq, Debug)]
pub enum Edge {
    Flash(i64, usize),
    Gap(i64, usize),
    Repeat(usize),
    Empty(usize),
}

#[derive(PartialEq, Debug)]
pub enum Vertex {
    Nop(Vec<Edge>),
    RepeatCounter(u32, Vec<Edge>),
    Bits(u8, Vec<Edge>),
    Done,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub struct NFA {
    verts: Vec<Vertex>,
}

#[derive(Debug)]
pub struct Matcher<'a> {
    pos: Vec<usize>,
    abs_tolerance: u32,
    rel_tolerance: u32,
    counter: u32,
    bits: u64,
    nfa: &'a NFA,
}

impl Vertex {
    fn add_edge(&mut self, edge: Edge) {
        match self {
            Vertex::Nop(edges) | Vertex::RepeatCounter(_, edges) | Vertex::Bits(_, edges) => {
                edges.push(edge);
            }
            _ => panic!("cannot add edge to {:?} vertex", self),
        }
    }

    fn get_edges(&self) -> &[Edge] {
        match self {
            Vertex::Nop(edge) | Vertex::RepeatCounter(_, edge) | Vertex::Bits(_, edge) => edge,
            Vertex::Done => &[],
        }
    }

    fn get_repeat_edge(&self) -> usize {
        if let Vertex::RepeatCounter(_, edges) = self {
            for e in edges {
                if let Edge::Repeat(dest) = e {
                    return *dest;
                }
            }
        }

        panic!("no repeat edge found");
    }

    fn get_empty_edge(&self) -> Option<usize> {
        for e in self.get_edges() {
            if let Edge::Empty(dest) = e {
                return Some(*dest);
            }
        }

        None
    }
}

impl Irp {
    // Generate a decoder for this IRP
    pub fn decode(&self) -> Result<NFA, String> {
        let mut verts: Vec<Vertex> = vec![Vertex::Nop(vec![])];
        let mut last = 0;

        for expr in &self.stream {
            self.expression(expr, &mut verts, &mut last, &[])?;
        }

        let pos = verts.len();

        verts.push(Vertex::Done);

        verts[last].add_edge(Edge::Empty(pos));

        Ok(NFA { verts })
    }

    fn expression(
        &self,
        expr: &Expression,
        verts: &mut Vec<Vertex>,
        last: &mut usize,
        bit_spec: &[Expression],
    ) -> Result<(), String> {
        match expr {
            Expression::Stream(irstream) => {
                for expr in &irstream.stream {
                    self.expression(expr, verts, last, &irstream.bit_spec)?;
                }
            }
            Expression::List(list) => {
                for expr in list {
                    self.expression(expr, verts, last, bit_spec)?;
                }
            }
            Expression::FlashConstant(v, u) => {
                let len = u.eval_float(*v, &self.general_spec)?;

                let pos = verts.len();

                verts.push(Vertex::Nop(vec![]));

                verts[*last].add_edge(Edge::Flash(len, pos));

                *last = pos;
            }
            Expression::GapConstant(v, u) => {
                let len = u.eval_float(*v, &self.general_spec)?;

                let pos = verts.len();

                verts.push(Vertex::Nop(vec![]));

                verts[*last].add_edge(Edge::Gap(len, pos));

                *last = pos;
            }
            Expression::BitField { length, .. } => {
                let start = *last;

                let (length, _) = length.eval(&Vartable::new())?;

                let end = verts.len();

                verts.push(Vertex::RepeatCounter(
                    length as u32,
                    vec![Edge::Repeat(start)],
                ));

                for (bit, e) in bit_spec.iter().enumerate() {
                    let mut n = start;

                    self.expression(e, verts, &mut n, bit_spec)?;

                    let bits_vert = verts.len();

                    verts.push(Vertex::Bits(bit as u8, vec![Edge::Empty(end)]));

                    verts[n].add_edge(Edge::Empty(bits_vert));
                }

                *last = end;
            }
            Expression::ExtentConstant(_, _) => {
                // should really check this is the last entry
            }
            _ => println!("expr:{:?}", expr),
        }

        Ok(())
    }
}

impl NFA {
    pub fn dotgraphviz(&self, dest: &Path) {
        let mut file = File::create(dest).expect("create file");

        writeln!(&mut file, "strict digraph NFA {{").unwrap();

        let mut vert_names = Vec::new();

        for (no, v) in self.verts.iter().enumerate() {
            let name = match v {
                Vertex::Nop(_) => format!("\"{} ({})\"", no_to_name(vert_names.len()), no),
                Vertex::Bits(v, _) => {
                    format!("\"bit {} ({})\"", v, no)
                }
                Vertex::Done => String::from("done"),
                Vertex::RepeatCounter(r, _) => format!("\"repeat {} ({})\"", r, no),
            };

            vert_names.push(name);
        }

        for (i, v) in self.verts.iter().enumerate() {
            for edge in v.get_edges() {
                match edge {
                    Edge::Flash(len, dest) => writeln!(
                        &mut file,
                        "\t{} -> {} [label=\"flash {}μs\"]",
                        vert_names[i], vert_names[*dest], len
                    )
                    .unwrap(),
                    Edge::Gap(len, dest) => writeln!(
                        &mut file,
                        "\t{} -> {} [label=\"gap {}μs\"]",
                        vert_names[i], vert_names[*dest], len
                    )
                    .unwrap(),
                    Edge::Repeat(dest) => writeln!(
                        &mut file,
                        "\t{} -> {} [label=repeat]",
                        vert_names[i], vert_names[*dest]
                    )
                    .unwrap(),
                    Edge::Empty(dest) => {
                        writeln!(&mut file, "\t{} -> {}", vert_names[i], vert_names[*dest]).unwrap()
                    }
                }
            }
        }

        writeln!(&mut file, "}}").unwrap();
    }

    pub fn matcher(&self, abs_tolerance: u32, rel_tolerance: u32) -> Matcher {
        Matcher {
            pos: Vec::new(),
            abs_tolerance,
            rel_tolerance,
            bits: 0,
            counter: 0,
            nfa: &self,
        }
    }
}

fn no_to_name(no: usize) -> String {
    let mut no = no;
    let mut res = String::new();

    loop {
        let ch = char::from_u32((65 + no % 26) as u32).unwrap();

        res.insert(0, ch);

        no /= 26;
        if no == 0 {
            return res;
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum InfraredData {
    Flash(u32),
    Gap(u32),
    Reset,
}

impl<'a> Matcher<'a> {
    pub fn reset(&mut self) {
        self.pos.truncate(0);
        self.bits = 0;
        self.counter = 0;
    }

    fn duration_matches(&self, expected: u32, received: u32) -> bool {
        let diff = if expected > received {
            expected - received
        } else {
            received - expected
        };

        if diff <= self.abs_tolerance {
            true
        } else {
            ((diff * 100) / expected) <= self.rel_tolerance
        }
    }

    pub fn input(&mut self, ir: InfraredData) -> Option<u64> {
        if ir == InfraredData::Reset {
            self.reset();
            return None;
        }

        if self.pos.is_empty() {
            self.pos.push(0);
        }

        let mut new_pos: HashSet<usize> = HashSet::new();

        let mut f = Vec::new();

        std::mem::swap(&mut f, &mut self.pos);

        for pos in f.into_iter() {
            let edges = self.nfa.verts[pos].get_edges();

            for edge in edges {
                match edge {
                    Edge::Flash(expected, dest) => {
                        if let InfraredData::Flash(received) = ir {
                            if self.duration_matches(*expected as u32, received) {
                                new_pos.insert(*dest);
                            }
                        }
                    }
                    Edge::Gap(expected, dest) => {
                        if let InfraredData::Gap(received) = ir {
                            if self.duration_matches(*expected as u32, received) {
                                new_pos.insert(*dest);
                            }
                        }
                    }
                    _ => (),
                }
            }
        }

        let mut changes;

        for mut pos in new_pos {
            changes = true;

            while changes {
                changes = false;
                match &self.nfa.verts[pos] {
                    Vertex::Done => {
                        let res = self.bits;
                        self.reset();
                        return Some(res);
                    }
                    Vertex::Bits(v, _) => {
                        self.bits <<= 1;
                        self.bits |= *v as u64;
                    }
                    Vertex::RepeatCounter(max, _) => {
                        self.counter += 1;
                        if self.counter < *max {
                            changes = true;
                            pos = self.nfa.verts[pos].get_repeat_edge();
                        }
                    }
                    _ => (),
                }

                // follow any empty edge
                if let Some(dest) = self.nfa.verts[pos].get_empty_edge() {
                    changes = true;
                    pos = dest;
                }
            }

            self.pos.push(pos);
        }

        None
    }
}

#[test]
fn sony8() {
    // sony 8
    let irp = Irp::parse("{40k,600}<1,-1|2,-1>(4,-1,F:8,^45m)[F:0..255]").unwrap();

    let nfa = irp.decode().unwrap();

    nfa.dotgraphviz(&PathBuf::from("test.dot"));

    let mut matcher = nfa.matcher(100, 3);

    for ir in vec![
        InfraredData::Flash(2400),
        InfraredData::Gap(600),
        InfraredData::Flash(600),
        InfraredData::Gap(600),
        InfraredData::Flash(600),
        InfraredData::Gap(600),
        InfraredData::Flash(1200),
        InfraredData::Gap(600),
        InfraredData::Flash(600),
        InfraredData::Gap(600),
        InfraredData::Flash(600),
        InfraredData::Gap(600),
        InfraredData::Flash(600),
        InfraredData::Gap(600),
        InfraredData::Flash(1200),
        InfraredData::Gap(600),
        InfraredData::Flash(1200),
        InfraredData::Gap(600),
    ] {
        println!("ir:{:?}", ir);

        assert_eq!(matcher.input(ir), None);

        println!("matcher:{:?}", matcher);
    }

    panic!("nfa: {:?}", nfa);
}
