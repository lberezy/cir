use super::build_nfa::{Action, Edge, NFA};
use super::Vartable;
use std::collections::HashMap;
#[allow(unused_imports)]
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Matcher<'a> {
    pos: Vec<(usize, Vartable<'a>)>,
    abs_tolerance: u32,
    rel_tolerance: u32,
    counter: u32,
    bits: u64,
    nfa: &'a NFA,
}

impl NFA {
    pub fn matcher(&self, abs_tolerance: u32, rel_tolerance: u32) -> Matcher {
        Matcher {
            pos: Vec::new(),
            abs_tolerance,
            rel_tolerance,
            bits: 0,
            counter: 0,
            nfa: self,
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
            self.pos.push((0, Vartable::new()));
        }

        let mut new_pos: HashMap<usize, Vartable> = HashMap::new();

        let mut f = Vec::new();

        std::mem::swap(&mut f, &mut self.pos);

        for (pos, vartab) in f.into_iter() {
            let edges = &self.nfa.verts[pos].edges;

            for edge in edges {
                match edge {
                    Edge::Flash(expected, dest) => {
                        if let InfraredData::Flash(received) = ir {
                            if self.duration_matches(*expected as u32, received) {
                                new_pos.insert(*dest, vartab.clone());
                            }
                        }
                    }
                    Edge::Gap(expected, dest) => {
                        if let InfraredData::Gap(received) = ir {
                            if self.duration_matches(*expected as u32, received) {
                                new_pos.insert(*dest, vartab.clone());
                            }
                        }
                    }
                    _ => (),
                }
            }
        }

        let mut changes;

        for (mut pos, mut vartable) in new_pos {
            changes = true;

            while changes {
                changes = false;
                for a in &self.nfa.verts[pos].actions {
                    match a {
                        Action::Set { var, expr } => {
                            let (val, len) = expr.eval(&vartable).unwrap();
                            vartable.vars.insert(var.to_string(), (val, len, None));
                            changes = true;
                        }
                    }
                }

                for edge in &self.nfa.verts[pos].edges {
                    match edge {
                        Edge::Branch(dest) => {
                            changes = true;
                            pos = *dest;
                        }
                        Edge::BranchCond { expr, yes, no } => {
                            let (cond, _) = expr.eval(&vartable).unwrap();

                            pos = if cond != 0 { *yes } else { *no };
                            changes = true;
                        }
                        Edge::Done => {
                            let (val, _) = Expression::Identifier(String::from("$bits"))
                                .eval(&vartable)
                                .unwrap();
                            self.reset();

                            return Some(val as u64);
                        }
                        _ => (),
                    }
                }
            }

            self.pos.push((pos, vartable));
        }

        None
    }
}

use crate::Expression;
#[cfg(test)]
use crate::Irp;

#[test]
fn sony8() {
    // sony 8
    let irp = Irp::parse("{40k,600}<1,-1|2,-1>(4,-1,F:8,^45m)[F:0..255]").unwrap();

    let nfa = irp.build_nfa().unwrap();

    nfa.dotgraphviz(&PathBuf::from("test.dot"));

    let mut matcher = nfa.matcher(100, 3);

    let mut res = None;

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
        if let Some(r) = matcher.input(ir) {
            if res.is_some() {
                panic!("double result: {:?} and {:?}", res, r);
            }

            res = Some(r);
        }
    }

    assert_eq!(res, Some(196));
}

#[test]
fn nec() {
    let irp = Irp::parse("{38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m,(16,-4,1,^108m)*) [D:0..255,S:0..255=255-D,F:0..255]").unwrap();

    let nfa = irp.build_nfa().unwrap();

    let _matcher = nfa.matcher(100, 3);
}
