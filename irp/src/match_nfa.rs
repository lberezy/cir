use super::{
    build_nfa::{Action, Edge, NFA},
    Vartable,
};
use log::trace;
use std::{collections::HashMap, fmt, fmt::Write};

#[derive(Debug)]
pub struct Matcher<'a> {
    pos: Vec<(usize, Vartable<'a>)>,
    abs_tolerance: u32,
    rel_tolerance: u32,
    nfa: &'a NFA,
}

impl NFA {
    pub fn matcher(&self, abs_tolerance: u32, rel_tolerance: u32) -> Matcher {
        Matcher {
            pos: Vec::new(),
            abs_tolerance,
            rel_tolerance,
            nfa: self,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum InfraredData {
    Flash(u32),
    Gap(u32),
    Reset,
}

impl InfraredData {
    #[must_use]
    fn consume(&self, v: u32) -> Self {
        match self {
            InfraredData::Flash(dur) => InfraredData::Flash(*dur - v),
            InfraredData::Gap(dur) => InfraredData::Gap(*dur - v),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for InfraredData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfraredData::Flash(dur) => write!(f, "+{}", dur),
            InfraredData::Gap(dur) => write!(f, "-{}", dur),
            InfraredData::Reset => write!(f, "!"),
        }
    }
}

impl<'a> fmt::Display for Vartable<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for (name, (val, _, expr)) in &self.vars {
            if let Some(expr) = expr {
                write!(s, " {} = {}", name, expr).unwrap();
            } else {
                write!(s, " {} = {}", name, val).unwrap();
            }
        }

        write!(f, "{}", s)
    }
}

impl<'a> Matcher<'a> {
    pub fn reset(&mut self) {
        self.pos.truncate(0);
    }

    fn tolerance_eq(&self, expected: u32, received: u32) -> bool {
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

    pub fn input(&mut self, ir: InfraredData) -> Option<HashMap<String, i64>> {
        if ir == InfraredData::Reset {
            trace!("decoder reset");
            self.reset();
            return None;
        }

        if self.pos.is_empty() {
            self.pos.push((0, Vartable::new()));
        }

        let mut work = Vec::new();

        for (pos, vartab) in &self.pos {
            work.push((Some(ir), *pos, vartab.clone()));
        }

        let mut new_pos = Vec::new();

        while let Some((ir, pos, vartab)) = work.pop() {
            let edges = &self.nfa.verts[pos].edges;

            trace!("pos:{} ir:{:?} vars:{}", pos, ir, vartab);

            for edge in edges {
                //trace!(&format!("edge:{:?}", edge));

                match edge {
                    Edge::Flash(expected, dest) => {
                        if let Some(ir @ InfraredData::Flash(received)) = ir {
                            if self.tolerance_eq(*expected as u32, received) {
                                let (success, vartab) = self.run_actions(pos, &vartab);

                                trace!(
                                    "matched flash {} (expected {}) => {}",
                                    received,
                                    *expected,
                                    dest
                                );

                                if success {
                                    work.push((None, *dest, vartab));
                                }
                            } else if received > *expected as u32 {
                                let (success, vartab) = self.run_actions(pos, &vartab);

                                trace!(
                                    "matched flash {} (expected {}) (incomplete consume) => {}",
                                    received,
                                    *expected,
                                    dest
                                );

                                if success {
                                    work.push((Some(ir.consume(*expected as u32)), *dest, vartab));
                                }
                            }
                        } else if ir.is_none() && new_pos.iter().all(|(n, _)| *n != pos) {
                            new_pos.push((pos, vartab.clone()));
                        }
                    }
                    Edge::Gap(expected, dest) => {
                        if let Some(ir @ InfraredData::Gap(received)) = ir {
                            if self.tolerance_eq(*expected as u32, received) {
                                let (success, vartab) = self.run_actions(pos, &vartab);

                                trace!(
                                    "matched gap {} (expected {}) => {}",
                                    received,
                                    *expected,
                                    dest
                                );

                                if success {
                                    work.push((None, *dest, vartab));
                                }
                            } else if received > *expected as u32 {
                                let (success, vartab) = self.run_actions(pos, &vartab);

                                trace!(
                                    "matched gap {} (expected {}) (incomplete consume) => {}",
                                    received,
                                    *expected,
                                    dest
                                );

                                if success {
                                    work.push((Some(ir.consume(*expected as u32)), *dest, vartab));
                                }
                            }
                        } else if ir.is_none() && new_pos.iter().all(|(n, _)| *n != pos) {
                            new_pos.push((pos, vartab.clone()));
                        }
                    }
                    Edge::Branch(dest) => {
                        let (success, vartab) = self.run_actions(pos, &vartab);

                        if success {
                            work.push((ir, *dest, vartab));
                        }
                    }
                    Edge::BranchCond { expr, yes, no } => {
                        let (success, vartab) = self.run_actions(pos, &vartab);

                        if success {
                            let (cond, _) = expr.eval(&vartab).unwrap();

                            let dest = if cond != 0 { *yes } else { *no };

                            trace!(
                                "conditional branch {}: {}: destination {}",
                                expr,
                                cond != 0,
                                dest
                            );

                            work.push((ir, dest, vartab));
                        }
                    }
                    Edge::Done(include) => {
                        let (success, vartab) = self.run_actions(pos, &vartab);

                        if success {
                            let mut res: HashMap<String, i64> = HashMap::new();

                            for (name, (val, _, _)) in &vartab.vars {
                                if include.contains(name) {
                                    trace!("done");

                                    res.insert(name.to_owned(), *val);
                                }
                            }

                            return Some(res);
                        }
                    }
                }
            }
        }

        self.pos = new_pos;

        None
    }

    fn run_actions<'v>(&self, pos: usize, vartab: &Vartable<'v>) -> (bool, Vartable<'v>) {
        let mut vartable = vartab.clone();

        for a in &self.nfa.verts[pos].actions {
            match a {
                Action::Set { var, expr } => {
                    let (val, len) = expr.eval(&vartable).unwrap();
                    trace!("set {} = {} = {}", var, expr, val);
                    vartable.vars.insert(var.to_string(), (val, len, None));
                }
                Action::AssertEq { left, right } => {
                    let (left_val, _) = left.eval(&vartable).unwrap();
                    let (right_val, _) = right.eval(&vartable).unwrap();

                    if left_val != right_val {
                        trace!("assert FAIL {} != {}", left, right);
                        return (false, vartable);
                    }
                }
            }
        }

        (true, vartable)
    }

    /// Generate a GraphViz dot file and write to the given path
    pub fn dotgraphviz(&self, path: &str) {
        crate::graphviz::graphviz(self.nfa, &self.pos, path);
    }
}

#[cfg(test)]
mod test {
    use super::{InfraredData, Matcher};
    use crate::{rawir, Irp};
    use num::Integer;
    use std::collections::HashMap;

    fn munge(matcher: &mut Matcher, s: &str) -> HashMap<String, i64> {
        let mut res = None;

        for ir in rawir::parse(s)
            .unwrap()
            .iter()
            .enumerate()
            .map(|(no, len)| {
                if no.is_odd() {
                    InfraredData::Gap(*len)
                } else {
                    InfraredData::Flash(*len)
                }
            })
        {
            if let Some(r) = matcher.input(ir) {
                if res.is_some() {
                    panic!("double result: {:?} and {:?}", res, r);
                }

                res = Some(r);
            }
        }

        res.unwrap()
    }

    #[test]
    fn sony8() {
        // sony 8
        let irp = Irp::parse("{40k,600}<1,-1|2,-1>(4,-1,F:8,^45m)[F:0..255]").unwrap();

        let nfa = irp.build_nfa().unwrap();

        let mut matcher = nfa.matcher(100, 3);

        let res = munge(&mut matcher,
            "+2400 -600 +600 -600 +600 -600 +1200 -600 +600 -600 +600 -600 +600 -600 +1200 -600 +1200 -31200");

        assert_eq!(res["F"], 196);
    }

    #[test]
    fn nec() {
        let irp = Irp::parse("{38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m,(16,-4,1,^108m)*) [D:0..255,S:0..255=255-D,F:0..255]").unwrap();

        let nfa = irp.build_nfa().unwrap();

        let mut matcher = nfa.matcher(100, 3);

        // let res = munge(&mut matcher, "+9024 -2256 +564 -96156");

        // assert_eq!(res, Some(196));

        // matcher.reset();

        let res = munge(&mut matcher,
            "+9024 -4512 +564 -564 +564 -564 +564 -564 +564 -564 +564 -564 +564 -564 +564 -1692 +564 -564 +564 -1692 +564 -1692 +564 -1692 +564 -1692 +564 -1692 +564 -1692 +564 -564 +564 -1692 +564 -564 +564 -564 +564 -1692 +564 -564 +564 -564 +564 -564 +564 -1692 +564 -1692 +564 -1692 +564 -1692 +564 -564 +564 -1692 +564 -1692 +564 -1692 +564 -564 +564 -564 +564 -39756");

        // not quite
        assert_eq!(res["F"], 191);
        assert_eq!(res["D"], 59);
        assert_eq!(res["S"], 196);
    }

    #[test]
    fn rc5() {
        // RC5
        let irp = Irp::parse("{36k,msb,889}<1,-1|-1,1>((1,~F:1:6,T:1,D:5,F:6,^114m)*,T=1-T)[D:0..31,F:0..127,T@:0..1=0]").unwrap();

        let nfa = irp.build_nfa().unwrap();

        let mut matcher = nfa.matcher(100, 3);

        let  res = munge(&mut matcher,
            "+889 -889 +1778 -1778 +889 -889 +889 -889 +889 -889 +1778 -889 +889 -889 +889 -889 +889 -889 +889 -889 +889 -1778 +889 -89997");

        assert_eq!(res["F"], 1);
        assert_eq!(res["D"], 30);
        assert_eq!(res["T"], 0);
    }
}
