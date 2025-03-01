use super::{Flags, Remote};
use std::fmt::Write;

impl Remote {
    /// Build an IRP representation for the remote. This can be used both for encoding
    /// and decoding.
    pub fn irp(&self) -> String {
        let mut irp = String::from("{");

        if self.frequency != 0 {
            write!(irp, "{}k,", self.frequency as f64 / 1000f64).unwrap();
        }

        if self.duty_cycle != 0 {
            write!(irp, "{}%,", self.duty_cycle).unwrap();
        }

        irp.push_str("msb}<");

        if self.flags.contains(Flags::BO) {
            write!(
                irp,
                "{},-zeroGap,zeroGap={},oneGap={}|{},-oneGap,zeroGap={},oneGap={}|",
                self.bit[1].0,
                self.bit[2].1,
                self.bit[3].1,
                self.bit[2].0,
                self.bit[1].1,
                self.bit[2].1,
            )
            .unwrap();
        } else if self.flags.contains(Flags::GRUNDIG) {
            write!(
                irp,
                "-{},{}|\
                -{},{},-{},{}|\
                -{},{},-{},{}|\
                -{},{},-{},{}|",
                // bit 0
                self.bit[3].1,
                self.bit[3].0,
                // bit 1
                self.bit[2].1,
                self.bit[2].0,
                self.bit[0].1,
                self.bit[0].0,
                // bit 2
                self.bit[1].1,
                self.bit[1].0,
                self.bit[1].1,
                self.bit[1].0,
                // bit 3
                self.bit[0].1,
                self.bit[0].0,
                self.bit[2].1,
                self.bit[2].0,
            )
            .unwrap();
        } else if self.flags.contains(Flags::XMP) {
            for i in 0..16 {
                write!(
                    irp,
                    "{},-{}|",
                    self.bit[0].0,
                    self.bit[0].1 + i * self.bit[1].1
                )
                .unwrap();
            }
        } else {
            for (bit_no, (pulse, space)) in self.bit.iter().enumerate() {
                if *pulse == 0 && *space == 0 {
                    break;
                }

                if (self.flags.intersects(Flags::RC5 | Flags::RC6) && bit_no == 1)
                    || self.flags.contains(Flags::SPACE_FIRST)
                {
                    if *space > 0 {
                        write!(irp, "-{},", space).unwrap();
                    }

                    if *pulse > 0 {
                        write!(irp, "{},", pulse).unwrap();
                    }
                } else {
                    if *pulse > 0 {
                        write!(irp, "{},", pulse).unwrap();
                    }

                    if *space > 0 {
                        write!(irp, "-{},", space).unwrap();
                    }
                }

                irp.pop();
                irp.push('|');
            }
        }

        irp.pop();
        irp.push_str(">(");

        add_irp_body(self, &mut irp, false);

        if self.repeat.0 != 0 && self.repeat.1 != 0 {
            irp.push('(');
            if self.flags.contains(Flags::REPEAT_HEADER) && self.header.0 != 0 && self.header.1 != 0
            {
                write!(irp, "{},-{},", self.header.0, self.header.1).unwrap();
            }
            if self.plead != 0 {
                write!(irp, "{},", self.plead).unwrap();
            }
            write!(irp, "{},-{},", self.repeat.0, self.repeat.1).unwrap();
            if self.ptrail != 0 {
                write!(irp, "{},", self.ptrail).unwrap();
            }
            add_gap(self, &mut irp, true);

            irp.pop();
            match self.min_repeat {
                0 => irp.push_str(")*)"),
                1 => irp.push_str(")+)"),
                _ => write!(irp, "){}+)", self.min_repeat).unwrap(),
            }
        } else if self
            .flags
            .intersects(Flags::NO_HEAD_REP | Flags::NO_FOOT_REP)
        {
            irp.push('(');

            add_irp_body(self, &mut irp, true);

            irp.pop();
            match self.min_repeat {
                0 => irp.push_str(")*)"),
                1 => irp.push_str(")+)"),
                _ => write!(irp, "){}+)", self.min_repeat).unwrap(),
            }
        } else {
            irp.pop();
            if self.min_repeat > 0 {
                write!(irp, "){}+", self.min_repeat + 1).unwrap();
            } else {
                irp.push_str(")+");
            }
        }

        if toggle_post_data(self) || self.flags.contains(Flags::BO) {
            irp.push('{');

            if toggle_post_data(self) {
                write!(irp, "POST=0x{:x},", self.post_data).unwrap();
            }

            if self.flags.contains(Flags::BO) {
                write!(irp, "zeroGap={},oneGap={},", self.bit[1].1, self.bit[3].1).unwrap();
            }

            irp.pop();
            irp.push('}');
        }

        write!(irp, " [CODE:0..{}", gen_mask(self.bits)).unwrap();

        if self.toggle_bit_mask.count_ones() == 1 {
            irp.push_str(",T@:0..1=0");
        }

        irp.push(']');

        irp
    }

    /// How many bits are there in the definition
    pub fn all_bits(&self) -> u64 {
        self.pre_data_bits + self.bits + self.post_data_bits
    }
}

fn add_irp_body(remote: &Remote, irp: &mut String, repeat: bool) {
    let supress_header = repeat && remote.flags.contains(Flags::NO_HEAD_REP);
    let supress_footer = repeat && remote.flags.contains(Flags::NO_FOOT_REP);

    if remote.flags.contains(Flags::BO) {
        write!(
            irp,
            "{},-{},{},-{},",
            remote.bit[1].0, remote.bit[1].1, remote.bit[1].0, remote.bit[1].1
        )
        .unwrap();
    }

    if !supress_header && remote.header.0 != 0 && remote.header.1 != 0 {
        write!(irp, "{},-{},", remote.header.0, remote.header.1).unwrap();
    }

    if remote.plead != 0 {
        write!(irp, "{},", remote.plead).unwrap();
    }

    let toggle_bit_mask = if remote.toggle_bit_mask.count_ones() == 1 {
        remote.toggle_bit_mask
    } else {
        0
    };

    if remote.pre_data_bits != 0 {
        add_bit_stream(
            remote,
            Stream::Constant(remote.pre_data),
            remote.pre_data_bits,
            toggle_bit_mask >> (remote.bits + remote.post_data_bits),
            remote.rc6_mask >> (remote.bits + remote.post_data_bits),
            irp,
        );

        if remote.pre.0 != 0 && remote.pre.1 != 0 {
            write!(irp, "{},-{},", remote.pre.0, remote.pre.1).unwrap();
        }
    }

    add_bit_stream(
        remote,
        Stream::Variable("CODE"),
        remote.bits,
        toggle_bit_mask >> remote.post_data_bits,
        remote.rc6_mask >> remote.post_data_bits,
        irp,
    );

    if remote.post_data_bits != 0 {
        let stream = if toggle_post_data(remote) {
            Stream::Variable("POST")
        } else {
            Stream::Constant(remote.post_data)
        };

        add_bit_stream(
            remote,
            stream,
            remote.post_data_bits,
            toggle_bit_mask,
            remote.rc6_mask,
            irp,
        );

        if remote.post.0 != 0 && remote.post.1 != 0 {
            write!(irp, "{},-{},", remote.post.0, remote.post.1).unwrap();
        }
    }

    if !supress_footer && remote.foot.0 != 0 && remote.foot.1 != 0 {
        write!(irp, "{},-{},", remote.foot.0, remote.foot.1).unwrap();
    }

    if remote.ptrail != 0 {
        write!(irp, "{},", remote.ptrail).unwrap();
    }

    add_gap(remote, irp, repeat);

    if remote.toggle_mask != 0 {
        write!(
            irp,
            "CODE=CODE^0x{:x},",
            remote.toggle_mask >> remote.post_data_bits
        )
        .unwrap();
    }

    if remote.toggle_bit_mask.count_ones() > 1 {
        write!(
            irp,
            "CODE=CODE^0x{:x},",
            remote.toggle_bit_mask >> remote.post_data_bits
        )
        .unwrap();
    }

    if toggle_post_data(remote) {
        write!(
            irp,
            "POST=POST^0x{:x},",
            remote.toggle_mask & gen_mask(remote.post_data_bits)
        )
        .unwrap();
    }
}

fn add_gap(remote: &Remote, irp: &mut String, repeat: bool) {
    if remote.gap != 0 || (remote.repeat_gap != 0 && repeat) {
        let gap = if repeat && remote.repeat_gap != 0 {
            remote.repeat_gap
        } else if !repeat
            && remote
                .flags
                .contains(Flags::NO_HEAD_REP | Flags::CONST_LENGTH)
        {
            remote.gap + remote.header.0 + remote.header.1
        } else {
            remote.gap
        };

        irp.push(if remote.flags.contains(Flags::CONST_LENGTH) {
            '^'
        } else {
            '-'
        });

        if remote.gap % 1000 == 0 {
            write!(irp, "{}m,", gap / 1000).unwrap();
        } else {
            write!(irp, "{},", gap).unwrap();
        }
    }
}

fn toggle_post_data(remote: &Remote) -> bool {
    remote.toggle_mask != 0 && (remote.toggle_mask & gen_mask(remote.post_data_bits)) != 0
}

#[derive(Clone, Copy)]
enum Stream<'a> {
    Constant(u64),
    Variable(&'a str),
    Toggle,
}

fn add_bit_stream(
    remote: &Remote,
    stream: Stream,
    bits: u64,
    toggle_mask: u64,
    rc6_mask: u64,
    irp: &mut String,
) {
    let mut edges = mask_edges(rc6_mask, bits);
    edges.extend_from_slice(&mask_edges(toggle_mask, bits));
    edges.sort_by(|a, b| b.partial_cmp(a).unwrap());
    edges.dedup();
    edges.push(0);

    let mut highest_bit = bits;

    for bit in edges {
        let is_toggle = (toggle_mask & (1 << bit)) != 0;
        let is_rc6 = (rc6_mask & (1 << bit)) != 0;

        if is_rc6 {
            write!(
                irp,
                "<{},-{}|-{},{}>(",
                remote.bit[0].0 * 2,
                remote.bit[0].1 * 2,
                remote.bit[1].1 * 2,
                remote.bit[1].0 * 2
            )
            .unwrap();
        }

        let stream = if is_toggle { Stream::Toggle } else { stream };

        let bit_count = highest_bit - bit;
        let offset = bit;

        match stream {
            Stream::Constant(v) => {
                let v = (v >> offset) & gen_mask(bit_count);

                if v <= 9 {
                    write!(irp, "{}:{},", v, bit_count).unwrap();
                } else {
                    write!(irp, "0x{:x}:{},", v, bit_count).unwrap();
                }
            }
            Stream::Variable(v) if offset == 0 => {
                write!(irp, "{}:{},", v, bit_count).unwrap();
            }
            Stream::Variable(v) => {
                write!(irp, "{}:{}:{},", v, bit_count, offset).unwrap();
            }
            Stream::Toggle => {
                assert_eq!(bit_count, 1);

                irp.push_str("T:1,");
            }
        }

        if is_rc6 {
            irp.pop();
            irp.push_str("),");
        }

        highest_bit = bit;
    }
}

fn gen_mask(v: u64) -> u64 {
    (1u64 << v) - 1
}

fn highest_bit(v: u64) -> u64 {
    63u64 - v.leading_zeros() as u64
}

/// For given bitmask, produce an array of edges of bit numbers where the mask changes
fn mask_edges(mask: u64, bits: u64) -> Vec<u64> {
    let mut v = mask & gen_mask(bits);
    let mut edges = Vec::new();

    while v != 0 {
        let bit = highest_bit(v) + 1;

        edges.push(bit);

        v = !v & gen_mask(bit);
    }

    edges
}

#[test]
fn test_edges() {
    assert_eq!(mask_edges(0, 32), vec![]);
    assert_eq!(mask_edges(1, 32), vec![1]);
    assert_eq!(mask_edges(2, 32), vec![2, 1]);
    assert_eq!(mask_edges(8, 32), vec![4, 3]);
    assert_eq!(mask_edges(0xf0, 32), vec![8, 4]);
}
