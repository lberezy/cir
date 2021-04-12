use super::parse;
use super::render::{render, Vartable};
use crate::protocols::read_protocols;
use crate::rawir;

#[test]
fn test() {
    let mut vars = Vartable::new();

    vars.set("F".to_string(), 1, 8);
    vars.set("D".to_string(), 0xe9, 8);
    vars.set("S".to_string(), 0xfe, 8);

    let res = render(
        "{38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m)* [D:0..255,S:0..255=255-D,F:0..255]",
        vars, 1,
    );

    // irptransmogrifier.sh  --irp "{38.0k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m)+" render -r -n F=1,D=0xe9,S=0xfe
    assert_eq!(
        res,
        Ok(rawir::parse("+9024,-4512,+564,-1692,+564,-564,+564,-564,+564,-1692,+564,-564,+564,-1692,+564,-1692,+564,-1692,+564,-564,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-35244").unwrap())
    );

    let mut vars = Vartable::new();

    vars.set("F".to_string(), 1, 8);
    vars.set("D".to_string(), 0xe9, 8);
    vars.set("T".to_string(), 0, 8);

    let res = render(
        "{36k,msb,889}<1,-1|-1,1>(1:1,~F:1:6,T:1,D:5,F:6,^114m)+",
        vars,
        0,
    );

    // irptransmogrifier.sh  --irp "{36k,msb,889}<1,-1|-1,1>(1:1,~F:1:6,T:1,D:5,F:6,^114m)+" render -r -n F=1,T=0,D=0xe9

    assert_eq!(
        res,
        Ok(rawir::parse("+889,-889,+1778,-889,+889,-1778,+1778,-889,+889,-1778,+1778,-889,+889,-889,+889,-889,+889,-889,+889,-1778,+889,-89108").unwrap())
    );

    let mut vars = Vartable::new();

    vars.set("F".to_string(), 1, 8);
    vars.set("D".to_string(), 0xe9, 8);
    vars.set("S".to_string(), 0x88, 8);

    let res = render(
        "{38k,400}<1,-1|1,-3>(8,-4,170:8,90:8,15:4,D:4,S:8,F:8,E:4,C:4,1,-48)+ {E=1,C=D^S:4:0^S:4:4^F:4:0^F:4:4^E:4}",
        vars, 0
    );

    assert_eq!(
        res,
        Ok(rawir::parse("+3200,-1600,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-1200,+400,-400,+400,-1200,+400,-400,+400,-1200,+400,-1200,+400,-1200,+400,-1200,+400,-1200,+400,-400,+400,-400,+400,-1200,+400,-400,+400,-400,+400,-400,+400,-1200,+400,-400,+400,-400,+400,-400,+400,-1200,+400,-1200,+400,-400,+400,-400,+400,-400,+400,-400,+400,-400,+400,-400,+400,-400,+400,-1200,+400,-400,+400,-400,+400,-400,+400,-1200,+400,-400,+400,-400,+400,-1200,+400,-19200").unwrap())
    );
}

#[test]
fn rs200() {
    let irp = "{35.7k,msb}<50p,-120p|21p,-120p>(25:6,(H4-1):2,(H3-1):2,(H2-1):2,(H1-1):2,P:1,(D-1):3,F:2,0:2,sum:4,-1160p)*{   P=~(#(D-1)+#F):1,sum=9+((H4-1)*4+(H3-1)) + ((H2-1)*4+(H1-1)) + (P*8+(D-1)) + F*4}";

    let mut vars = Vartable::new();

    vars.set("D".to_string(), 4, 8);
    vars.set("F".to_string(), 1, 8);
    vars.set("H1".to_string(), 4, 8);
    vars.set("H2".to_string(), 2, 8);
    vars.set("H3".to_string(), 3, 8);
    vars.set("H4".to_string(), 4, 8);

    let res = render(irp, vars, 1);

    assert!(compare_with_rounding(
        res,
        Ok(rawir::parse("+1401,-3361,+588,-3361,+588,-3361,+1401,-3361,+1401,-3361,+588,-3361,+588,-3361,+588,-3361,+588,-3361,+1401,-3361,+1401,-3361,+588,-3361,+588,-3361,+588,-3361,+1401,-3361,+1401,-3361,+588,-3361,+588,-3361,+1401,-3361,+588,-3361,+1401,-3361,+1401,-3361,+1401,-3361,+588,-3361,+1401,-3361,+588,-35854").unwrap())
    ));
}

#[test]
fn nec() {
    let mut vars = Vartable::new();

    vars.set("F".to_string(), 1, 8);
    vars.set("D".to_string(), 0xe9, 8);

    let res = render(
        "{38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m)* [D:0..255,S:0..255=255-D,F:0..255]",
        vars,1
    );

    // irptransmogrifier.sh --irp  "{38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,^108m)* [D:0..255,S:0..255=255-D,F:0..255]" render -r -n F=1,D=0xe9
    assert_eq!(
        res,
        Ok(rawir::parse("+9024,-4512,+564,-1692,+564,-564,+564,-564,+564,-1692,+564,-564,+564,-1692,+564,-1692,+564,-1692,+564,-564,+564,-1692,+564,-1692,+564,-564,+564,-1692,+564,-564,+564,-564,+564,-564,+564,-1692,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-564,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-1692,+564,-39756").unwrap())
    );
}

#[test]
fn keeprite_ac() {
    let mut vars = Vartable::new();

    vars.set("A".to_string(), 1, 32);
    vars.set("B".to_string(), 0xe9, 32);

    let res = render("{38.1k,570,msb}<1,-1|1,-3>(16,-8,A:35,1,-20m,B:32,1,-20m)[A:0..0x7FFFFFFFF, B:0..UINT32_MAX]", vars, 1);

    // irptransmogrifier.sh --irp  "{38.1k,570,msb}<1,-1|1,-3>(16,-8,A:35,1,-20m,B:32,1,-20m)[A:0..0x7FFFFFFFF, B:0..UINT32_MAX]" render -r -n A=1,B=0xe9
    assert_eq!(
        res,
        Ok(rawir::parse("+9120,-4560,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-1710,+570,-20000,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-570,+570,-1710,+570,-1710,+570,-1710,+570,-570,+570,-1710,+570,-570,+570,-570,+570,-1710,+570,-20000").unwrap())
    );
}

#[test]
fn vars() {
    let mut vars = Vartable::new();

    vars.set("S".to_string(), 2, 8);
    vars.set("F".to_string(), 0xe9, 8);

    let res = render(
        "{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=8}[S:0..1,F:1..255]",
        vars,
        1,
    );

    assert_eq!(
        res,
        Err(String::from(
            "2 is more than maximum value 1 for parameter S"
        ))
    );

    let mut vars = Vartable::new();

    vars.set("S".to_string(), 1, 8);
    vars.set("F".to_string(), 0, 8);

    let res = render(
        "{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=8}[S:0..1,F:1..255]",
        vars,
        1,
    );

    assert_eq!(
        res,
        Err(String::from(
            "0 is less than minimum value 1 for parameter F"
        ))
    );

    let mut vars = Vartable::new();

    vars.set("S".to_string(), 1, 8);
    vars.set("X".to_string(), 0, 8);

    let res = render(
        "{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=8}[S:0..1,F:1..255]",
        vars,
        1,
    );

    assert_eq!(res, Err(String::from("missing value for F")));

    let mut vars = Vartable::new();

    vars.set("S".to_string(), 1, 8);
    vars.set("F".to_string(), 2, 8);
    vars.set("X".to_string(), 0, 8);

    let res = render(
        "{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=8}[S:0..1,F:1..255]",
        vars,
        1,
    );

    assert_eq!(res, Err(String::from("no parameter called X")));

    let mut vars = Vartable::new();

    vars.set("S".to_string(), 1, 8);
    vars.set("F".to_string(), 2, 8);
    vars.set("X".to_string(), 0, 8);

    let res = render(
        "{40k,520,msb}<1,-10|1,-1,1,-8>(S:1,<1:2|2:2>(F:D),-90m)*{D=8}",
        vars,
        1,
    );

    assert!(res.is_ok());
}

#[test]
fn parse_all_of_them() {
    let protocols = read_protocols();

    let mut broken = 0;
    let mut total = 0;
    for p in &protocols {
        total += 1;
        if let Err(s) = parse(&p.irp) {
            broken += 1;
            println!("{}: {}: {}", p.name, p.irp, s);
        }
    }

    if broken != 0 {
        panic!("{} out of {} broken", broken, total);
    }
}

fn compare_with_rounding(l: Result<Vec<u32>, String>, r: Result<Vec<u32>, String>) -> bool {
    if l == r {
        return true;
    }

    match (l, r) {
        (Ok(l), Ok(r)) => {
            if l.len() != r.len() {
                println!(
                    "comparing:\n{:?} with\n{:?}\n have different lengths {} and {}",
                    l,
                    r,
                    l.len(),
                    r.len()
                );

                return false;
            }

            for i in 0..l.len() {
                let diff = if l[i] > r[i] {
                    l[i] - r[i]
                } else {
                    r[i] - l[i]
                };
                if diff > 1 {
                    println!(
                        "comparing:\nleft:{:?} with\nright:{:?}\nfailed at position {} out of {}",
                        l,
                        r,
                        i,
                        l.len()
                    );

                    return false;
                }
            }

            true
        }
        _ => false,
    }
}
