use crate::ir::*;

pub fn parse_ir(s: &str) -> IR {
    let tokens = tokenize(s);

    let mut ir = IR {
        fns: Vec::new(),
        main_fn: FnId::MAX,
    };

    let mut tokens_ref = &tokens[..];
    while !tokens_ref.is_empty() {
        tokens_ref = parse_fn(tokens_ref, &mut ir);
    }

    if ir.main_fn == FnId::MAX {
        panic!("no main fn specified!");
    }

    ir
}

fn parse_fn<'tok, 'tokv>(mut tokens: &'tokv [Token<'tok>], ir: &mut IR) -> &'tokv [Token<'tok>] {
    let mut is_main = false;
    if tokens[0] == Token::Word("main") {
        is_main = true;
        tokens = &tokens[1..];
    }

    let [Token::Word("function"), Token::Word(fname), Token::Sign('('), Token::Sign(')'), Token::Sign(':'), ..] = tokens else { panic!(); };
    tokens = &tokens[5..];

    let fid = ir.fns.len();
    let correct_fname = format!("f{}", fid);
    assert!(fname == &correct_fname);

    if is_main {
        ir.main_fn = fid;
    }

    let litf = LitFunction {
        blocks: Vec::new(),
        start_block: BlockId::MAX,
    };
    ir.fns.push(litf);

    let mut nxt_block_start = false;
    loop {
        match tokens {
            [Token::Word("start"), ..] => {
                nxt_block_start = true;
                tokens = &tokens[1..];
            },
            [Token::Word("block"), Token::Word(bname), Token::Sign(':'), ..] => {
                let bid = ir.fns[fid].blocks.len();
                let correct_bname = format!("b{}", bid);
                assert!(bname == &correct_bname);
                if nxt_block_start {
                    ir.fns[fid].start_block = bid;
                    nxt_block_start = false;
                }
                ir.fns[fid].blocks.push(Vec::new());
                tokens = &tokens[3..];
            },
            [Token::Word("end"), ..] => {
                tokens = &tokens[1..];
                break;
            },
            _ /*some stmt*/ => {
                let (newtok, stmt) = parse_stmt(tokens);
                let bid = ir.fns[fid].blocks.len() - 1;
                ir.fns[fid].blocks[bid].push(stmt);

                assert!(newtok[0] == Token::Sign(';'));
                tokens = &newtok[1..];
            },
        }
    }

    if ir.fns[fid].start_block == BlockId::MAX { panic!(); }

    tokens
}

fn parse_stmt<'tok, 'tokv>(tokens: &'tokv [Token<'tok>]) -> (&'tokv [Token<'tok>], Statement) {
    match tokens {
        [Token::Word(node), Token::Sign('='), ..] => {
            let node = extract_id(node, 'n');
            let (newtok, expr) = parse_expr(&tokens[2..]);
            let st = Statement::Compute(node, expr);
            return (newtok, st);
        },
        [Token::Word(t), Token::Sign('['), Token::Word(i), Token::Sign(']'), Token::Sign('<'), Token::Sign('-'), Token::Word(v), ..] => {
            let t = extract_id(t, 'n');
            let i = extract_id(i, 'n');
            let v = extract_id(v, 'n');
            let st = Statement::Store(t, i, v);
            return (&tokens[7..], st);
        },
        [Token::Word("if"), Token::Word(n), Token::Word("then"), Token::Word(b1), Token::Word("else"), Token::Word(b2), ..] => {
            let n = extract_id(n, 'n');
            let b1 = extract_id(b1, 'b');
            let b2 = extract_id(b2, 'b');
            let st = Statement::If(n, b1, b2);
            return (&tokens[6..], st);
        },
        [Token::Word("throw"), Token::Sign('('), Token::Str(msg), Token::Sign(')'), ..] => {
            let st = Statement::Command(Command::Throw(msg.to_string()));
            return (&tokens[4..], st);
        },
        [Token::Word("print"), Token::Sign('('), Token::Word(n), Token::Sign(')'), ..] => {
            let n = extract_id(n, 'n');
            let st = Statement::Command(Command::Print(n));
            return (&tokens[4..], st);
        },
        [Token::Word(f), Token::Sign('('), Token::Word(arg), Token::Sign(')'), ..] => {
            let f = extract_id(f, 'n');
            let arg = extract_id(arg, 'n');
            let st = Statement::FnCall(f, arg);
            return (&tokens[4..], st);
        },
        [Token::Word("return"), ..] => {
            let st = Statement::Return;
            return (&tokens[1..], st);
        },
        rest => panic!("{:?}", rest),
    }
}

fn parse_expr<'tok, 'tokv>(tokens: &'tokv [Token<'tok>]) -> (&'tokv [Token<'tok>], Expr) {
    match tokens {
        [Token::Word(t), Token::Sign('['), Token::Word(i), Token::Sign(']'), ..] => {
            let t = extract_id(t, 'n');
            let i = extract_id(i, 'n');
            let expr = Expr::Index(t, i);
            return (&tokens[4..], expr);
        },
        [Token::Word("arg"), ..] => {
            let expr = Expr::Arg;
            return (&tokens[1..], expr);
        },
        [Token::Sign('{'), Token::Sign('}'), ..] => {
            let expr = Expr::NewTable;
            return (&tokens[2..], expr);
        },

        [Token::Sign('#'), Token::Word(n), ..] => {
            let n = extract_id(n, 'n');
            let expr = Expr::Len(n);
            return (&tokens[2..], expr);
        },

        [Token::Word("type"), Token::Sign('('), Token::Word(n), Token::Sign(')'), ..] => {
            let n = extract_id(n, 'n');
            let expr = Expr::Intrinsic(Intrinsic::Type(n));
            return (&tokens[4..], expr);
        },

        [Token::Word("next"), Token::Sign('('), Token::Word(l), Token::Sign(','), Token::Word(r), Token::Sign(')'), ..] => {
            let l = extract_id(l, 'n');
            let r = extract_id(r, 'n');
            let expr = Expr::Intrinsic(Intrinsic::Next(l, r));
            return (&tokens[6..], expr);
        },

        [Token::Number(num), ..] => {
            let expr = Expr::Num(*num);
            return (&tokens[1..], expr);
        },
        [Token::Word("true"), ..] => {
            let expr = Expr::Bool(true);
            return (&tokens[1..], expr);
        },
        [Token::Word("false"), ..] => {
            let expr = Expr::Bool(false);
            return (&tokens[1..], expr);
        },
        [Token::Word("nil"), ..] => {
            let expr = Expr::Nil;
            return (&tokens[1..], expr);
        },
        [Token::Str(s), ..] => {
            let expr = Expr::Str(s.to_string());
            return (&tokens[1..], expr);
        },

        [Token::Word(f), ..] if f.starts_with("f") => {
            let f = extract_id(f, 'f');
            let expr = Expr::LitFunction(f);
            return (&tokens[1..], expr);
        },

        // binops 1
        [Token::Word(l), Token::Sign(op), Token::Word(r), ..] => {
            let op = match op {
                '+' => BinOpKind::Plus,
                '-' => BinOpKind::Minus,
                '*' => BinOpKind::Mul,
                '/' => BinOpKind::Div,
                '%' => BinOpKind::Mod,
                '^' => BinOpKind::Pow,
                '<' => BinOpKind::Lt,
                '>' => BinOpKind::Gt,
                _ => panic!(),
            };
            let l = extract_id(l, 'n');
            let r = extract_id(r, 'n');

            let expr = Expr::BinOp(op, l, r);
            return (&tokens[3..], expr);
        },

        // binops 2
        [Token::Word(l), Token::Sign(op1), Token::Sign(op2), Token::Word(r), ..] => {
            let op = match (op1, op2) {
                ('<', '=') => BinOpKind::Le,
                ('>', '=') => BinOpKind::Ge,
                ('=', '=') => BinOpKind::IsEqual,
                ('~', '=') => BinOpKind::IsNotEqual,
                ('.', '.') => BinOpKind::Concat,
                _ => panic!(),
            };
            let l = extract_id(l, 'n');
            let r = extract_id(r, 'n');

            let expr = Expr::BinOp(op, l, r);
            return (&tokens[4..], expr);
        },
        _ => panic!(),
    }
}

#[derive(Debug, PartialEq)]
enum Token<'s> {
    // something that starts with letters, and might contain numbers.
    Word(&'s str),
    Str(&'s str),
    Number(f64),
    Sign(char),
}

fn tokenize<'s>(mut s: &'s str) -> Vec<Token<'s>> {
    let mut tokens = Vec::new();

    loop {
        s = s.trim();
        if s.is_empty() { break; }

        let c = s.chars().next().unwrap();
        if c.is_alphabetic() {
            let mut i = 0;
            for ch in s.chars() {
                if ch.is_alphanumeric() { i += 1; }
                else { break; }
            }
            tokens.push(Token::Word(&s[..i].trim()));
            s = &s[i..];
        } else if c == '"' || c == '\'' {
            let i = s[1..].chars().position(|x| x == c).unwrap() + 1;
            tokens.push(Token::Str(&s[1..i]));
            s = &s[i+1..];
        } else if c.is_numeric() {
            let mut i = 0;
            for ch in s.chars() {
                if ch.is_numeric() || ch == '.' { i += 1; }
                else { break; }
            }
            let substr = &s[..i];
            let num = substr.parse::<f64>().unwrap();
            tokens.push(Token::Number(num));
            s = &s[i..];
        } else {
            tokens.push(Token::Sign(c));
            s = &s[1..];
        }
    }

    tokens
}

fn extract_id(s: &str, c: char) -> usize {
    assert!(c == s.chars().next().unwrap());
    let id = s[1..].parse::<usize>().unwrap();

    id
}
